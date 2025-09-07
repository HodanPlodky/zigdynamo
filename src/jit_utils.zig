const bc_interpret = @import("bc_interpreter.zig");
const bytecode = @import("bytecode.zig");
const runtime = @import("runtime.zig");
const std = @import("std");

pub const GPR64 = enum(u4) {
    rax = 0,
    rbx = 3,
    rcx = 1,
    rdx = 2,
    rsi = 6,
    rdi = 7,
    rsp = 4,
    rbp = 5,

    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
};

pub const Scale = enum(u2) {
    scale1 = 0b00,
    scale2 = 0b01,
    scale4 = 0b10,
    scale8 = 0b11,

    pub fn from_size(size: usize) Scale {
        return switch (size) {
            0 => Scale.scale1,
            2 => Scale.scale2,
            4 => Scale.scale4,
            8 => Scale.scale8,
            else => @panic("invalid scale"),
        };
    }
};

/// struct representing state of interpreter
/// for jit compiled code this is necessary because
/// normal structs in zig do not gurantee order of
/// fields and jit code needs different set of fields
/// then the bytecode interpreter
pub const JitState = extern struct {
    intepreter: *const bc_interpret.JitInterpreter,
    stack: *const bc_interpret.Stack,
    env: *const bc_interpret.Environment,
    gc: *const bc_interpret.GC,

    // basic
    alloc_stack: *const fn (*bc_interpret.Stack, usize) callconv(.C) void,

    // objects handle
    create_closure: *const fn (noalias *bc_interpret.JitInterpreter, u64, u64) callconv(.C) void,
    create_object: *const fn (noalias *bc_interpret.JitInterpreter, bytecode.ConstantIndex) callconv(.C) void,
    get_field: *const fn (noalias *bc_interpret.JitInterpreter, bytecode.ConstantIndex) callconv(.C) void,
    set_field: *const fn (noalias *bc_interpret.JitInterpreter, bytecode.ConstantIndex) callconv(.C) void,

    // calls
    call: *const fn (noalias *bc_interpret.JitInterpreter, noalias *const JitState) callconv(.C) void,
    method_call: *const fn (noalias *bc_interpret.JitInterpreter, noalias *const JitState, bytecode.ConstantIndex) callconv(.C) void,
    print: *const fn (noalias *bc_interpret.JitInterpreter, arg_count: u64) callconv(.C) void,

    // debug
    dbg: *const fn (runtime.Value) callconv(.C) void,
    dbg_raw: *const fn (u64) callconv(.C) void,
    dbg_inst: *const fn (u64) callconv(.C) void,

    // panics
    binop_panic: *const fn (runtime.Value, runtime.Value) callconv(.C) void,
    if_condition_panic: *const fn () callconv(.C) void,
    string_panic: *const fn () callconv(.C) void,

    pub fn get_offset(comptime field_name: []const u8) u32 {
        return @offsetOf(JitState, field_name);
    }
};

pub const JitFunction = struct {
    code: [*]u8,

    pub fn run(self: *const JitFunction, state: *const JitState) void {
        const f: *const fn (*const JitState) callconv(.C) void = @alignCast(@ptrCast(self.code));
        f(state);
    }
};

pub const JitError = error{
    CanOnlyCompileFn,
    UnsupportedBcInstruction,
    OutOfMem,
    HeuristicNotMet,
};

const PanicTable = struct {
    binop_panic: usize = 0,
    if_condition_panic: usize = 0,
    string_panic: usize = 0,
};

pub const Heuristic = struct {
    call_count: u4 = 5,
};

/// Base class for jit compilation that contains
/// code that is necessary for all of the jit
/// compilers
pub const JitCompilerBase = struct {
    code_slice: []u8,
    code_ptr: usize,
    panic_table: PanicTable,
    scratch_arena: std.heap.ArenaAllocator,
    heuristic: Heuristic,

    pub fn init(code_buffer_size: usize, heuristic: Heuristic) JitCompilerBase {
        const addr = std.os.linux.mmap(
            null,
            code_buffer_size,
            std.os.linux.PROT.READ | std.os.linux.PROT.WRITE,
            std.os.linux.MAP{
                .ANONYMOUS = true,
                .TYPE = std.os.linux.MAP_TYPE.PRIVATE,
            },
            -1,
            0,
        );

        const code: [*]u8 = @ptrFromInt(addr);
        const code_slice = code[0..code_buffer_size];

        var res = JitCompilerBase{
            .code_slice = code_slice,
            .code_ptr = 0,
            .panic_table = .{},
            .scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .heuristic = heuristic,
        };
        res.init_panic_handlers() catch unreachable;
        return res;
    }

    fn init_panic_handlers(self: *JitCompilerBase) !void {
        try self.emit_panic_handler_with_prolog("binop_panic", JitCompilerBase.bin_op_panic_prolog);
        try self.emit_panic_handler("if_condition_panic");
        try self.emit_panic_handler("string_panic");
    }

    fn bin_op_panic_prolog(self: *JitCompilerBase) !void {
        try self.mov_reg_reg(GPR64.rdi, GPR64.r8);
        try self.mov_reg_reg(GPR64.rsi, GPR64.r9);
    }

    fn emit_panic_handler(self: *JitCompilerBase, comptime panic_name: []const u8) !void {
        @field(self.panic_table, panic_name) = self.code_ptr;
        try self.call(panic_name);
    }

    fn emit_panic_handler_with_prolog(self: *JitCompilerBase, comptime panic_name: []const u8, comptime prolog: fn (*JitCompiler) JitError!void) !void {
        @field(self.panic_table, panic_name) = self.code_ptr;
        try prolog(self);
        try self.call(panic_name);
    }

    /// creates jump depending on current flags such that if the test
    /// return false then in jumps to panic
    fn emit_panic(self: *JitCompilerBase, comptime panic_name: []const u8) !void {
        const panic_offset = @field(self.panic_table, panic_name);
        std.debug.assert(self.code_ptr > panic_offset);
        var diff: usize = self.code_ptr + 2 - panic_offset;

        // we got signed so thats why size - 1
        if (diff < (1 << 7)) {
            const diff_u8: u8 = @intCast(diff);
            const jmp_val: u8 = ~diff_u8 + 1;
            const jmp_slice: [2]u8 = .{ 0x75, jmp_val };
            try self.emit_slice(jmp_slice[0..]);
            return;
        }
        diff += 4;
        if (diff < (1 << 31)) {
            // jne 401000 <tmp>
            // 0f 85 7e 8c ed ff
            const diff_u32: u32 = @intCast(diff);
            const jmp_val: u32 = ~diff_u32 + 1;
            const jmp_slice: [6]u8 = .{
                0x0f,
                0x85,
                @intCast(jmp_val & 0xff),
                @intCast((jmp_val >> 8) & 0xff),
                @intCast((jmp_val >> 16) & 0xff),
                @intCast((jmp_val >> 24) & 0xff),
            };

            try self.emit_slice(jmp_slice[0..]);
            return;
        }
        @panic("cannot do panic jump");
    }

    fn emit_slice(self: *JitCompilerBase, inst: []const u8) !void {
        if (inst.len > (self.code_slice.len - self.code_ptr)) {
            return JitError.OutOfMem;
        }
        @memcpy(self.code_slice[self.code_ptr..(self.code_ptr + inst.len)], inst);
        self.code_ptr += inst.len;
    }

    fn emit_byte(self: *JitCompilerBase, byte: u8) !void {
        const slice: [1]u8 = .{byte};
        try self.emit_slice(slice[0..]);
    }

    //
    // Regs movement
    //

    fn mov_reg_reg(self: *JitCompilerBase, dst: GPR64, src: GPR64) !void {
        // mov rbx, rdi
        // 48 89 fb
        // 48 = REX
        // 89 = opcode (mod r/m64, r)
        // fb = modrm 11_111_011 => mod | src | dst

        // REX | W | R | X | B
        // reg => R, r/m64 => B
        //const rex = 0x48 | ((src_val & 0x8) >> 1) | ((dst_val & 0x8) >> 3);
        const rex = JitCompilerBase.create_rex(src, dst);
        try self.emit_byte(rex);

        // opcode
        try self.emit_byte(0x89);

        // MODrm
        // mod = 0b11 | lower bits src | lower bits dest
        const modrm = JitCompilerBase.create_modrm_regs(src, dst);
        try self.emit_byte(modrm);
    }

    fn set_reg_64(self: *JitCompilerBase, reg: GPR64, value: u64) !void {
        const reg_val: u8 = @intFromEnum(reg);
        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we move immediate
        // and there is no other reg
        try self.emit_byte(0x48 | ((reg_val & 0x8) >> 3));

        // opcode has in it self the lower 3 bits of reg
        // index (0xb8 is base and you add those)
        const base_opcode: u8 = 0xb8;
        const opcode = base_opcode + (reg_val & 0x7);
        try self.emit_byte(opcode);

        const value_bytes: [8]u8 = .{
            @intCast(value & 0xff),
            @intCast((value >> 8) & 0xff),
            @intCast((value >> 16) & 0xff),
            @intCast((value >> 24) & 0xff),
            @intCast((value >> 32) & 0xff),
            @intCast((value >> 40) & 0xff),
            @intCast((value >> 48) & 0xff),
            @intCast((value >> 56) & 0xff),
        };

        try self.emit_slice(value_bytes[0..]);
    }

    fn deref_ptr(self: *JitCompilerBase, dst: GPR64, src: GPR64) !void {
        // mov rcx,QWORD PTR [rbx]
        // 48 8b 0b
        // 48 = REX
        // 8b = opcode
        // 0b = modrm

        const dest_value: u8 = @intFromEnum(dst);
        const src_value: u8 = @intFromEnum(src);

        // REX
        const rex = 0x48 | ((dest_value & 0x8) >> 1) | ((src_value & 0x8) >> 3);
        try self.emit_byte(rex);

        // opcode
        try self.emit_byte(0x8b);

        const modrm = ((dest_value & 0x7) << 3) | (src_value & 0x7);
        try self.emit_byte(modrm);
    }

    //
    // Other helpers
    //

    /// creates basic REX.W assuming only regs
    fn create_rex(reg: GPR64, rm64: GPR64) u8 {
        const reg_val: u8 = @intFromEnum(reg);
        const rm64_val: u8 = @intFromEnum(rm64);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The W is set
        // The R contains highest bit of reg number
        // The B contains highest bit of rm64 number
        return 0x48 | ((reg_val & 0x8) >> 1) | ((rm64_val & 0x8) >> 3);
    }

    /// creates basic MODrm assuming only regs
    fn create_modrm_regs(reg: GPR64, rm64: GPR64) u8 {
        const reg_val: u8 = @intFromEnum(reg);
        const rm64_val: u8 = @intFromEnum(rm64);

        // MODrm
        // mod = 0b11 = direct | lower bits reg | lower bits rm64
        return 0b11_000_000 | ((reg_val & 0x7) << 3) | (rm64_val & 0x7);
    }

    /// creates basic MODrm assuming only regs
    fn create_modrm_sib(reg: GPR64, offset: u32) u8 {
        const reg_val: u8 = @intFromEnum(reg);

        // ModRM
        // | mode | reg | r/m |
        // mode = 00/01/10 =>
        //      this depends on size of the
        //      offset 0 => 00, 1-255 => 01
        //      otherwise 10
        // reg = bottom 3 bits of to_reg_val
        // rm = 100 => the sib follows
        const mod: u8 = if (offset >= 256)
            0b1000_0000
        else if (offset != 0)
            0b0100_0000
        else
            0;

        const to_reg_lower: u8 = (reg_val & 0x7) << 3;
        const rm_sib: u8 = 0b100;
        return mod | to_reg_lower | rm_sib;
    }

    fn create_sib(scale: Scale, base: GPR64, index: GPR64) u8 {
        const base_val = @intFromEnum(base);
        var index_val = @intFromEnum(index);

        // SIB
        // | scale : 2b | index : 3b | base : 3b |
        var scale_val: u8 = @intFromEnum(scale);
        scale_val <<= 6;
        index_val <<= 3;
        return scale_val | index_val | base_val;
    }


};
