const std = @import("std");
const bytecode = @import("bytecode.zig");
const bc_interpret = @import("bc_interpreter.zig");
const runtime = @import("runtime.zig");

pub const JitError = error{
    CanOnlyCompileFn,
    OutOfMem,
};

pub const JitFunction = struct {
    code: [*]u8,

    pub fn run(self: *const JitFunction, state: *const JitState) void {
        const f: *const fn (*const JitState) callconv(.C) void = @alignCast(@ptrCast(self.code));
        f(state);
    }
};

/// struct representing state of interpreter
/// for jit compiled code this is necessary because
/// normal structs in zig do not gurantee order of
/// fields and jit code needs different set of fields
/// then the bytecode interpreter
pub const JitState = extern struct {
    intepreter: *const bc_interpret.Interpreter,
    stack: *const bc_interpret.Stack,
    env: *const bc_interpret.Environment,
    gc: *const bc_interpret.GC,

    // basic
    push: *const fn (*bc_interpret.Stack, runtime.Value) callconv(.C) void,
    get_local: *const fn (*const bc_interpret.Environment, u32) callconv(.C) runtime.Value,
    set_local: *const fn (*bc_interpret.Environment, u32, runtime.Value) callconv(.C) void,

    // call handle
    pop_locals: *const fn (*bc_interpret.Environment) callconv(.C) void,
    push_locals: *const fn (*bc_interpret.Environment, u64, usize, u32) callconv(.C) void,

    // gc handle
    gc_alloc_object: *const fn (*bc_interpret.Interpreter, usize) callconv(.C) *bytecode.Object,
    gc_alloc_closure: *const fn (*bc_interpret.Interpreter, usize) callconv(.C) *bytecode.Closure,

    // panics
    binop_panic: *const fn (runtime.Value, runtime.Value) callconv(.C) void,

    pub fn get_offset(comptime field_name: []const u8) u32 {
        return @offsetOf(JitState, field_name);
    }
};

const GPR64 = enum(u4) {
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

const Scale = enum(u2) {
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

const PanicTable = struct {
    binop_panic: usize = 0,
};

pub const JitCompiler = struct {
    const stack_addr = GPR64.r15;
    const state_addr = GPR64.rbx;

    code_slice: []u8,
    code_ptr: usize,
    pc: usize,
    bytecode: []const u8,
    panic_table: PanicTable,

    pub fn init(code_buffer_size: usize) JitCompiler {
        const addr = std.os.linux.mmap(
            null,
            code_buffer_size,
            std.os.linux.PROT.READ | std.os.linux.PROT.WRITE | std.os.linux.PROT.EXEC,
            std.os.linux.MAP{
                .ANONYMOUS = true,
                .TYPE = std.os.linux.MAP_TYPE.PRIVATE,
            },
            -1,
            0,
        );

        const code: [*]u8 = @ptrFromInt(addr);
        const code_slice = code[0..code_buffer_size];

        var res = JitCompiler{
            .code_slice = code_slice,
            .code_ptr = 0,
            .pc = undefined,
            .bytecode = undefined,
            .panic_table = .{},
        };
        res.init_panic_handlers() catch unreachable;
        return res;
    }

    /// emit panic calls for and populate panic call table
    fn init_panic_handlers(self: *JitCompiler) !void {
        try self.emit_panic_handler_with_prolog("binop_panic", JitCompiler.bin_op_panic_prolog);
    }

    fn bin_op_panic_prolog(self: *JitCompiler) !void {
        try self.mov_reg_reg(GPR64.rdi, GPR64.r8);
        try self.mov_reg_reg(GPR64.rsi, GPR64.r9);
    }

    fn emit_panic_handler(self: *JitCompiler, comptime panic_name: []const u8) !void {
        @field(self.panic_table, panic_name) = self.code_ptr;
        try self.call(panic_name);
    }

    fn emit_panic_handler_with_prolog(self: *JitCompiler, comptime panic_name: []const u8, comptime prolog: fn (*JitCompiler) JitError!void) !void {
        @field(self.panic_table, panic_name) = self.code_ptr;
        try prolog(self);
        try self.call(panic_name);
    }

    /// creates jump depending on current flags such that if the test
    /// return false then in jumps to panic
    fn emit_panic(self: *JitCompiler, comptime panic_name: []const u8) !void {
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

    pub fn compile_fn(self: *JitCompiler, function: bytecode.Constant) JitError!JitFunction {
        switch (function.get_type()) {
            bytecode.ConstantType.function => self.pc = 0,
            else => return JitError.CanOnlyCompileFn,
        }

        const start = self.code_ptr;

        // ignore the header of function
        self.bytecode = function.get_slice()[(4 + 1 + 4 + 4)..];

        try self.emit_prolog();

        while (self.pc < self.bytecode.len) {
            try self.compile_bytecode_inst();
        }

        // should not be necessary but oh well I am pussy
        try self.emit_epilog();
        try self.emit_byte(0xc3);

        return JitFunction{ .code = @ptrCast(&self.code_slice[start]) };
    }

    fn compile_bytecode_inst(self: *JitCompiler) !void {
        const inst = self.read_inst();
        switch (inst) {
            bytecode.Instruction.pop => {
                try self.stack_pop();
            },
            bytecode.Instruction.ret => {
                try self.mov_from_jit_state(GPR64.rdi, "env");
                try self.call("pop_locals");
                try self.emit_epilog();

                // return
                try self.emit_byte(0xc3);
            },

            bytecode.Instruction.push_byte => {
                try self.mov_reg_reg(GPR64.rdi, stack_addr);

                const number: u32 = @intCast(self.bytecode[self.pc]);
                const value = runtime.Value.new_num(number);

                try self.set_reg_64(GPR64.rsi, value.data);

                try self.call("push");

                self.pc += 1;
            },
            bytecode.Instruction.nil => {
                try self.mov_reg_reg(GPR64.rdi, stack_addr);
                const value = runtime.Value.new_nil();
                try self.set_reg_64(GPR64.rsi, value.data);
                try self.call("push");
            },
            bytecode.Instruction.add => {
                try self.get_stack_to_reg(GPR64.r9, 0);
                try self.get_stack_to_reg(GPR64.r8, 1);

                // check lower bits
                try self.mov_reg_reg(GPR64.rdi, GPR64.r8);
                // or rdi, r9
                // or r/m64 reg
                try self.emit_basic_reg(0x09, GPR64.r9, GPR64.rdi);
                // test dil,0x7 (dil lowest 8 bit of rdi)
                // 40 f6 c7 07
                const test_slice: [4]u8 = .{ 0x40, 0xf6, 0xc7, 0x07 };
                try self.emit_slice(test_slice[0..]);

                // handle cond
                try self.emit_panic("binop_panic");

                // add r8,r9
                // add r/m64 reg =>
                try self.emit_basic_reg(0x01, GPR64.r9, GPR64.r8);

                try self.stack_pop();
                try self.stack_set_top(GPR64.r8);
            },
            else => {
                std.debug.print("{}\n", .{inst});
                @panic("unimplemented");
            },
        }
    }
    fn read_inst(self: *JitCompiler) bytecode.Instruction {
        const res: bytecode.Instruction = @enumFromInt(self.bytecode[self.pc]);
        self.pc += 1;
        return res;
    }

    //
    // State helper
    //

    /// load value from JIT state based on field name of the value
    fn mov_from_jit_state(self: *JitCompiler, dst: GPR64, comptime field_name: []const u8) !void {
        const offset = comptime JitState.get_offset(field_name);
        try self.mov_from_jit_state_offset(dst, offset);
    }

    /// emits instruction for moving the 64bit value from JIT state into
    /// 64bit register, this assumes that the pointer to state is stored in rbx
    fn mov_from_jit_state_offset(self: *JitCompiler, to_reg: GPR64, offset: u32) !void {
        try self.mov_from_struct_64(to_reg, state_addr, offset);
    }

    //
    // Calls
    //

    fn call(self: *JitCompiler, comptime function_name: []const u8) !void {
        try self.mov_from_jit_state(GPR64.rax, function_name);
        try self.call_from_rax();
    }

    fn call_from_rax(self: *JitCompiler) !void {
        // call rax
        const slice: [2]u8 = .{ 0xff, 0xd0 };
        try self.emit_slice(slice[0..]);
    }

    //
    // Stack helpers
    //

    /// clobers rax and rcx
    fn get_stack_to_reg(self: *JitCompiler, dst: GPR64, offset: u8) !void {
        // load len
        try self.mov_from_struct_64(GPR64.rcx, stack_addr, 8);

        // load stack ptr
        try self.deref_ptr(GPR64.rax, stack_addr);

        // load value from top of stack with offset
        // mov reg, [rax + rcx*8 - offset]
        // offset is calculated with two's complement
        const scale = Scale.from_size(@sizeOf(runtime.Value));
        try self.mov_index_access64(dst, scale, GPR64.rax, GPR64.rcx, (~((offset + 1) * @sizeOf(runtime.Value))) + 1);
    }

    /// clobers rax
    fn stack_pop(self: *JitCompiler) !void {
        const stack_addr_val: u8 = @intFromEnum(stack_addr);

        const len_offset = 8;
        // dec QWORD PTR [stack_addr+0x60]
        // 49 ff 4f 60
        // 49 = REX.W | B-bit = high stack_addr
        // ff = opcode ????
        // 4f = modrm = 01_001_[lower_stack_addr]
        // 60 = offset
        const rex = 0x48 | ((stack_addr_val & 0x8) >> 3);
        const modrm = 0b01_001_000 | (stack_addr_val & 0x7);
        const dec_slice: [4]u8 = .{ rex, 0xff, modrm, len_offset };
        try self.emit_slice(dec_slice[0..]);
    }

    /// clobers rax, rcx
    fn stack_set_top(self: *JitCompiler, dst: GPR64) !void {
        const reg_val: u8 = @intFromEnum(dst);

        // load len
        try self.mov_from_struct_64(GPR64.rcx, stack_addr, 8);

        // load stack ptr
        try self.deref_ptr(GPR64.rax, stack_addr);

        // mov QWORD PTR [rax+rcx*8-0x8],r8
        // 4c 89 44 c8 f8
        // 4c = REX => set W R B
        // 89 = opcode
        // 44 = modrm = 01_000_100
        // c8 = sib = 11_001_000 = scale8 | rcx | rax
        // f8 = -0x8 = size of Value
        const rex = 0x48 | ((reg_val & 0x8) >> 1);
        const modrm = 0x40 | ((reg_val & 0x7) << 3) | 0b100;
        const scale = Scale.from_size(@sizeOf(runtime.Value));
        const scale_bits: u8 = @intFromEnum(scale);
        const sib = (scale_bits << 6) | 0b1000;
        const size: u8 = @sizeOf(runtime.Value);
        const offset = ~size + 1;

        const move_slice: [5]u8 = .{ rex, 0x89, modrm, sib, offset };
        try self.emit_slice(move_slice[0..]);
    }

    //
    // Struct access
    //

    fn mov_from_struct_64(self: *JitCompiler, dst: GPR64, base: GPR64, offset: u32) !void {
        // REX.W + 8B /r
        const dst_val: u8 = @intFromEnum(dst);
        const base_val: u8 = @intFromEnum(base);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we set the self
        // ptr reg as rbx and that has highes bit num 0
        try self.emit_byte(0x48 | ((base_val & 0x8) >> 3) | ((dst_val & 0x8) >> 1));

        // opcode
        try self.emit_byte(0x8b);

        // ModRM
        // | mode | reg | r/m |
        // mode will be less then 0b11 => setting it to 0b10 (thats what I saw in wild)
        // only exception would be if the offset would be 0
        // 10 | bottom 3 bits of to_reg | rbx bottom 3 bit (0x2)
        const mod: u8 = if (offset != 0)
            0x80
        else
            0x00;

        const to_reg_lower = dst_val & 0x7;
        try self.emit_byte(mod | (to_reg_lower << 3) | (base_val & 0x7));

        // if offset is not zero we have to
        // add it at the end of the instruction
        if (offset != 0) {
            const offset_bytes: [4]u8 = .{
                @intCast(offset & 0xff),
                @intCast((offset >> 8) & 0xff),
                @intCast((offset >> 16) & 0xff),
                @intCast((offset >> 24) & 0xff),
            };
            try self.emit_slice(offset_bytes[0..]);
        }
    }

    fn mov_index_access64(self: *JitCompiler, dst: GPR64, scale: Scale, base: GPR64, index: GPR64, offset: u8) !void {
        // example:
        //      48 8b 74 d1 f8
        //      REX.W opcode 01_110_100
        //      rcx = 1
        //      rdx = 2
        //      rsi = 6
        //      modrm = 01_11_100
        //      01 = indirect
        //      110 = 6 = rsi
        //      100 = 4 => its sib
        //      sib = 11_010_001
        //      scale = 11 => 8
        //      index = 010 => rdx
        //      base = 001 => rcx
        //      f8 = 11111000 = -8
        // mov rsi,QWORD PTR [rcx+rdx*8-0x8]

        const dst_val: u8 = @intFromEnum(dst);
        var index_value: u8 = @intFromEnum(index);
        var base_value: u8 = @intFromEnum(base);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The R contains highest bit of reg number
        // The X contains highest bit of index number
        // The B contains highest bit of base number
        try self.emit_byte(0x48 | ((dst_val & 0x8) >> 1) | ((index_value & 0x8) >> 2) | ((base_value & 0x8) >> 3));

        // after this I dont need higher bits for index and base
        index_value &= 0x7;
        base_value &= 0x7;

        // opcode
        try self.emit_byte(0x8b);

        // ModRM
        // | mode | reg | r/m |
        // mode = 01 =>
        //      it is indirect but still not sure why
        //      whats difference between 10 and 01
        //      I think it has to do with sib
        //      again there is an exception for cases
        //      where there is not data after sib
        //      (or in our case offset is zero) in that
        //      case the mod is 0
        // reg = bottom 3 bits of to_reg_val
        // rm = 100 => the sib follows
        const mod: u8 = if (offset != 0)
            0b0100_0000
        else
            0;

        const to_reg_lower: u8 = (dst_val & 0x7) << 3;
        const rm_sib: u8 = 0b100;
        try self.emit_byte(mod | to_reg_lower | rm_sib);

        // SIB
        // | scale : 2b | index : 3b | base : 3b |
        var scale_value: u8 = @intFromEnum(scale);
        scale_value <<= 6;
        index_value <<= 3;
        try self.emit_byte(scale_value | index_value | base_value);

        // if offset is zero it is captured above
        if (offset != 0) {
            try self.emit_byte(offset);
        }
    }

    //
    // Regs movement
    //

    fn mov_reg_reg(self: *JitCompiler, dst: GPR64, src: GPR64) !void {
        // mov rbx, rdi
        // 48 89 fb
        // 48 = REX
        // 89 = opcode (mod r/m64, r)
        // fb = modrm 11_111_011 => mod | src | dst

        // REX | W | R | X | B
        // reg => R, r/m64 => B
        //const rex = 0x48 | ((src_val & 0x8) >> 1) | ((dst_val & 0x8) >> 3);
        const rex = JitCompiler.create_rex(src, dst);
        try self.emit_byte(rex);

        // opcode
        try self.emit_byte(0x89);

        // MODrm
        // mod = 0b11 | lower bits src | lower bits dest
        const modrm = JitCompiler.create_modrm(src, dst);
        try self.emit_byte(modrm);
    }

    fn set_reg_64(self: *JitCompiler, reg: GPR64, value: u64) !void {
        const reg_val: u8 = @intFromEnum(reg);
        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we move immediate
        // and there is no other reg
        try self.emit_byte(0x48 | ((reg_val & 0x8) >> 1));

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

    fn deref_ptr(self: *JitCompiler, dst: GPR64, src: GPR64) !void {
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
    // Prolog and epilog
    //

    fn emit_prolog(self: *JitCompiler) !void {
        // push rax
        try self.emit_byte(0x50);
        // push rbx
        try self.emit_byte(0x53);
        // push r8
        try self.emit_byte(0x41);
        try self.emit_byte(0x50);
        // push r9
        try self.emit_byte(0x41);
        try self.emit_byte(0x51);
        // push r15
        try self.emit_byte(0x41);
        try self.emit_byte(0x57);

        // mov rbx, rdi
        // rbx will store address to state
        try self.mov_reg_reg(state_addr, GPR64.rdi);

        // mov r15, [rbx + <stack offset>]
        // r15 will store stack addr
        try self.mov_from_jit_state(stack_addr, "stack");
    }

    fn emit_epilog(self: *JitCompiler) !void {
        // pop r15
        try self.emit_byte(0x41);
        try self.emit_byte(0x5f);
        // pop r9
        try self.emit_byte(0x41);
        try self.emit_byte(0x59);
        // pop r8
        try self.emit_byte(0x41);
        try self.emit_byte(0x58);
        // pop rbx
        try self.emit_byte(0x5b);
        // pop rax
        try self.emit_byte(0x58);
    }

    //
    // Basic emits
    //

    fn emit_slice(self: *JitCompiler, inst: []const u8) !void {
        if (inst.len > (self.code_slice.len - self.code_ptr)) {
            return JitError.OutOfMem;
        }
        @memcpy(self.code_slice[self.code_ptr..(self.code_ptr + inst.len)], inst);
        self.code_ptr += inst.len;
    }

    fn emit_byte(self: *JitCompiler, byte: u8) !void {
        const slice: [1]u8 = .{byte};
        try self.emit_slice(slice[0..]);
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
    fn create_modrm(reg: GPR64, rm64: GPR64) u8 {
        const reg_val: u8 = @intFromEnum(reg);
        const rm64_val: u8 = @intFromEnum(rm64);

        // MODrm
        // mod = 0b11 = direct | lower bits reg | lower bits rm64
        return 0b11_000_000 | ((reg_val & 0x7) << 3) | (rm64_val & 0x7);
    }

    /// emits most basic inst like add with only regs
    /// check if you can use this before going all in
    fn emit_basic_reg(self: *JitCompiler, opcode: u8, reg: GPR64, rm64: GPR64) !void {
        const inst_slice: [3]u8 = .{
            JitCompiler.create_rex(reg, rm64),
            opcode,
            JitCompiler.create_modrm(reg, rm64),
        };

        try self.emit_slice(inst_slice[0..]);
    }

    fn emit_break(self: *JitCompiler) !void {
        try self.emit_byte(0xcc);
    }
};
