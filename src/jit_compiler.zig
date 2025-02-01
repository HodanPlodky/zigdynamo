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

    pub fn get_offset(comptime field_name: []const u8) u32 {
        return comptime {
            for (@typeInfo(JitState).Struct.fields, 0..) |field, index| {
                if (std.mem.eql(u8, field_name, field.name)) {
                    return index * @sizeOf(usize);
                }
            }
            @compileError("did not find field");
        };
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
};

pub const JitCompiler = struct {
    code_slice: []u8,
    code_ptr: usize,
    pc: usize,
    bytecode: []const u8,

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

        return JitCompiler{
            .code_slice = code_slice,
            .code_ptr = 0,
            .pc = undefined,
            .bytecode = undefined,
        };
    }

    pub fn compile_fn(self: *JitCompiler, function: bytecode.Constant) JitError!JitFunction {
        switch (function.get_type()) {
            bytecode.ConstantType.function => self.pc = 0,
            else => return JitError.CanOnlyCompileFn,
        }

        const start = self.code_ptr;

        // ignore the header of function
        self.bytecode = function.get_slice()[(4 + 1 + 4 + 4)..];

        try self.compile_prolog();

        while (self.pc < self.bytecode.len) {
            try self.compile_bytecode_inst();
        }

        // should not be necessary but oh well I am pussy
        try self.compile_epilog();
        try self.compile_add_byte(0xc3);

        return JitFunction{ .code = @ptrCast(&self.code_slice[start]) };
    }

    fn compile_bytecode_inst(self: *JitCompiler) !void {
        const inst = self.read_inst();
        switch (inst) {
            bytecode.Instruction.pop => {
                // dec QWORD PTR [rbx+0x60]
                // rbx is self ptr
                // 0x60 is address of len of stack
                const tmp: [4]u8 = .{ 0x48, 0xff, 0x4b, 0x60 };
                try self.compile_add_slice(tmp[0..]);
            },
            bytecode.Instruction.ret => {
                try self.mov_from_jit_state(GPR64.rdi, "env");
                try self.mov_from_jit_state(GPR64.rax, "pop_locals");
                try self.call_from_rax();
                try self.compile_epilog();

                // return
                try self.compile_add_byte(0xc3);
            },

            bytecode.Instruction.push_byte => {
                try self.mov_from_jit_state(GPR64.rdi, "stack");
                try self.mov_from_jit_state(GPR64.rax, "push");

                const number: u32 = @intCast(self.bytecode[self.pc]);
                const value = runtime.Value.new_num(number);

                try self.set_reg_64(GPR64.rsi, value.data);

                try self.call_from_rax();

                self.pc += 1;
            },
            else => {
                std.debug.print("{}\n", .{inst});
                @panic("unimplemented");
            },
        }
    }

    fn mov_from_jit_state(self: *JitCompiler, to_reg: GPR64, comptime field_name: []const u8) !void {
        const offset = comptime JitState.get_offset(field_name);
        try self.mov_from_jit_state_offset(to_reg, offset);
    }

    /// emits instruction for moving the 64bit value from self (Interpreter) into
    /// 64bit register, this assumes that the pointer to self is stored in rbx
    fn mov_from_jit_state_offset(self: *JitCompiler, to_reg: GPR64, offset: u32) !void {
        // REX.W + 8B /r
        const to_reg_val: u8 = @intFromEnum(to_reg);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we set the self
        // ptr reg as rbx and that has highes bit num 0
        try self.compile_add_byte(0x48 | ((to_reg_val & 0x8) >> 1));

        // opcode
        try self.compile_add_byte(0x8b);

        // ModRM
        // | mode | reg | r/m |
        // mode will be less then 0b11 => setting it to 0b10 (thats what I saw in wild)
        // only exception would be if the offset would be 0
        // 10 | bottom 3 bits of to_reg | rbx bottom 3 bit (0x2)
        const mod: u8 = if (offset != 0)
            0x80
        else
            0x00;

        const to_reg_lower = to_reg_val & 0x7;
        const rbx_reg: u4 = @intFromEnum(GPR64.rbx);
        try self.compile_add_byte(mod | (to_reg_lower << 3) | rbx_reg);

        // if offset is not zero we have to
        // add it at the end of the instruction
        if (offset != 0) {
            const offset_bytes: [4]u8 = .{
                @intCast(offset & 0xff),
                @intCast((offset >> 8) & 0xff),
                @intCast((offset >> 16) & 0xff),
                @intCast((offset >> 24) & 0xff),
            };
            try self.compile_add_slice(offset_bytes[0..]);
        }
    }

    fn set_reg_64(self: *JitCompiler, reg: GPR64, value: u64) !void {
        const reg_val: u8 = @intFromEnum(reg);
        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we move immediate
        // and there is no other reg
        try self.compile_add_byte(0x48 | ((reg_val & 0x4) >> 1));

        // opcode has in it self the lower 3 bits of reg
        // index (0xb8 is base and you add those)
        const base_opcode: u8 = 0xb8;
        const opcode = base_opcode + (reg_val & 0x7);
        try self.compile_add_byte(opcode);

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

        try self.compile_add_slice(value_bytes[0..]);
    }

    fn call_from_rax(self: *JitCompiler) !void {
        const slice: [2]u8 = .{ 0xff, 0xd0 };
        try self.compile_add_slice(slice[0..]);
    }

    fn mov_index_access64(self: *JitCompiler, to_reg: GPR64, scale: Scale, base: GPR64, index: GPR64, offset: u8) !void {
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

        const to_reg_val: u8 = @intFromEnum(to_reg);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we set the sib
        // byte which means the r/m will be 0b100
        try self.compile_add_byte(0x48 | ((to_reg_val & 0x4) >> 1));

        // opcode
        try self.compile_add_byte(0x8b);

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

        const to_reg_lower: u8 = (to_reg_val & 0x7) << 3;
        const rm_sib: u8 = 0b100;
        try self.compile_add_byte(mod | to_reg_lower | rm_sib);

        // SIB
        // | scale : 2b | index : 3b | base : 3b |
        var scale_value: u8 = @intFromEnum(scale);
        scale_value <<= 6;
        var index_value: u8 = @intFromEnum(index);
        index_value <<= 3;
        const base_value: u8 = @intFromEnum(base);
        try self.compile_add_byte(scale_value | index_value | base_value);

        // if offset is zero it is captured above
        if (offset != 0) {
            try self.compile_add_byte(offset);
        }
    }

    fn read_inst(self: *JitCompiler) bytecode.Instruction {
        const res: bytecode.Instruction = @enumFromInt(self.bytecode[self.pc]);
        self.pc += 1;
        return res;
    }

    fn compile_prolog(self: *JitCompiler) !void {
        // push rax
        try self.compile_add_byte(0x50);
        // push rbx
        try self.compile_add_byte(0x53);
        // mov rbx, rdi
        const move_self: [3]u8 = .{ 0x48, 0x89, 0xfb };
        try self.compile_add_slice(move_self[0..]);
    }

    fn compile_epilog(self: *JitCompiler) !void {
        // pop rbx
        try self.compile_add_byte(0x5b);
        // pop rax
        try self.compile_add_byte(0x58);
    }

    fn compile_add_slice(self: *JitCompiler, inst: []const u8) !void {
        if (inst.len > (self.code_slice.len - self.code_ptr)) {
            return JitError.OutOfMem;
        }
        @memcpy(self.code_slice[self.code_ptr..(self.code_ptr + inst.len)], inst);
        self.code_ptr += inst.len;
    }

    fn compile_add_byte(self: *JitCompiler, byte: u8) !void {
        const slice: [1]u8 = .{byte};
        try self.compile_add_slice(slice[0..]);
    }
};
