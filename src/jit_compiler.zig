const std = @import("std");
const bytecode = @import("bytecode.zig");
const bc_interpret = @import("bc_interpreter.zig");

pub const JitError = error{
    CanOnlyCompileFn,
    OutOfMem,
};

pub const JitFunction = struct {
    code: [*]u8,

    pub fn run(self: *const JitFunction, interpreter: *anyopaque) void {
        const f: *const fn (*anyopaque) callconv(.C) void = @alignCast(@ptrCast(self.code));
        std.debug.print("call jit: {x}\n", .{f});
        f(interpreter);
    }
};

/// struct representing state of interpreter
/// for jit compiled code this is necessary because
/// normal structs in zig do not gurantee order of
/// fields and jit code needs different set of fields
/// then the bytecode interpreter
pub const JitState = extern struct {
    intepreter: *bc_interpret.Interpreter,
    stack: *bc_interpret.Stack,
    env: *bc_interpret.Environment,
    gc: *bc_interpret.GC,
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
        std.debug.print("jit code: {x}\n", .{addr});

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
                // load self.env.local.buffer.ptr rcx
                // mov rcx,QWORD PTR [rbx+0x90]
                try self.mov_from_self_64(GPR64.rcx, 0x90);

                // load self.env.local.buffer.len to rdx
                // mov rdx,QWORD PTR [rbx+0x98]
                try self.mov_from_self_64(GPR64.rdx, 0x98);

                // load old fp
                // mov eax,DWORD PTR [rcx+rdx*8-0x10]
                //self.mov_index_access(G)

                // mov eax,DWORD PTR [rcx+rdx*8-0x10]
                // we can do it in the 64bit since we know that
                // the old fp value will for sure be in lower bits
                // 0xf0 = -10
                try self.mov_index_access64(GPR64.rax, Scale.scale8, GPR64.rcx, GPR64.rdx, 0xf0);

                try self.compile_epilog();

                // return
                try self.compile_add_byte(0xc3);
            },

            bytecode.Instruction.push_byte => {
                // load stack ptr
                try self.mov_from_self_64(GPR64.rdi, 0x58);

                // load stack len
                try self.mov_from_self_64(GPR64.rsi, 0x58);

                // inc rsi
                const inc_rsi: [3]u8 = .{ 0x48, 0xff, 0xc6 };
                try self.compile_add_slice(inc_rsi[0..]);
                self.pc += 1;
            },
            else => {
                std.debug.print("{}\n", .{inst});
                @panic("unimplemented");
            },
        }
    }

    /// emits instruction for moving the 64bit value from self (Interpreter) into
    /// 64bit register, this assumes that the pointer to self is stored in rbx
    fn mov_from_self_64(self: *JitCompiler, to_reg: GPR64, offset: u32) !void {
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
        // push rbx
        try self.compile_add_byte(0x53);
        // mov rbx, rdi
        const move_self: [3]u8 = .{ 0x48, 0x89, 0xfb };
        try self.compile_add_slice(move_self[0..]);
    }

    fn compile_epilog(self: *JitCompiler) !void {
        // pop rbx
        try self.compile_add_byte(0x5b);
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
