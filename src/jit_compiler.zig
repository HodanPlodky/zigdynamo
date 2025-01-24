const std = @import("std");
const bytecode = @import("bytecode.zig");

pub const JitError = error {
    CanOnlyCompileFn,
};

pub const JitFunction = struct {
    code: [*]u8,
};

pub const JitCompiler = struct {
    code: [*]u8,
    code_ptr: usize,
    pc: usize,
    bytecode: []u8,

    pub fn init(code_buffer_size: usize) JitCompiler {
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

        return JitCompiler{
            .code = code,
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

        // ignore the header of function
        self.bytecode = function.get_slice()[(4 + 1 + 4 + 4)..];

        while (self.pc < self.bytecode.len) {
            self.compile_bytecode_inst();
        }

        unreachable;
    }

    fn compile_bytecode_inst(self: *JitCompiler) !void {
        
    }
};
