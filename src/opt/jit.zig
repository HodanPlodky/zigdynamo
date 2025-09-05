const jit_utils = @import("../jit_utils.zig");

const GPR64 = jit_utils.GPR64;

pub const JitCompiler = struct {
    code_slice: []u8,
    code_ptr: usize,
};
