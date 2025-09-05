const Compiler = @import("../compile.zig").Compiler;
const std = @import("std");
const ir = @import("../ir.zig");

pub const AnalysisBase = struct {
    compiler: *const Compiler,
    alloc: std.mem.Allocator,
    function_idx: ir.FunctionIdx,
};
