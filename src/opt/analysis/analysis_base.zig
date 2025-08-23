const Compiler = @import("../compile.zig").Compiler;
const std = @import("std");

pub const AnalysisBase = struct {
    compiler: *const Compiler,
    alloc: std.mem.Allocator,
};
