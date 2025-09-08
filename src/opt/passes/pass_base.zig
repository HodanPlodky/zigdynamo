const std = @import("std");
const Compiler = @import("../compile.zig").Compiler;
const ir = @import("../ir.zig");
const AnalysisBase = @import("../analysis/analysis_base.zig").AnalysisBase;

pub const PassBase = struct {
    compiler: *Compiler,
    alloc: std.mem.Allocator,
    analysis_base: AnalysisBase,
};
