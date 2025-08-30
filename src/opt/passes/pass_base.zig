const std = @import("std");
const Compiler = @import("../compile.zig").Compiler;
const ir = @import("../ir.zig");
const AnalysisBase = @import("../analysis/analysis_base.zig").AnalysisBase;

pub const PassBase = struct {
    compiler: *Compiler,
    alloc: std.mem.Allocator,
    function_idx: ir.FunctionIdx,
    analysis_base: AnalysisBase,

    pub fn get_entry(self: *const PassBase) ir.Function {
        return self.compiler.get(ir.Function, self.function_idx);
    }
};
