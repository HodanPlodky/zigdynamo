const std = @import("std");
const Compiler = @import("../compile.zig").Compiler;
const ir = @import("../ir.zig");
const AnalysisBase = @import("../analysis/analysis_base.zig").AnalysisBase;
const Stores = @import("../stores.zig").Stores;

pub const PassBase = struct {
    compiler: *Compiler,
    alloc: std.mem.Allocator,
    analysis_base: AnalysisBase,

    pub fn get(self: *const PassBase, comptime T: type, idx: Stores.get_index_type(T)) T {
        return self.compiler.get(T, idx);
    }
};
