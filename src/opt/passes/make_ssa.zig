const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const DominantorAnalysis = @import("../analysis/dominator.zig").DominatorAnalysis;
const ir = @import("../ir.zig");

pub const MakeSSA = struct {
    base: Base,
    dom: DominantorAnalysis,

    counters: []u32,

    pub fn init(base: Base, local_count: u32) !MakeSSA {
        return MakeSSA{
            .base = base,
            .counters = try base.alloc.alloc(u32, local_count),
            .dom = try DominantorAnalysis.init(base.analysis_base),
        };
    }

    pub fn run(self: *MakeSSA) !void {
        try self.dom.analyze();
        for (
            self.base.compiler.stores.set_local.data.items(.basicblock_idx),
            self.base.compiler.stores.set_local.data.items(.local_idx),
        ) |bb_idx, local_idx| {
            _ = local_idx;
            var iter = self.dom.frontiers[bb_idx.index].iterator(.{});
            while (iter.next()) |front_bb_idx| {
                const front_bb = ir.BasicBlockIdx.new(@intCast(front_bb_idx));
                _ = try self.base.compiler.insert_inst(front_bb, ir.Instruction.nop, 0);
            }
        }
    }
};
