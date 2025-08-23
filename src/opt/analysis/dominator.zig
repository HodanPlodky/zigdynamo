const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");

pub const DominatorAnalysis = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    dominators: []BitSet,
    post_order: std.ArrayListUnmanaged(ir.BasicBlockIdx),

    temp_set: BitSet,

    pub fn init(base: Base) !DominatorAnalysis {
        const bb_count = base.compiler.stores.get_max_idx(ir.BasicBlock);
        const res = DominatorAnalysis{
            .base = base,
            .dominators = try base.alloc.alloc(BitSet, bb_count),
            .post_order = try std.ArrayListUnmanaged(ir.BasicBlockIdx).initCapacity(base.alloc, bb_count),
            .temp_set = try BitSet.initFull(base.alloc, bb_count),
        };

        return res;
    }

    pub fn analyze(self: *DominatorAnalysis, function_idx: ir.FunctionIdx) !void {
        const bb_count = self.base.compiler.stores.get_max_idx(ir.BasicBlock);
        for (self.dominators) |*doms| {
            doms.* = try BitSet.initFull(self.base.alloc, bb_count);
        }
        const function = self.base.compiler.get_const_ptr(ir.Function, function_idx);
        var visited = BitSet.initEmpty(self.base.alloc, bb_count);

        // get post order of basicblocks
        self.dfs(&visited, function.basicblocks);

        while (true) {
            var change = false;

            var index: usize = self.post_order.items.len;
            while (index > 0) {
                index -= 1;
                const bb_idx = self.post_order[index];
                const bb = self.base.compiler.get_const_ptr(ir.BasicBlock, bb_idx);
                self.temp_set.setAll();
                for (bb.predecessors) |pred| {
                    const pred_dom_set = self.dominators[pred.index];
                    self.temp_set.setIntersection(pred_dom_set);
                }
                self.temp_set.set(bb_idx.index);

                const orig_set = self.dominators[bb_idx.index];
                if (self.temp_set.eql(orig_set)) {
                    self.dominators[bb_idx.index];
                    change = true;
                }
            }

            if (!change) {
                break;
            }
        }
    }

    fn dfs(self: *DominatorAnalysis, visited: *BitSet, basicblock_idx: ir.BasicBlockIdx) void {
        if (visited.isSet(basicblock_idx.index)) {
            return;
        }

        visited.set(basicblock_idx.index);

        var succesors = self.base.compiler.get_succesors(basicblock_idx);
        while (succesors.next()) |succ| {
            self.dfs(visited, succ);
        }

        // should be allocated from the init
        self.post_order.appendAssumeCapacity(basicblock_idx);
    }
};
