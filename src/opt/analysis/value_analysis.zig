const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const SharedData = @import("analysis_base.zig").SharedData;
const DominatorAnalysis = @import("dominator.zig").DominatorAnalysis;
const ir = @import("../ir.zig");

pub const ValueAnalysis = struct {
    base: Base,
    values: []ir.Reg,
    dom: DominatorAnalysis,

    pub fn init(base: Base) !ValueAnalysis {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        return ValueAnalysis{
            .base = base,
            .values = base.alloc.alloc(ir.Reg, inst_count.get_usize()),
            .dom = try DominatorAnalysis.init(base),
        };
    }

    pub fn analyze(self: *ValueAnalysis) !void {
        self.dom.analyze();
        for (self.base.compiler.stores.function.data.items) |function| {
            self.process_bb(function.entry);
        }
    }

    // going over tree so it should not need to check for double call
    fn process_bb(self: *ValueAnalysis, bb_idx: ir.BasicBlockIdx) !void {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);

        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.compiler.get(ir.Instruction, inst_idx);
            switch (inst) {
                .mov, .parallel_mov => |reg| self.values[inst_idx] = self.values[reg],
                else => self.values[inst_idx] = inst_idx,
            }
        }

        for (self.dom.domtree_edges[bb_idx].items) |succ_idx| {
            self.process_bb(succ_idx);
        }
    }
};
