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
            .values = try base.alloc.alloc(ir.Reg, inst_count.get_usize()),
            .dom = try DominatorAnalysis.init(base),
        };
    }

    pub fn analyze(self: *ValueAnalysis) !void {
        try self.dom.analyze();
        for (self.base.compiler.stores.function.data.items) |function| {
            try self.process_bb(function.entry);
        }
    }

    // going over tree so it should not need to check for double call
    fn process_bb(self: *ValueAnalysis, bb_idx: ir.BasicBlockIdx) !void {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);

        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.compiler.get(ir.Instruction, inst_idx);
            switch (inst) {
                .mov, .parallel_copy => |reg| {
                    self.set(inst_idx, reg);
                },
                else => self.values[inst_idx.get_usize()] = inst_idx,
            }
        }

        for (self.dom.domtree_edges[bb_idx.get_usize()].items) |succ_idx| {
            try self.process_bb(succ_idx);
        }
    }

    pub fn set(self: *ValueAnalysis, dst: ir.Reg, src: ir.Reg) void {
        self.values[dst.get_usize()] = self.values[src.get_usize()];
    }

    pub fn get(self: *const ValueAnalysis, reg: ir.Reg) ir.Reg {
        return self.values[reg.get_usize()];
    }
};
