const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const DominantorAnalysis = @import("../analysis/dominator.zig").DominatorAnalysis;
const ir = @import("../ir.zig");

const ParallelCopyGroup = struct {
    insts: std.ArrayList(ir.InstructionIdx),
};

/// create a CSSA format
/// to have correct out of ssa translation it good to go first to cssa
/// if the program is in cssa it ok just to set all of the variables
/// connected through the phi (phony here) function to the same register
/// or place in memory
/// Revisiting Out-of-SSA Translation for Correctness, Code Quality, and Efficiency
/// https://inria.hal.science/inria-00349925
pub const MakeCSSA = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    groups_start: std.ArrayList(ParallelCopyGroup),
    groups_end: std.ArrayList(ParallelCopyGroup),
    base: Base,

    pub fn init(base: Base) !MakeCSSA {
        const bb_count = base.compiler.stores.get_max_idx(ir.BasicBlock);
        return MakeCSSA{
            .base = base,
            .groups_start = try std.ArrayList(ParallelCopyGroup).initCapacity(
                base.alloc,
                bb_count.get_usize(),
            ),
            .groups_end = try std.ArrayList(ParallelCopyGroup).initCapacity(
                base.alloc,
                bb_count.get_usize(),
            ),
        };
    }

    pub fn run(self: *MakeCSSA) !void {
        var iter = self.base.compiler.stores.idx_iter(ir.BasicBlock);
        while (iter.next()) |bb_idx| {
            try self.process_bb(bb_idx);
        }
    }

    fn process_bb(self: *MakeCSSA, bb_idx: ir.BasicBlockIdx) !void {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.compiler.get(ir.Instruction, inst_idx);
            switch (inst) {
                .phony => |phony_idx| try self.process_phony(inst_idx, phony_idx, bb_idx),
                .parallel_copy => {},
                else => break,
            }
        }
    }

    fn process_phony(
        self: *MakeCSSA,
        inst_idx: ir.InstructionIdx,
        phony_idx: ir.PhonyIdx,
        bb_idx: ir.BasicBlockIdx,
    ) !void {
        // need to insert parallel copies
        const phony_data = self.base.compiler.get(ir.PhonyData, phony_idx);
        for (0..phony_data.data.len) |idx| {
            const pair = phony_data.data[idx];
            const new_reg = try self.insert_parallel_end(pair.label, pair.reg);
            phony_data.data[idx].reg = new_reg;
        }
        const new_phony = try self.base.compiler.insert_inst(bb_idx, .{ .phony = phony_idx }, 0);
        self.base.compiler.set(ir.Instruction, inst_idx, .{ .parallel_copy = new_phony });
    }

    fn insert_parallel_end(self: *MakeCSSA, to: ir.BasicBlockIdx, reg: ir.Reg) !ir.Reg {
        const bb = self.base.compiler.get(ir.BasicBlock, to);
        const src_inst = self.base.get(ir.Instruction, reg);
        var src_reg = reg;
        if (src_inst.is_constant()) {
            src_reg = try self.base.compiler.insert_inst(
                to,
                .{ .regify = reg },
                bb.instructions.items.len - 1,
            );
        }
        return self.base.compiler.insert_inst(
            to,
            .{ .parallel_copy = src_reg },
            bb.instructions.items.len - 1,
        );
    }
};

test "basic" {}
