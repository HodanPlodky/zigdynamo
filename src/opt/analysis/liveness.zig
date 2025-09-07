const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");
const rev = @import("../../utils.zig").ReversedSlice;

// not implemented
// since I think I wont need it 
// in near future
pub const LivenessAnalysis = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    liveness_at: []BitSet,

    pub fn init(base: Base) !LivenessAnalysis {
        const inst_idx = base.compiler.stores.get_max_idx(ir.Instruction);
        return LivenessAnalysis{
            .base = base,
            .liveness_at = try base.alloc.alloc(BitSet, inst_idx.get_usize()),
        };
    }

    pub fn run(self: *LivenessAnalysis) !void {
        const inst_count = self.liveness_at.len;
        for (self.liveness_at) |*live| {
            live.* = try BitSet.initEmpty(self.base.alloc, inst_count);
        }

        var iter = self.base.compiler.stores.idx_iter(ir.Function);
        while (iter.next()) |function| {
            self.process_fn(function);
        }
    }

    fn process_fn(self: *LivenessAnalysis, function: ir.FunctionIdx) void {
        while (true) {
            var change = false;
            for (self.base.shared_data.post_orders[function.get_usize()]) |bb_idx| {
                change |= self.process_bb(bb_idx);
            }
        }
    }

    fn process_bb(self: *LivenessAnalysis, bb_idx: ir.BasicBlockIdx) bool {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
        var inst_iter = rev(ir.InstructionIdx).init(bb.instructions);
        while (inst_iter.next()) |inst_idx| {
            _ = inst_idx; 
        }

        return false;
    }

    pub fn get_live_at(self: *const LivenessAnalysis, place: ir.InstructionIdx) BitSet {
        return self.liveness_at[place.get_usize()];
    }
};

