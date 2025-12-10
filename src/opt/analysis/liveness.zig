const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");
const rev = @import("../../utils.zig").ReversedSlice;
const bit_set_move = @import("../../utils.zig").bit_set_move;
const Canonical = @import("../analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;

pub const LivenessAnalysis = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    canonical: Canonical,

    liveness_at: []BitSet,
    live_at: []BitSet,

    curr: BitSet,

    pub fn init(base: Base) !LivenessAnalysis {
        const inst_idx = base.compiler.stores.get_max_idx(ir.Instruction);
        return LivenessAnalysis{
            .base = base,
            .canonical = try Canonical.init(base),
            .liveness_at = try base.alloc.alloc(BitSet, inst_idx.get_usize()),
            .live_at = try base.alloc.alloc(BitSet, inst_idx.get_usize()),
            .curr = try BitSet.initEmpty(base.alloc, inst_idx.get_usize()),
        };
    }

    pub fn analyze(self: *LivenessAnalysis) !void {
        try self.canonical.analyze();
        const inst_count = self.liveness_at.len;
        for (self.liveness_at) |*live| {
            live.* = try BitSet.initEmpty(self.base.alloc, inst_count);
        }

        for (self.live_at) |*live| {
            live.* = try BitSet.initEmpty(self.base.alloc, inst_count);
        }

        var iter = self.base.compiler.stores.idx_iter(ir.Function);
        while (iter.next()) |function| {
            self.process_fn(function);
        }

        for (self.liveness_at, 0..) |inst_live, place_idx| {
            var bit_iter = inst_live.iterator(.{});
            while (bit_iter.next()) |inst_idx| {
                self.live_at[inst_idx].set(place_idx);
            }
        }
    }

    fn process_fn(self: *LivenessAnalysis, function: ir.FunctionIdx) void {
        const post_order = self.base.shared_data.get_postorder(function);
        while (true) {
            var change = false;
            for (post_order) |bb_idx| {
                change |= self.process_bb(bb_idx);
            }
            if (!change) {
                break;
            }
        }
    }

    fn process_bb(self: *LivenessAnalysis, bb_idx: ir.BasicBlockIdx) bool {
        var change = false;
        self.curr.unsetAll();
        var succ_iter = self.base.compiler.get_succesors(bb_idx);
        while (succ_iter.next()) |succ_idx| {
            const succ = self.base.compiler.get(ir.BasicBlock, succ_idx);
            const first = succ.instructions.items[0];
            const first_liveness = self.liveness_at[first.get_usize()];
            self.curr.setUnion(first_liveness);
        }

        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
        var inst_iter = rev(ir.InstructionIdx).init(bb.instructions.items);
        while (inst_iter.next()) |inst_idx| {
            // remove output
            const out_canon = self.canonical.find_canonical(inst_idx);
            self.curr.unset(out_canon.get_usize());

            // add inputs
            var iter = self.base.compiler.stores.get_reg_iter(inst_idx);
            while (iter.next()) |reg| {
                const canon_reg = self.canonical.find_canonical(reg);
                self.curr.set(canon_reg.get_usize());
            }
            change |= !self.curr.eql(self.liveness_at[inst_idx.get_usize()]);
            bit_set_move(&self.curr, &self.liveness_at[inst_idx.get_usize()]);
        }

        return change;
    }

    pub fn get_live_at(self: *const LivenessAnalysis, place: ir.InstructionIdx) BitSet {
        return self.liveness_at[place.get_usize()];
    }

    pub fn combine_liveness_to(self: *LivenessAnalysis, dst: ir.Reg, src: ir.Reg) void {
        const liveness_src = self.liveness_at[src.get_usize()];
        const live_at_src = self.live_at[src.get_usize()];
        self.liveness_at[dst.get_usize()].setUnion(liveness_src);
        self.live_at[dst.get_usize()].setUnion(live_at_src);
    }

    pub fn dump(self: *const LivenessAnalysis) !void {
        std.debug.print("live_at\n", .{});
        for (0.., self.live_at) |inst_idx, set| {
            if (set.count() == 0) {
                continue;
            }
            std.debug.print("  {}: ", .{inst_idx});
            var iter = set.iterator(.{});
            while (iter.next()) |reg| {
                std.debug.print("{} ", .{reg});
            }
            std.debug.print("\n", .{});
        }
    }
};
