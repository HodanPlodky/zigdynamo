const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");
const rev = @import("../../utils.zig").ReversedSlice;
const dump_set = @import("../../utils.zig").dump_set;
const bit_set_move = @import("../../utils.zig").bit_set_move;
const Canonical = @import("../analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;

pub const LivenessAnalysis = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    canonical: []ir.Reg,

    liveness_at: []BitSet,
    out_live: []BitSet,
    live_at: []BitSet,

    curr: BitSet,

    pub fn init(base: Base, canonical: []ir.Reg) !LivenessAnalysis {
        const inst_idx = base.compiler.stores.get_max_idx(ir.Instruction);
        const bb_idx = base.compiler.stores.get_max_idx(ir.BasicBlock);
        return LivenessAnalysis{
            .base = base,
            .canonical = canonical,
            .liveness_at = try base.alloc.alloc(BitSet, inst_idx.get_usize()),
            .out_live = try base.alloc.alloc(BitSet, bb_idx.get_usize()),
            .live_at = try base.alloc.alloc(BitSet, inst_idx.get_usize()),
            .curr = try BitSet.initEmpty(base.alloc, inst_idx.get_usize()),
        };
    }

    pub fn analyze(self: *LivenessAnalysis) !void {
        const inst_count = self.liveness_at.len;
        for (self.liveness_at) |*live| {
            live.* = try BitSet.initEmpty(self.base.alloc, inst_count);
        }

        for (self.live_at) |*live| {
            live.* = try BitSet.initEmpty(self.base.alloc, inst_count);
        }

        for (self.out_live) |*live| {
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
            const first_liveness = self.out_live[succ_idx.get_usize()];
            self.curr.setUnion(first_liveness);
        }

        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
        var inst_iter = rev(ir.InstructionIdx).init(bb.instructions.items);
        while (inst_iter.next()) |inst_idx| {
            change |= !self.curr.eql(self.liveness_at[inst_idx.get_usize()]);
            bit_set_move(&self.curr, &self.liveness_at[inst_idx.get_usize()]);

            const inst = self.base.get(ir.Instruction, inst_idx);
            // remove output
            if (inst.has_output()) {
                const out_canon = self.get_output(inst_idx);
                self.curr.unset(out_canon.get_usize());
            }

            // add inputs
            var iter = self.base.compiler.stores.get_reg_iter(inst_idx);
            while (iter.next()) |reg| {
                const canon_reg = self.get_canon(reg);
                self.curr.set(canon_reg.get_usize());
            }
        }

        bit_set_move(&self.curr, &self.out_live[bb_idx.get_usize()]);

        return change;
    }

    pub fn get_liveness_at(self: *const LivenessAnalysis, place: ir.InstructionIdx) BitSet {
        return self.liveness_at[place.get_usize()];
    }

    fn get_output(self: *const LivenessAnalysis, inst_idx: ir.InstructionIdx) ir.Reg {
        const inst = self.base.get(ir.Instruction, inst_idx);
        return switch (inst) {
            .copy => |copy_idx| {
                const copy = self.base.get(ir.CopyData, copy_idx);
                return copy.dst;
            },
            else => self.get_canon(inst_idx),
        };
    }

    fn get_canon(self: *const LivenessAnalysis, reg: ir.Reg) ir.Reg {
        return self.canonical[reg.get_usize()];
    }

    pub fn is_live_at(self: *LivenessAnalysis, place: ir.InstructionIdx, reg: ir.Reg) bool {
        const liveness = self.get_liveness_at(place);
        const canon = self.get_canon(reg);
        return liveness.isSet(canon.get_usize());
    }

    pub fn liveness_overlaps(self: *LivenessAnalysis, a: ir.Reg, b: ir.Reg) bool {
        const a_liveness = self.live_at[a.get_usize()];
        const b_liveness = self.live_at[b.get_usize()];

        // used curr as temporaly set
        bit_set_move(&a_liveness, &self.curr);
        self.curr.setIntersection(b_liveness);
        return self.curr.count() != 0;
    }

    pub fn combine_liveness_to(self: *LivenessAnalysis, dst: ir.Reg, src: ir.Reg) void {
        const live_at_src = self.live_at[src.get_usize()];
        self.live_at[dst.get_usize()].setUnion(live_at_src);

        var iter = self.live_at[dst.get_usize()].iterator(.{});
        while (iter.next()) |place| {
            self.liveness_at[place].set(dst.get_usize());
        }
    }

    pub fn combine_regs(self: *LivenessAnalysis, dst: ir.Reg, src: ir.Reg) void {
        self.combine_liveness_to(dst, src);
        self.combine_liveness_to(src, dst);
    }

    pub fn dump(self: *const LivenessAnalysis) !void {
        std.debug.print("live at\n", .{});
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

        std.debug.print("\nliveness at\n", .{});
        for (0.., self.liveness_at) |inst_idx, set| {
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

    pub fn dump_liveset(self: *const LivenessAnalysis, place: ir.InstructionIdx) !void {
        const set = self.liveness_at[place.get_usize()];
        var iter = set.iterator(.{});
        while (iter.next()) |reg| {
            std.debug.print("{} ", .{reg});
        }
        std.debug.print("\n", .{});
    }
};
