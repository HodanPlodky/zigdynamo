const std = @import("std");
const ir = @import("../ir.zig");
const Base = @import("pass_base.zig").PassBase;
const LivenessAnalysis = @import("../analysis/liveness.zig").LivenessAnalysis;
const ValueAnalysis = @import("../analysis/value_analysis.zig").ValueAnalysis;
const DominatorAnalysis = @import("../analysis/dominator.zig").DominatorAnalysis;
const Canonical = @import("../analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;

pub const CopyElimination = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    liveness: LivenessAnalysis,
    canon: Canonical,
    values: ValueAnalysis,
    dom: DominatorAnalysis,

    /// register that should be considered as holding this
    /// perticular value
    canonical_regs: []ir.Reg,
    to_remove: BitSet,

    pub fn init(base: Base, canon: Canonical) !CopyElimination {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        return CopyElimination{
            .base = base,
            .liveness = try LivenessAnalysis.init(base.analysis_base, canon.canonical_regs),
            .canon = canon,
            .values = try ValueAnalysis.init(base.analysis_base),
            .dom = try DominatorAnalysis.init(base.analysis_base),
            .canonical_regs = try base.alloc.alloc(ir.Reg, inst_count.get_usize()),
            .to_remove = try BitSet.initEmpty(base.alloc, inst_count.get_usize()),
        };
    }

    pub fn run(self: *CopyElimination) !void {
        try self.liveness.analyze();
        try self.values.analyze();
        try self.dom.analyze();

        // init canonicals since there is a possibility to fuck up unreachable
        // instructions
        var inst_iter = self.base.compiler.stores.idx_iter(ir.Instruction);
        while (inst_iter.next()) |inst_idx| {
            self.canonical_regs[inst_idx.get_usize()] = inst_idx;
        }

        for (self.base.compiler.stores.function.data.items) |function| {
            self.process_fn(function);
        }
        self.fix_insts_regs();
    }

    fn process_fn(self: *CopyElimination, function: ir.Function) void {
        self.process_bb(function.entry);
    }

    // recursion is done on tree so it should be good to
    // not check for cycles
    fn process_bb(self: *CopyElimination, bb_idx: ir.BasicBlockIdx) void {
        const bb = self.base.get(ir.BasicBlock, bb_idx);
        for (bb.instructions.items) |inst_idx| {
            self.process_inst(inst_idx);
        }

        for (self.dom.domtree_edges[bb_idx.get_usize()].items) |succ| {
            self.process_bb(succ);
        }
    }

    fn process_inst(self: *CopyElimination, inst_idx: ir.InstructionIdx) void {
        self.canonical_regs[inst_idx.get_usize()] = inst_idx;
        if (self.get_copy_data(inst_idx)) |data| {
            const src_canon = self.canon.find_canonical(data.src);
            const dst_canon = self.canon.find_canonical(data.dst);
            const src_val = self.values.get(src_canon);
            const dst_val = self.values.get(dst_canon);

            // if they either have a same value of their liveness set are distincted
            // then you can technically have them in same register so it is ok to
            // remove this copy in this case set cannonical to same reg and mark it to remove
            //const dst_is_live_at_src = self.liveness.is_live_at(data.src, dst_canon);
            //const src_is_live_at_dst = self.liveness.is_live_at(data.dst, src_canon);
            const liveness_overlaps = self.liveness.liveness_overlaps(dst_canon, src_canon);

            if (src_val.eql(dst_val) or !liveness_overlaps) {
                self.to_remove.set(inst_idx.get_usize());
                self.canon.union_regs(data.dst, data.src);
                self.liveness.combine_regs(src_canon, dst_canon);
                self.canonical_regs[data.dst.get_usize()] = data.src;

                // tmp
                self.fix_insts_regs();
            }
        }
    }

    fn fix_insts_regs(self: *CopyElimination) void {
        var inst_iter = self.base.compiler.stores.idx_iter(ir.Instruction);
        while (inst_iter.next()) |inst_idx| {
            var reg_iter = self.base.compiler.stores.get_reg_iter_ptr(inst_idx);
            while (reg_iter.next()) |reg| {
                reg.* = self.canonical_regs[reg.get_usize()];
            }
        }
    }

    fn get_copy_data(self: *const CopyElimination, inst_idx: ir.InstructionIdx) ?struct { src: ir.Reg, dst: ir.Reg } {
        const inst = self.base.get(ir.Instruction, inst_idx);
        return switch (inst) {
            .mov, .parallel_copy => |reg| .{ .src = reg, .dst = inst_idx },
            else => null,
        };
    }
};
