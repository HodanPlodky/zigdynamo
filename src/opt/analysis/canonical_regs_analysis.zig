const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");

/// Get canonical name for register
pub const CanonicalRegsAnalysis = struct {
    base: Base,
    canonical_regs: []ir.Reg,

    pub fn init(base: Base) !CanonicalRegsAnalysis {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);

        return .{
            .base = base,
            .canonical_regs = try base.alloc.alloc(ir.Reg, inst_count.get_usize()),
        };
    }

    pub fn analyze(self: *CanonicalRegsAnalysis) void {
        var inst_iter = self.base.compiler.stores.idx_iter(ir.Instruction);
        while (inst_iter.next()) |inst_idx| {
            self.canonical_regs[inst_idx.get_usize()] = inst_idx;
        }

        var function_iter = self.base.compiler.stores.idx_iter(ir.Function);
        while (function_iter.next()) |fn_idx| {
            self.process_fn(fn_idx);
        }
    }

    fn process_fn(self: *CanonicalRegsAnalysis, fn_idx: ir.FunctionIdx) void {
        const function = self.base.get(ir.Function, fn_idx);
        for (function.basicblocks.items) |bb_idx| {
            self.process_bb(bb_idx);
        }
    }

    fn process_bb(self: *CanonicalRegsAnalysis, bb_idx: ir.BasicBlockIdx) void {
        const bb = self.base.get(ir.BasicBlock, bb_idx);
        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.get(ir.Instruction, inst_idx);
            switch (inst) {
                .phony => |phony_idx| {
                    const data = self.base.get(ir.PhonyData, phony_idx);
                    for (data.data) |pair| {
                        self.union_regs(inst_idx, pair.reg);
                    }
                },
                else => {},
            }
        }
    }

    pub fn find_canonical(self: *CanonicalRegsAnalysis, reg: ir.Reg) ir.Reg {
        const idx = reg.get_usize();
        const current = self.canonical_regs[idx];
        if (!current.eql(reg)) {
            self.canonical_regs[idx] = self.find_canonical(current);
            return self.canonical_regs[idx];
        }
        return reg;
    }

    pub fn union_regs(self: *CanonicalRegsAnalysis, left: ir.Reg, right: ir.Reg) void {
        const canon_left = self.find_canonical(left);
        const canon_right = self.find_canonical(right);

        if (!canon_left.eql(canon_right)) {
            // arbitrary choice and could be better for perfomance but oh well
            // either way the order I call it it should be good
            self.canonical_regs[canon_right.get_usize()] = canon_left;
        }
    }
};
