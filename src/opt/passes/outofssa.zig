const std = @import("std");
const ir = @import("../ir.zig");
const Base = @import("pass_base.zig").PassBase;
const Canonical = @import("../analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;

/// Sets canonical name for registers that are connected with phony
/// it requires to be in CSSA
pub const OutOfSSAPass = struct {
    base: Base,

    // allocate this just to have it for output
    // of compiler
    canonical_regs: []ir.Reg,
    canonical_analysis: Canonical,

    pub fn init(base: Base, perma_alloc: std.mem.Allocator, canon: Canonical) !OutOfSSAPass {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        return OutOfSSAPass{
            .base = base,
            .canonical_regs = try perma_alloc.alloc(ir.Reg, inst_count.get_usize()),
            .canonical_analysis = canon,
        };
    }

    pub fn run(self: *OutOfSSAPass) !void {
        // store result for output since the result from analysis
        // is stored in the scratch allocator

        var inst_iter = self.base.compiler.stores.idx_iter(ir.Instruction);
        // replace regs for canonical regs
        inst_iter.reset();
        while (inst_iter.next()) |inst_idx| {
            var reg_iter = self.base.compiler.stores.get_reg_iter_ptr(inst_idx);
            while (reg_iter.next()) |reg_ptr| {
                const canonical = self.canonical_analysis.find_canonical(reg_ptr.*);
                reg_ptr.* = canonical;
            }
        }
        @memcpy(self.canonical_regs, self.canonical_analysis.canonical_regs);

        // remove phonies
        for (self.base.compiler.stores.basicblock.data.items) |*bb| {
            var index = bb.instructions.items.len;
            // we go backwards
            while (index > 0) {
                index -= 1;
                const inst_idx = bb.instructions.items[index];
                const inst = self.base.get(ir.Instruction, inst_idx);
                switch (inst) {
                    .phony => {
                        _ = bb.instructions.orderedRemove(index);
                    },
                    else => {},
                }
            }
        }
    }
};
