const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const ir = @import("../ir.zig");

pub const UnusedElim = struct {
    const BitSet = std.DynamicBitSetUnmanaged;
    base: Base,

    pub fn init(base: Base) !UnusedElim {
        return UnusedElim{
            .base = base,
        };
    }

    pub fn run(self: *UnusedElim) !void {
        const inst_count = self.base.compiler.stores.get_max_idx(ir.Instruction);
        var used = try BitSet.initEmpty(self.base.alloc, inst_count.index);

        while (self.do_run(&used)) {
            used.unsetAll();
        }
    }

    fn do_run(self: *UnusedElim, used: *BitSet) bool {
        var change = false;
        for (self.base.compiler.stores.basicblock.data.items) |bb| {
            for (bb.instructions.items) |inst_idx| {
                var iter = self.base.compiler.stores.get_reg_iter(inst_idx);
                while (iter.next()) |reg| {
                    used.set(reg.index);
                }
            }
        }
        for (self.base.compiler.stores.basicblock.data.items) |*bb| {
            var index = bb.instructions.items.len;
            // we go backwards
            while (index > 0) {
                index -= 1;
                const inst_idx = bb.instructions.items[index];
                if (self.cannot_remove(inst_idx)) {
                    continue;
                }
                if (!used.isSet(inst_idx.index)) {
                    _ = bb.instructions.orderedRemove(index);
                    change = true;
                }
            }
        }
        return change;
    }

    fn cannot_remove(self: *const UnusedElim, inst_idx: ir.InstructionIdx) bool {
        const inst = self.base.compiler.get(ir.Instruction, inst_idx);
        if (inst.is_terminator()) {
            return true;
        }
        return false;
    }
};
