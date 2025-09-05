const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const ir = @import("../ir.zig");

pub const MovElim = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,

    // allocated once
    visited: BitSet,

    pub fn init(base: Base) !MovElim {
        const bb_count = base.compiler.stores.get_max_idx(ir.BasicBlock);
        return MovElim{
            .base = base,
            .visited = try BitSet.initEmpty(base.alloc, bb_count.index),
        };
    }

    pub fn run(self: *MovElim) !void {
        for (self.base.compiler.stores.function.data.items) |function| {
            self.process_function(function);
        }
    }

    fn process_function(self: *MovElim, function: ir.Function) void {
        for (function.basicblocks.items) |bb| {
            self.process_bb(bb);
        }
    }

    fn process_bb(self: *MovElim, bb_idx: ir.BasicBlockIdx) void {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.compiler.get(ir.Instruction, inst_idx);
            switch (inst) {
                .mov => {},
                // could be made better
                .phony => {},
                else => self.process_inst(inst_idx),
            }
        }
    }

    fn process_inst(self: *MovElim, inst_idx: ir.InstructionIdx) void {
        var regs_iter = self.base.compiler.stores.get_reg_iter_ptr(inst_idx);
        while (regs_iter.next()) |reg| {
            const orig = self.get_origin(reg.*);
            reg.* = orig;
        }
    }

    fn get_origin(self: *MovElim, reg: ir.Reg) ir.Reg {
        var res = reg;
        while (true) {
            const inst = self.base.compiler.get(ir.Instruction, res);
            switch (inst) {
                .mov => |mov_reg| {
                    res = mov_reg;
                },
                else => return res,
            }
        }
    }
};
