const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const ir = @import("../ir.zig");

pub const MovElim = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,

    origins: []usize,

    // allocated once
    visited: BitSet,

    pub fn init(base: Base) !MovElim {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        const bb_count = base.compiler.stores.get_max_idx(ir.BasicBlock);
        return MovElim{
            .base = base,
            .origins = try base.alloc.alloc(usize, inst_count.index),
            .visited = try BitSet.initEmpty(base.alloc, bb_count.index),
        };
    }

    pub fn run(self: *MovElim) !void {
        for (self.base.compiler.stores.function.data.items) |function| {
            self.process_bb(function.entry);
        }
    }

    fn process_function(self: *MovElim, function: ir.Function) void {
        self.visited.unsetAll();
        self.process_bb(function.entry);
    }

    fn process_bb(self: *MovElim, bb_idx: ir.BasicBlockIdx) void {
        _ = self;
        _ = bb_idx;
    }
};
