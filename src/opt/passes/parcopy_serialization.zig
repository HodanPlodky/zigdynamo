const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const Canonical = @import("../analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;
const ir = @import("../ir.zig");
const rev = @import("../../utils.zig").ReversedSlice;

// this assumes we did not allocate 2**32 instruction/register
// which would better be correct otherwise we are fuck in
// other ways
comptime {
    std.debug.assert(@sizeOf(ir.Reg) == @sizeOf(u32));
}
const BOTTOM: ir.Reg = ir.Reg.new(std.math.maxInt(u32));
const TOP: ir.Reg = ir.Reg.new(std.math.maxInt(u32) - 1);

// TODO please do it
pub const SerializationPass = struct {
    base: Base,
    canon: Canonical,
    group: std.ArrayList(ir.InstructionIdx) = .{},
    group_positions: std.ArrayList(usize) = .{},

    reserved_reg: ir.Reg = undefined,

    todo: std.ArrayList(ir.InstructionIdx) = .{},
    ready: std.ArrayList(ir.InstructionIdx) = .{},
    loc: []ir.Reg,
    pred: []ir.Reg,

    pub fn init(base: Base, canon: Canonical) !SerializationPass {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        return SerializationPass{
            .base = base,
            .canon = canon,
            .loc = try base.alloc.alloc(ir.Reg, inst_count.get_usize() + 1),
            .pred = try base.alloc.alloc(ir.Reg, inst_count.get_usize() + 1),
        };
    }

    pub fn run(self: *SerializationPass) !void {
        // allocate instruction to represent fresh variable to use
        // in copy emit
        self.reserved_reg = try self.base.compiler.create_inst(.nop);
        for (self.base.compiler.stores.function.data.items) |function| {
            try self.process_fn(function);
        }
    }

    fn process_fn(self: *SerializationPass, function: ir.Function) !void {
        for (function.basicblocks.items) |bb_idx| {
            try self.process_bb(bb_idx);
        }
    }

    fn process_bb(self: *SerializationPass, bb_idx: ir.BasicBlockIdx) !void {
        try self.get_group_top(bb_idx);
        try self.process_group(bb_idx, Place.top);
        try self.get_group_bottom(bb_idx);
        try self.process_group(bb_idx, Place.bottom);
    }

    fn get_group_top(self: *SerializationPass, bb_idx: ir.BasicBlockIdx) !void {
        self.group.clearRetainingCapacity();
        self.group_positions.clearRetainingCapacity();

        // assumes only parallel_copy instructions are at the end
        // and if we hit other instruction its is no longer group
        // phonies should not be there already
        const bb = self.base.get(ir.BasicBlock, bb_idx);
        for (0.., bb.instructions.items) |pos, inst_idx| {
            const inst = self.base.get(ir.Instruction, inst_idx);
            switch (inst) {
                .parallel_copy => {
                    try self.group.append(self.base.alloc, inst_idx);
                    try self.group_positions.append(self.base.alloc, pos);
                },
                // ignore phony as it is not in the group
                else => break,
            }
        }
    }

    fn get_group_bottom(self: *SerializationPass, bb_idx: ir.BasicBlockIdx) !void {
        self.group.clearRetainingCapacity();
        self.group_positions.clearRetainingCapacity();

        // assumes only parallel_copy instructions are at the end
        // and if we hit other instruction its is no longer group
        const bb = self.base.get(ir.BasicBlock, bb_idx);
        var iter = rev(ir.InstructionIdx).init(bb.instructions.items);
        while (iter.next()) |inst_idx| {
            const inst = self.base.get(ir.Instruction, inst_idx);
            if (inst.is_terminator()) {
                continue;
            }
            switch (inst) {
                .parallel_copy => {
                    try self.group.append(self.base.alloc, inst_idx);
                    try self.group_positions.append(self.base.alloc, iter.current);
                },
                else => break,
            }
        }
    }

    const Place = enum {
        top,
        bottom,
    };

    /// uses the algorithm from https://inria.hal.science/inria-00349925
    /// for serializing parallel copy groups (Section III-C)
    fn process_group(self: *SerializationPass, bb_idx: ir.BasicBlockIdx, comptime place: Place) !void {
        const bb = self.base.compiler.stores.get_ptr(ir.BasicBlock, bb_idx);

        // remove parallel copies in the groub from basic blocks
        // so we can emit them after in order

        for (self.group_positions.items) |pos| {
            // could be done with remove many but
            // right now if you handle bottom group
            // you dont have correct order
            _ = bb.instructions.orderedRemove(pos);
        }

        // this will be direct translation of that
        // algorithm so it will a bit longwinded

        self.ready.clearRetainingCapacity();
        self.todo.clearRetainingCapacity();
        self.pred[self.reserved_reg.get_usize()] = BOTTOM;

        // this one thing is not in algo but I need to reset it
        // just in case
        @memset(self.loc, TOP);
        @memset(self.pred, TOP);

        var emit_count: usize = 0;

        // initialization
        for (self.group.items) |inst_idx| {
            const inst = self.base.get(ir.Instruction, inst_idx);
            std.debug.assert(std.meta.activeTag(inst) == .parallel_copy);
            const src = inst.parallel_copy;
            const dst = self.base.compiler.get_canonical_output(inst_idx);
            self.loc[dst.get_usize()] = BOTTOM;
            self.pred[src.get_usize()] = BOTTOM;
        }

        for (self.group.items) |inst_idx| {
            const inst = self.base.get(ir.Instruction, inst_idx);
            const src = inst.parallel_copy;
            const dst = self.base.compiler.get_canonical_output(inst_idx);

            self.loc[src.get_usize()] = src;
            self.pred[dst.get_usize()] = src;
            try self.todo.append(self.base.alloc, dst);
        }

        for (self.group.items) |inst_idx| {
            const dst = self.base.compiler.get_canonical_output(inst_idx);
            if (self.loc[dst.get_usize()].eql(BOTTOM)) {
                try self.ready.append(self.base.alloc, dst);
            }
        }

        while (self.todo.items.len > 0) {
            while (self.ready.items.len > 0) {
                const b = self.ready.pop().?;
                const a = self.pred[b.get_usize()];
                const c = self.loc[a.get_usize()];
                try self.emit_copy(bb_idx, place, b, c, &emit_count);
                self.loc[a.get_usize()] = b;
                if (a.eql(c) and !self.pred[a.get_usize()].eql(BOTTOM)) {
                    // just popped so I can assume there is enough
                    // capacity
                    self.ready.appendAssumeCapacity(a);
                }
            }

            const b = self.todo.pop().?;
            if (b.eql(self.loc[b.get_usize()])) {
                try self.emit_copy(bb_idx, place, self.reserved_reg, b, &emit_count);
                self.loc[b.get_usize()] = self.reserved_reg;
                try self.ready.append(self.base.alloc, b);
            }
        }
    }

    fn emit_copy(
        self: *SerializationPass,
        bb_idx: ir.BasicBlockIdx,
        comptime place: Place,
        dst: ir.Reg,
        src: ir.Reg,
        count: *usize,
    ) !void {
        const bb = self.base.get(ir.BasicBlock, bb_idx);
        const copy_idx = try self.base.compiler.create_with(
            ir.CopyData,
            .{ .dst = dst, .src = src },
        );

        switch (place) {
            .top => {
                _ = try self.base.compiler.insert_inst(bb_idx, .{ .copy = copy_idx }, count.*);
            },
            .bottom => {
                const second_to_last = bb.instructions.items.len - 1;
                _ = try self.base.compiler.insert_inst(bb_idx, .{ .copy = copy_idx }, second_to_last);
            },
        }
        count.* += 1;
    }
};
