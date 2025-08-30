const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const DominantorAnalysis = @import("../analysis/dominator.zig").DominatorAnalysis;
const ir = @import("../ir.zig");

pub const MakeSSA = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    dom: DominantorAnalysis,

    local_count: u32,
    counters: []u32,
    stacks: []std.ArrayListUnmanaged(ir.Reg),
    stack_checkpoints: []usize,

    pub fn init(base: Base) !MakeSSA {
        const local_count = base.compiler.locals.curr_idx;
        return MakeSSA{
            .base = base,
            .local_count = local_count,
            .counters = try base.alloc.alloc(u32, local_count),
            .stacks = try base.alloc.alloc(std.ArrayListUnmanaged(ir.Reg), local_count),
            .stack_checkpoints = try base.alloc.alloc(usize, local_count),
            .dom = try DominantorAnalysis.init(base.analysis_base),
        };
    }

    pub fn run(self: *MakeSSA) !void {
        try self.dom.analyze();
        const places = try self.base.alloc.alloc(BitSet, @intCast(self.local_count));
        const bb_count = self.base.compiler.stores.get_max_idx(ir.BasicBlock);

        for (places) |*local| {
            local.* = try BitSet.initEmpty(self.base.alloc, bb_count.index);
        }

        try self.place_phonys(places);

        @memset(self.counters, 0);

        // TODO better starter bound
        for (self.stacks) |*stack| {
            stack.* = try std.ArrayListUnmanaged(ir.Reg).initCapacity(self.base.alloc, @intCast(bb_count.index));
        }

        var visited = try BitSet.initEmpty(self.base.alloc, bb_count.index);
        try self.search(self.base.get_entry().entry, &visited);
    }

    fn place_phonys(self: *MakeSSA, places: []BitSet) !void {
        for (
            self.base.compiler.stores.set_local.data.items(.basicblock_idx),
            self.base.compiler.stores.set_local.data.items(.local_idx),
        ) |bb_idx, local_idx| {
            var iter = self.dom.frontiers[bb_idx.index].iterator(.{});
            while (iter.next()) |front_bb_idx| {
                places[@intCast(local_idx)].set(front_bb_idx);
            }
        }

        for (places, 0..) |local, local_idx| {
            var iter = local.iterator(.{});
            const tmp_reg = ir.Reg.new(@intCast(local_idx));
            while (iter.next()) |idx| {
                const bb_idx = ir.BasicBlockIdx.new(@intCast(idx));

                const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
                // should hold we will see in future
                std.debug.assert(bb.predecessors.items.len == 2);

                const phony = try self.base.compiler.create_phony(
                    bb.predecessors.items[0],
                    tmp_reg,
                    bb.predecessors.items[1],
                    tmp_reg,
                    @intCast(local_idx),
                );
                _ = try self.base.compiler.insert_inst(bb_idx, phony, 0);
            }
        }
    }

    fn search(self: *MakeSSA, bb_idx: ir.BasicBlockIdx, visited: *BitSet) !void {
        // TODO this could be removed later but for now it is sanity check
        std.debug.assert(!visited.isSet(bb_idx.index));
        visited.set(bb_idx.index);

        for (0..self.local_count) |local_idx| {
            self.stack_checkpoints[local_idx] = self.stacks[local_idx].items.len;
        }

        const bb = self.base.compiler.get_const_ptr(ir.BasicBlock, bb_idx);

        var append_count: usize = 0;
        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.compiler.get(ir.Instruction, inst_idx);
            switch (inst) {
                .phony => |phony_data_idx| {
                    const phony_data = self.base.compiler.get(ir.PhonyData, phony_data_idx);

                    // other wise what the hell is this phony inst
                    std.debug.assert(phony_data.data.len >= 2);
                    if (phony_data.origin == null) {
                        // it is already complete phi
                        continue;
                    }
                    const local_idx: usize = @intCast(phony_data.origin.?);

                    try self.stacks[local_idx].append(self.base.alloc, inst_idx);
                    append_count += 1;
                },
                .set_local => |set_local_idx| {
                    const set_local_data = self.base.compiler.get(ir.SetLocalData, set_local_idx);
                    const local_idx: usize = @intCast(set_local_data.local_idx);
                    try self.stacks[local_idx].append(self.base.alloc, set_local_data.value);
                    self.base.compiler.set(ir.Instruction, inst_idx, .{ .mov = set_local_data.value });
                    append_count += 1;
                },
                .get_local => |local_idx| {
                    const curr = self.stacks[@intCast(local_idx)].getLast();
                    self.base.compiler.set(ir.Instruction, inst_idx, .{ .mov = curr });
                },
                else => {},
            }
        }

        var iter = self.base.compiler.get_succesors(bb_idx);
        while (iter.next()) |succ| {
            for (self.stacks, 0..) |stack, local_idx| {
                if (stack.items.len == 0) {
                    // the local is not used yet
                    continue;
                }
                const reg = stack.getLast();
                self.fix_phonys(succ, @intCast(local_idx), bb_idx, reg);
            }
        }

        for (self.dom.domtree_edges[@intCast(bb_idx.index)].items) |dom_succ| {
            std.debug.assert(self.dom.idoms[@intCast(dom_succ.index)].?.index == bb_idx.index);
            try self.search(dom_succ, visited);
        }

        // pop stacks
        for (0..self.local_count) |local_idx| {
            // should always be same or lower so no alloc and not errors
            self.stacks[local_idx].resize(self.base.alloc, self.stack_checkpoints[local_idx]) catch unreachable;
        }
    }

    fn fix_phonys(
        self: *MakeSSA,
        bb_idx: ir.BasicBlockIdx,
        origin_local: u32,
        origin_bb_idx: ir.BasicBlockIdx,
        new_reg: ir.Reg,
    ) void {
        const bb = self.base.compiler.get_const_ptr(ir.BasicBlock, bb_idx);
        for (bb.instructions.items) |inst_idx| {
            const inst = self.base.compiler.get(ir.Instruction, inst_idx);
            switch (inst) {
                .phony => |phony_idx| {
                    const phony_data = self.base.compiler.get(ir.PhonyData, phony_idx);
                    if (phony_data.origin == null or phony_data.origin.? != origin_local) {
                        // not phi we are looking for
                        continue;
                    }

                    for (phony_data.data) |*item| {
                        if (item.label.index != origin_bb_idx.index) {
                            continue;
                        }

                        item.reg = new_reg;
                    }
                },
                else => {},
            }
        }
    }
};
