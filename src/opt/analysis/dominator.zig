const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");
const bit_set_move = @import("../../utils.zig").bit_set_move;

pub const DominatorAnalysis = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    dominators: []BitSet,
    frontiers: []BitSet,
    idoms: []?ir.BasicBlockIdx,
    post_order: std.ArrayListUnmanaged(ir.BasicBlockIdx),

    // temp set to not allocate
    // when it is not necessary
    temp_set: BitSet,

    pub fn init(base: Base) !DominatorAnalysis {
        const bb_count = base.compiler.stores.get_max_idx(ir.BasicBlock).index;
        const res = DominatorAnalysis{
            .base = base,
            .dominators = try base.alloc.alloc(BitSet, bb_count),
            .frontiers = try base.alloc.alloc(BitSet, bb_count),
            .idoms = try base.alloc.alloc(?ir.BasicBlockIdx, bb_count),
            .post_order = try std.ArrayListUnmanaged(ir.BasicBlockIdx).initCapacity(base.alloc, bb_count),
            .temp_set = try BitSet.initFull(base.alloc, bb_count),
        };

        return res;
    }

    pub fn analyze(self: *DominatorAnalysis) !void {
        const bb_count = self.base.compiler.stores.get_max_idx(ir.BasicBlock).index;
        for (self.dominators) |*doms| {
            doms.* = try BitSet.initFull(self.base.alloc, bb_count);
        }
        const function = self.base.compiler.get_const_ptr(ir.Function, self.base.function_idx);
        var visited = try BitSet.initEmpty(self.base.alloc, bb_count);

        // get post order of basicblocks
        self.dfs(&visited, function.entry);

        // compute dominators
        self.compute_doms();

        self.compute_immediate();

        try self.compute_frontier();
    }

    fn dfs(self: *DominatorAnalysis, visited: *BitSet, basicblock_idx: ir.BasicBlockIdx) void {
        if (visited.isSet(basicblock_idx.index)) {
            return;
        }

        visited.set(basicblock_idx.index);

        var succesors = self.base.compiler.get_succesors(basicblock_idx);
        while (succesors.next()) |succ| {
            self.dfs(visited, succ);
        }

        // should be allocated from the init
        self.post_order.appendAssumeCapacity(basicblock_idx);
    }

    fn compute_doms(self: *DominatorAnalysis) void {
        while (true) {
            var change = false;

            var index: usize = self.post_order.items.len;
            while (index > 0) {
                index -= 1;
                const bb_idx = self.post_order.items[index];
                const bb = self.base.compiler.get_const_ptr(ir.BasicBlock, bb_idx);

                if (bb.predecessors.items.len > 0) {
                    self.temp_set.setAll();
                    for (bb.predecessors.items) |pred| {
                        const pred_dom_set = self.dominators[pred.index];
                        self.temp_set.setIntersection(pred_dom_set);
                    }
                    self.temp_set.set(bb_idx.index);
                } else {
                    self.temp_set.unsetAll();
                    self.temp_set.set(bb_idx.index);
                }

                const orig_set = &self.dominators[bb_idx.index];
                if (!self.temp_set.eql(orig_set.*)) {
                    bit_set_move(&self.temp_set, orig_set);
                    change = true;
                }
            }

            if (!change) {
                break;
            }
        }
    }

    fn compute_immediate(self: *DominatorAnalysis) void {
        for (self.post_order.items, 1..) |bb_idx, idx| {
            const bb_doms = self.dominators[bb_idx.index];
            var idom: ?ir.BasicBlockIdx = null;
            for (self.post_order.items[idx..]) |before| {
                if (bb_doms.isSet(before.index)) {
                    idom = before;
                }
            }

            self.idoms[bb_idx.index] = idom;
        }
    }

    fn compute_frontier(self: *DominatorAnalysis) !void {
        const bb_count = self.base.compiler.stores.get_max_idx(ir.BasicBlock).index;
        for (self.post_order.items) |bb_idx| {
            self.frontiers[bb_idx.index] = try BitSet.initEmpty(self.base.alloc, bb_count);
        }

        for (self.post_order.items) |bb_idx| {
            const bb = self.base.compiler.get_const_ptr(ir.BasicBlock, bb_idx);

            if (bb.predecessors.items.len >= 2) {
                std.debug.assert(self.idoms[bb_idx.index] != null);
                for (bb.predecessors.items) |pred_idx| {
                    var runner_idx = pred_idx;
                    while (runner_idx.index != self.idoms[bb_idx.index].?.index) {
                        self.frontiers[runner_idx.index].set(bb_idx.index);
                        runner_idx = self.idoms[runner_idx.index].?;
                    }
                }
            }
        }
    }
};

test "basic" {
    const Compiler = @import("../compile.zig").Compiler;

    // this is how to use it
    var permanent_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer permanent_arena.deinit();
    var scratch_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch_arena.deinit();

    const permanent_alloc = permanent_arena.allocator();
    const scratch_alloc = scratch_arena.allocator();

    const globals = try std.testing.allocator.alloc([]const u8, 0);
    defer std.testing.allocator.free(globals);
    var compiler = try Compiler.init(globals, permanent_alloc, scratch_alloc);
    const fn_idx = try compiler.create_empty();

    // basic blocks
    const entry_idx = compiler.current;
    const true_idx = try compiler.append_basicblock();
    const false_idx = try compiler.append_basicblock();
    const join_idx = try compiler.append_basicblock();

    const cond = try compiler.append_inst(ir.Instruction.true);

    const branch_idx = try compiler.create_with(ir.BranchData, .{
        .cond = cond,
        .true_branch = true_idx,
        .false_branch = false_idx,
    });
    try compiler.append_terminator(ir.Instruction{ .branch = branch_idx });

    compiler.set_basicblock(true_idx);
    try compiler.append_terminator(ir.Instruction{ .jmp = join_idx });
    compiler.set_basicblock(false_idx);
    try compiler.append_terminator(ir.Instruction{ .jmp = join_idx });

    compiler.set_basicblock(join_idx);
    try compiler.append_terminator(ir.Instruction{ .ret = cond });

    const base = Base{ .alloc = scratch_alloc, .compiler = &compiler, .function_idx = fn_idx };
    var dom = try DominatorAnalysis.init(base);
    try dom.analyze();

    // dominators
    const entry_res: [1]ir.BasicBlockIdx = .{entry_idx};
    const true_res: [2]ir.BasicBlockIdx = .{ entry_idx, true_idx };
    const false_res: [2]ir.BasicBlockIdx = .{ entry_idx, false_idx };
    const join_res: [2]ir.BasicBlockIdx = .{ entry_idx, join_idx };

    var dom_res = dom.dominators[entry_idx.index];
    for (entry_res) |item| {
        dom_res.toggle(item.index);
    }
    try std.testing.expectEqual(dom_res.findFirstSet(), null);

    dom_res = dom.dominators[true_idx.index];
    for (true_res) |item| {
        dom_res.toggle(item.index);
    }
    try std.testing.expectEqual(dom_res.findFirstSet(), null);

    dom_res = dom.dominators[false_idx.index];
    for (false_res) |item| {
        dom_res.toggle(item.index);
    }
    try std.testing.expectEqual(dom_res.findFirstSet(), null);

    dom_res = dom.dominators[join_idx.index];
    for (join_res) |item| {
        dom_res.toggle(item.index);
    }
    try std.testing.expectEqual(dom_res.findFirstSet(), null);

    // immediate
    try std.testing.expectEqual(dom.idoms[entry_idx.index], null);
    try std.testing.expectEqual(dom.idoms[true_idx.index], entry_idx);
    try std.testing.expectEqual(dom.idoms[false_idx.index], entry_idx);
    try std.testing.expectEqual(dom.idoms[join_idx.index], entry_idx);

    // frontier
    var tmp = try std.DynamicBitSetUnmanaged.initEmpty(scratch_alloc, 4);
    tmp.set(join_idx.index);
    try std.testing.expectEqual(dom.frontiers[entry_idx.index].findFirstSet(), null);
    try std.testing.expect(dom.frontiers[true_idx.index].eql(tmp));
    try std.testing.expect(dom.frontiers[false_idx.index].eql(tmp));
    try std.testing.expectEqual(dom.frontiers[join_idx.index].findFirstSet(), null);
}
