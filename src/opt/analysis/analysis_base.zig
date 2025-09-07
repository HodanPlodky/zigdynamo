const Compiler = @import("../compile.zig").Compiler;
const std = @import("std");
const ir = @import("../ir.zig");

pub const AnalysisBase = struct {
    compiler: *const Compiler,
    alloc: std.mem.Allocator,
    shared_data: SharedData,
};

/// Data that would be used in a lot of the
/// analysis/passes, passes should handle any
/// invalidations
pub const SharedData = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    post_orders: []std.ArrayListUnmanaged(ir.BasicBlockIdx),
    emit_orders: []std.ArrayListUnmanaged(ir.BasicBlockIdx),
    visited_bb: BitSet,

    pub fn init(compiler: *const Compiler, alloc: std.mem.Allocator) !SharedData {
        const function_idx = compiler.stores.get_max_idx(ir.Function);

        // orders init
        const post_orders = try alloc.alloc(
            std.ArrayListUnmanaged(ir.BasicBlockIdx),
            function_idx.get_usize(),
        );
        const emit_orders = try alloc.alloc(
            std.ArrayListUnmanaged(ir.BasicBlockIdx),
            function_idx.get_usize(),
        );
        for (compiler.stores.function.data.items, 0..) |function, idx| {
            post_orders[idx] = try std.ArrayListUnmanaged(ir.BasicBlockIdx).initCapacity(
                alloc,
                function.basicblocks.items.len,
            );

            emit_orders[idx] = try std.ArrayListUnmanaged(ir.BasicBlockIdx).initCapacity(
                alloc,
                function.basicblocks.items.len,
            );
        }

        // temp values init
        const basicblock_idx = compiler.stores.get_max_idx(ir.BasicBlock);
        const visited_bb = try BitSet.initEmpty(alloc, basicblock_idx.get_usize());

        var res = SharedData{
            .post_orders = post_orders,
            .emit_orders = emit_orders,
            .visited_bb = visited_bb,
        };

        res.update_all_post_orders(compiler);
        return res;
    }

    pub fn get_postorder(self: *const SharedData, function_idx: ir.FunctionIdx) []ir.BasicBlockIdx {
        return self.post_orders[function_idx.get_usize()];
    }

    pub fn get_emitorder(self: *const SharedData, function_idx: ir.FunctionIdx) []ir.BasicBlockIdx {
        return self.emit_orders[function_idx.get_usize()];
    }

    pub fn update_all_post_orders(self: *SharedData, compiler: *const Compiler) void {
        var iter = compiler.stores.idx_iter(ir.Function);
        while (iter.next()) |idx| {
            self.update_post_order(compiler, idx);
        }
    }

    pub fn update_post_order(
        self: *SharedData,
        compiler: *const Compiler,
        function_idx: ir.FunctionIdx,
    ) void {
        self.post_orders[function_idx.get_usize()].clearRetainingCapacity();
        self.visited_bb.unsetAll();

        const function = compiler.get(ir.Function, function_idx);
        self.dfs(function.entry, function_idx, compiler);
    }

    fn dfs(
        self: *SharedData,
        bb: ir.BasicBlockIdx,
        function_idx: ir.FunctionIdx,
        compiler: *const Compiler,
    ) void {
        if (self.visited_bb.isSet(bb.get_usize())) {
            return;
        }
        self.visited_bb.set(bb.get_usize());

        var iter = compiler.get_succesors(bb);
        while (iter.next()) |succ| {
            self.dfs(succ, function_idx, compiler);
        }

        self.post_orders[function_idx.get_usize()].appendAssumeCapacity(bb);
    }
};
