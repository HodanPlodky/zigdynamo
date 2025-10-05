const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");
const LiveRangeAnalysis = @import("live_ranges.zig").LiveRangesAnalysis;
const GPR64 = @import("../../jit_utils.zig").GPR64;
const Value = @import("../../runtime.zig").Value;

pub const RegAllocAnalysis = struct {
    pub const ValuePlace = union(enum) {
        reg: GPR64,
        memory: usize,
        value: Value,
        none,
    };

    base: Base,
    translates: []ValuePlace,
    free_regs: std.ArrayList(GPR64),
    ranges: LiveRangeAnalysis,

    release: []std.ArrayList(ir.Reg),
    curr_max_mem: usize = 0,

    pub fn init(base: Base, free_regs: []GPR64) !RegAllocAnalysis {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        return RegAllocAnalysis{
            .base = base,
            .translates = try base.alloc.alloc(ValuePlace, inst_count.get_usize()),
            .free_regs = std.ArrayList(GPR64).initBuffer(free_regs),
            .ranges = try LiveRangeAnalysis.init(base),

            // TODO: try to bound it to max size of the function
            .release = try base.alloc.alloc(std.ArrayList(ir.Reg), inst_count.get_usize()),
        };
    }

    pub fn analyze(self: *RegAllocAnalysis) !void {
        for (self.release) |*item| {
            item.* = .{};
        }
        self.ranges.analyze();
        var iter = self.base.compiler.stores.idx_iter(ir.Function);
        while (iter.next()) |idx| {
            try self.analyze_fn(idx);
        }
    }

    fn analyze_fn(self: *RegAllocAnalysis, function_idx: ir.FunctionIdx) !void {
        for (self.release) |*item| {
            item.clearRetainingCapacity();
        }
        var curr_idx: u32 = 0;
        for (self.base.shared_data.get_emitorder(function_idx)) |bb_idx| {
            const bb = self.base.compiler.stores.get(ir.BasicBlock, bb_idx);

            for (bb.instructions.items) |inst_idx| {
                try self.process_inst(inst_idx, curr_idx);
                curr_idx += 1;
            }
        }
    }

    fn process_inst(self: *RegAllocAnalysis, inst_idx: ir.InstructionIdx, curr_idx: u32) !void {
        const range = self.ranges.get_range(inst_idx);
        std.debug.assert(range.begin == curr_idx);

        self.do_release(curr_idx);

        const inst = self.base.compiler.get(ir.Instruction, inst_idx);
        const inst_type = self.base.compiler.stores.get_type(inst);

        // if the inst does not returns anything
        // then dont set arch reg for it
        switch (inst_type) {
            .Void => {
                self.translates[inst_idx.get_usize()] = .none;
                return;
            },
            .Bottom => unreachable,
            else => {},
        }

        // the constant instruction
        // should set the translation as
        // runtime value
        switch (inst) {
            .ldi => |num| {
                self.translates[inst_idx.get_usize()] = .{ .value = Value.new_num(num) };
                return;
            },
            .nil => {
                self.translates[inst_idx.get_usize()] = .{ .value = Value.new_nil() };
                return;
            },
            .true => {
                self.translates[inst_idx.get_usize()] = .{ .value = Value.new_true() };
                return;
            },
            .false => {
                self.translates[inst_idx.get_usize()] = .{ .value = Value.new_false() };
                return;
            },
            else => {},
        }

        if (self.free_regs.pop()) |arch_reg| {
            self.translates[inst_idx.get_usize()] = .{ .reg = arch_reg };
            try self.release[@intCast(range.end)].append(self.base.alloc, inst_idx);
        } else {
            self.translates[inst_idx.get_usize()] = .{ .memory = self.curr_max_mem };
        }
    }

    fn do_release(self: *RegAllocAnalysis, curr_idx: u32) void {
        for (self.release[@intCast(curr_idx)].items) |reg| {
            const place = self.translates[reg.get_usize()];
            switch (place) {
                .reg => |arch_reg| self.free_regs.appendAssumeCapacity(arch_reg),
                else => unreachable,
            }
        }
    }
};

const Compiler = @import("../compile.zig").Compiler;

fn test_run_analysis(compiler: *const Compiler, alloc: std.mem.Allocator, free_regs: []GPR64) !RegAllocAnalysis {
    const SharedData = @import("analysis_base.zig").SharedData;
    const AnalysisBase = @import("analysis_base.zig").AnalysisBase;

    const shared_data = try SharedData.init(compiler, alloc);
    const analysis_base = AnalysisBase{
        .compiler = compiler,
        .alloc = alloc,
        .shared_data = shared_data,
    };

    var reg_alloc = try RegAllocAnalysis.init(analysis_base, free_regs);
    try reg_alloc.analyze();
    return reg_alloc;
}

test "basic reg alloc" {
    //const snap = @import("../../snap.zig");

    // this is how to use it
    var permanent_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer permanent_arena.deinit();
    var scratch_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch_arena.deinit();

    const permanent_alloc = permanent_arena.allocator();
    const scratch_alloc = scratch_arena.allocator();
    var compiler = try Compiler.init(&.{}, permanent_alloc, scratch_alloc);
    _ = try compiler.create_empty();

    const ldi = try compiler.append_inst(.{ .ldi = 1 });
    try compiler.append_terminator(.{ .ret = ldi });

    {
        var free_regs: [4]GPR64 = .{ GPR64.rax, GPR64.rbx, GPR64.rcx, GPR64.rdx };
        const regs = try test_run_analysis(&compiler, scratch_alloc, &free_regs);
        
        try std.testing.expectEqual(regs.translates.len, 2);
        try std.testing.expectEqualSlices(
            RegAllocAnalysis.ValuePlace,
            &.{RegAllocAnalysis.ValuePlace{.value = Value.new_num(1)}, RegAllocAnalysis.ValuePlace.none},
            regs.translates,
        );
    }
}
