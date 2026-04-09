const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");
const Liveness = @import("liveness.zig").LivenessAnalysis;

pub const LiveRangesAnalysis = struct {
    // I can get away with "only"
    // u32 since the index for inst
    // is u32 so I cannot have more inst
    const Range = struct {
        begin: u32,
        end: u32,

        pub fn empty(self: *const Range) bool {
            return self.begin == self.end;
        }
    };

    base: Base,
    ranges: []Range,
    liveness: Liveness,

    pub fn init(base: Base) !LiveRangesAnalysis {
        const inst_count = base.compiler.stores.get_max_idx(ir.Instruction);
        return LiveRangesAnalysis{
            .base = base,
            .ranges = try base.alloc.alloc(Range, inst_count.get_usize()),
            .liveness = try Liveness.init(base, base.compiler.canonical_regs),
        };
    }

    pub fn analyze(self: *LiveRangesAnalysis) !void {
        try self.liveness.analyze();
        var fn_iter = self.base.compiler.stores.idx_iter(ir.Function);
        @memset(self.ranges, .{ .begin = std.math.maxInt(u32), .end = 0 });
        while (fn_iter.next()) |idx| {
            self.analyze_fn(idx);
        }
    }

    fn analyze_fn(self: *LiveRangesAnalysis, function_idx: ir.FunctionIdx) void {
        var curr_idx: u32 = 0;

        const order = self.base.shared_data.get_emitorder(function_idx);
        for (order) |bb_idx| {
            curr_idx = self.process_bb(bb_idx, curr_idx);
        }
    }

    fn process_bb(self: *LiveRangesAnalysis, bb_idx: ir.BasicBlockIdx, curr_idx: u32) u32 {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);
        var new_idx = curr_idx;

        for (bb.instructions.items, 0..) |inst_idx, index| {
            const outreg = self.base.compiler.get_canonical_output(inst_idx);
            if (self.ranges[outreg.get_usize()].begin >= new_idx) {
                self.ranges[outreg.get_usize()].begin = new_idx;
            }
            self.ranges[outreg.get_usize()].end = new_idx;

            const live = self.liveness.get_liveness_in(bb_idx, index);
            var live_iter = live.iterator(.{});
            while (live_iter.next()) |reg| {
                // should already be canon
                self.ranges[reg].end = new_idx;
            }
            new_idx += 1;
        }

        return new_idx;
    }

    pub fn get_range(self: *const LiveRangesAnalysis, inst_idx: ir.InstructionIdx) Range {
        return self.ranges[inst_idx.get_usize()];
    }
};
