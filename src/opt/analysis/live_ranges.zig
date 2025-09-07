const std = @import("std");
const Base = @import("analysis_base.zig").AnalysisBase;
const ir = @import("../ir.zig");

pub const LiveRangesAnalysis = struct {
    // I can get away with "only"
    // u32 since the index for inst
    // is u32 so I cannot have move inst
    const Range = struct {
        begin: u32,
        end: u32,
    };

    base: Base,
    ranges: []Range,

    pub fn init(base: Base) LiveRangesAnalysis {
        const inst_count = base.compiler.stores.get_max_idx();
        return LiveRangesAnalysis{
            .base = base,
            .ranges = try base.alloc.alloc(Range, inst_count.get_usize()),
        };
    }

    pub fn analyze(self: *LiveRangesAnalysis) void {
        var fn_iter = self.base.compiler.stores.idx_iter(ir.Function);
        while (fn_iter.next()) |idx| {
            self.analyze_fn(idx);
        }
    }

    fn analyze_fn(self: *LiveRangesAnalysis, function_idx: ir.FunctionIdx) void {
        var curr_idx: u32 = 0;    

        for (self.base.shared_data.get_emit(function_idx)) |bb_idx| {
            curr_idx = self.process_bb(bb_idx, curr_idx);
        }
    }

    fn process_bb(self: *LiveRangesAnalysis, bb_idx: ir.BasicBlockIdx, curr_idx: u32) void {
        const bb = self.base.compiler.get(ir.BasicBlock, bb_idx);

        for (bb.instructions.items) |inst_idx| {
            self.ranges[inst_idx.get_usize()].begin = curr_idx;
            self.ranges[inst_idx.get_usize()].end = curr_idx;
            var reg_iter = self.base.compiler.stores.get_reg_iter(inst_idx);
            while(reg_iter.next()) |reg| {
                self.ranges[reg.get_usize()].end = curr_idx;
            }
            curr_idx += 1;
        }

        return curr_idx;
    }

    pub fn get_range(self: *const LiveRangesAnalysis, inst_idx: ir.InstructionIdx) Range {
        return self.ranges[inst_idx.get_usize()];
    }
};
