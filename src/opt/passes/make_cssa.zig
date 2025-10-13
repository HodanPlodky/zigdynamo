const std = @import("std");
const Base = @import("pass_base.zig").PassBase;
const DominantorAnalysis = @import("../analysis/dominator.zig").DominatorAnalysis;
const ir = @import("../ir.zig");

/// create a CSSA format
/// Revisiting Out-of-SSA Translation for Correctness, Code Quality, and Efficiency
/// https://inria.hal.science/inria-00349925
pub const MakeCSSA = struct {
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
};

test "basic" {}
