const Base = @import("pass_base.zig").PassBase;
const Canonical = @import("../analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;

// TODO please do it
pub const SerializationPass = struct {
    base: Base,

    pub fn init(base: Base, canon: Canonical) !SerializationPass {
        _ = canon;
        return SerializationPass{
            .base = base,
        };
    }

    pub fn run(self: *SerializationPass) !void {
        _ = self;
    }
};
