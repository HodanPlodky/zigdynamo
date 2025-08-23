const Base = @import("analysis_base.zig").AnalysisBase;


pub const DominatorAnalysis = struct {
    base: Base,

    pub fn init(base: Base) DominatorAnalysis {
        return DominatorAnalysis {
            .base = base,
        };
    }
};
