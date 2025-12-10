const Base = @import("pass_base.zig").PassBase;

// TODO please do it
pub const SerializationPass = struct {
    base: Base,

    pub fn init(base: Base) !SerializationPass {
        return SerializationPass{
            .base = base,
        };
    }

    pub fn run(self: *SerializationPass) !void {
        _ = self;
    }
};
