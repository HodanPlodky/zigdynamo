const snap = @import("snap.zig");

test "basic" {
    try snap.Snap.init(
        @src(),
        \\ sada
    ).create(1);
}
