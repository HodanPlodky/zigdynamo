const snap = @import("snap.zig");

test "basic" {
    try snap.Snap.init(@src(),
        \\1
    ).equal(1);

    try snap.Snap.init(@src(),
       \\3
    ).equal(1 + 2);
}
