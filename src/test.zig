comptime {
    _ = @import("parser.zig");
    _ = @import("lexer.zig");
    _ = @import("compiler.zig");
    _ = @import("test_runtimes.zig");
    _ = @import("utils.zig");
    _ = @import("test_snap.zig");
    _ = @import("opt/compile.zig");
    _ = @import("opt/analysis/dominator.zig");
    _ = @import("opt/interpreter.zig");
}
