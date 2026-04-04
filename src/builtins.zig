const runtime = @import("runtime.zig");
const std = @import("std");
const JitCallConv = @import("jit_utils.zig").JitCallConv;

pub const Builtin = enum(u8) {
    print,
    exit,
};

fn check_arg_count(count: u64, comptime expected: u64) void {
    if (count != expected) {
        std.debug.print("unexpected number of args in builtin\n", .{});
        std.posix.exit(1);
    }
}

pub fn BuiltinDispatch(comptime Interpreter: type) fn(*Interpreter, Builtin, u64) callconv(JitCallConv) void {
    return struct {
        fn dispatch(inter: *Interpreter, builtin: Builtin, arg_count: u64) callconv(JitCallConv) void {
            switch (builtin) {
                .print => inter.do_print(arg_count),
                .exit => {
                    check_arg_count(arg_count, 1);
                    const value: runtime.Value = inter.stack.pop();
                    const value_type = value.get_type();
                    std.debug.assert(value_type == runtime.ValueType.number);
                    const number = value.get_number() % 0xff;
                    std.posix.exit(@intCast(number));
                },
            }
        }
    }.dispatch;
}

pub fn get_builtin(name: []const u8) ?Builtin {
    if (std.mem.eql(u8, name, "print")) {
        return Builtin.print;
    } else if (std.mem.eql(u8, name, "exit")) {
        return Builtin.exit;
    }

    return null;
}
