const runtime = @import("runtime.zig");
const std = @import("std");
const JitCallConv = @import("jit_utils.zig").JitCallConv;
const ConstantIndex = @import("bytecode.zig").ConstantIndex;
const Constant = @import("bytecode.zig").Constant;

pub const Builtin = enum(u8) {
    print,
    char_to_int,
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
                    if (value_type != .number) {
                        @panic("exit must get number");
                    }
                    const number = value.get_number() % 0xff;
                    std.posix.exit(@intCast(number));
                },
                .char_to_int => {
                    check_arg_count(arg_count, 1);
                    const value: runtime.Value = inter.stack.pop();
                    const value_type = value.get_type();
                    if (value_type != .string) {
                        @panic("char_to_int must get string");
                    }

                    const index = value.get_idx();
                    const constant_idx = ConstantIndex.new(index);
                    const constant: Constant = inter.bytecode.get_constant(constant_idx);
                    if (constant.get_type() != .string) {
                        @panic("char_to_int must get string");
                    }
                    const slice = constant.get_slice();
                    if (slice.len != 6) {
                        @panic("char_to_int must get string of length 1");
                    }
                    const number: u32 = @intCast(slice[5]);
                    const result = runtime.Value.new_num(number);
                    inter.stack.push(result);
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
    } else if (std.mem.eql(u8, name, "char_to_int")) {
        return Builtin.char_to_int;
    }

    return null;
}
