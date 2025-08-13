const snap = @import("snap.zig");

test "basic" {
    try snap.Snap.init(@src(),
        \\1
    ).equal(1);

    try snap.Snap.init(@src(),
        \\3
    ).equal(1 + 2);
}

test "struct pretty" {
    const tmp_array: [3]u32 = .{ 1, 2, 3 };
    const tmp_struct = .{ .a = 1 };

    const value = .{
        .number = 5,
        .inner = .{
            .name = "Hodan",
            .number = 4,
            .deeper = tmp_struct,
        },
        .inner2 = tmp_struct,
        .array = tmp_array,
    };

    try snap.Snap.init(@src(),
        \\{
        \\    number: 5
        \\    inner: {
        \\        name: &"Hodan"
        \\        number: 4
        \\        deeper: {
        \\            a: 1
        \\        }
        \\    }
        \\    inner2: {
        \\        a: 1
        \\    }
        \\    array: [
        \\        1
        \\        2
        \\        3
        \\    ]
        \\}
    ).equal(value);
}

test "union pretty" {
    const Result = union(enum) {
        ok: i32,
        err,
    };

    try snap.Snap.init(@src(),
        \\tag(ok): 1
    ).equal(Result{ .ok = 1 });

    try snap.Snap.init(@src(),
        \\@typeInfo(test_snap.test.union pretty.Result).@"union".tag_type.?.err
    ).equal(Result.err);
}

const Node = union(enum) {
    cons: struct {
        value: i32,
        next: *const Node,
    },
    nil,
};

test "cons list" {
    const nil: Node = Node.nil;
    const a = Node{ .cons = .{ .value = 1, .next = &nil } };
    const b = Node{ .cons = .{ .value = 2, .next = &a } };

    try snap.Snap.init(@src(),
        \\tag(cons): {
        \\    value: 2
        \\    next: &tag(cons): {
        \\        value: 1
        \\        next: &tag(nil): void
        \\    }
        \\}
    ).equal(b);
}
