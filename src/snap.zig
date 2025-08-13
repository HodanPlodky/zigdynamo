// Simple version of snapshot testing
// heavily inspired by https://github.com/mnemnion/ohsnap/tree/trunk
// reason to do this was to stay compatible with versions of
// zig the ohsnap library is broken from 0.14.1 because of
// the pretty print dependancy (I think)

const std = @import("std");
const builtin = @import("builtin");

comptime {
    std.debug.assert(builtin.is_test);
}

const SnapError = error{
    Update,
};

// #stolen
fn is_multiline_string(line: []const u8) bool {
    for (line, 0..) |c, i| {
        switch (c) {
            ' ' => {},
            '\\' => return (i + 1 < line.len and line[i + 1] == '\\'),
            else => return false,
        }
    }
    return false;
}

pub const Snap = struct {
    location: std.builtin.SourceLocation,
    expected: []const u8,

    pub fn init(location: std.builtin.SourceLocation, expected: []const u8) Snap {
        return Snap{
            .location = location,
            .expected = expected,
        };
    }

    pub fn equal(self: *const Snap, value: anytype) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const alloc = arena.allocator();

        const output = try self.create_value_str(@TypeOf(value), value, alloc);

        try std.testing.expectEqualStrings(self.expected, output);
    }

    pub fn equal_fmt(self: *const Snap, value: anytype) !void {
        const output = try std.fmt.allocPrint(std.testing.allocator, "{}", .{value});
        defer std.testing.allocator.free(output);

        try std.testing.expectEqualStrings(self.expected, output);
    }

    pub fn equal_slice(self: *const Snap, comptime T: type, value: []T) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const alloc = arena.allocator();

        const output = try self.create_value_str([]T, value, alloc);

        try std.testing.expectEqualStrings(self.expected, output);
    }

    fn create_value_str(self: *const Snap, comptime T: type, value: anytype, alloc: std.mem.Allocator) ![]const u8 {
        var out_data = try std.ArrayList(u8).initCapacity(alloc, self.expected.len);
        var out_writer = out_data.writer();
        try pretty_print(T, value, &out_writer, 0);

        //const info = @typeInfo(T);
        //switch (info) {
        //.pointer => {
        //if (info.pointer.size == .slice) {
        //try out_writer.print("[\n", .{});
        //for (value) |item| {
        //try out_writer.print("    {}\n", .{item});
        //}
        //try out_writer.print("]", .{});
        //} else {
        //try out_writer.print("{any}", .{value});
        //}
        //},
        //else => try out_writer.print("{any}", .{value}),
        //}

        return out_data.items;
    }

    const Writer = std.ArrayList(u8).Writer;
    const writer_indent: usize = 4;

    fn pretty_print(comptime T: type, value: T, writer: *Writer, depth: usize) Writer.Error!void {
        const info = @typeInfo(T);
        switch (info) {
            .@"struct" => try pretty_print_struct(T, value, writer, depth),
            .pointer => {
                if (info.pointer.size == .slice) {
                    try pretty_print_slice(T, value, writer, depth);
                } else {
                    try writer.print("&", .{});
                    try pretty_print(info.pointer.child, value.*, writer, depth);
                }
            },
            .@"union" => try pretty_print_union(T, value, writer, depth),
            .array => {
                if (info.array.child == u8) {
                    try writer.print("\"{s}\"", .{value});
                } else {
                    try pretty_print_slice(T, value, writer, depth);
                }
            },
            .optional => try pretty_print_optinal(T, value, writer, depth),
            else => {
                try writer.print("{}", .{value});
            },
        }
    }

    fn pretty_print_depth(writer: *Writer, depth: usize) !void {
        for (0..depth) |_| {
            try writer.print(" ", .{});
        }
    }

    fn pretty_print_struct(comptime T: type, value: T, writer: *Writer, depth: usize) !void {
        comptime {
            std.debug.assert(@typeInfo(T) == .@"struct");
        }

        try writer.print("{{\n", .{});

        const info = @typeInfo(T);

        const val_struct = info.@"struct";

        inline for (val_struct.fields) |field| {
            try pretty_print_depth(writer, depth + writer_indent);
            try writer.print("{s}: ", .{field.name});
            try pretty_print(field.type, @field(value, field.name), writer, depth + writer_indent);
            try writer.print("\n", .{});
        }
        try pretty_print_depth(writer, depth);
        try writer.print("}}", .{});
    }

    fn pretty_print_union(comptime T: type, value: T, writer: *Writer, depth: usize) !void {
        const info = @typeInfo(T);
        comptime {
            std.debug.assert(info == .@"union");
            std.debug.assert(info.@"union".tag_type != null);
        }

        const tag_name = @tagName(value);

        inline for (info.@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, tag_name)) {
                try writer.print("tag({s}): ", .{field.name});
                try pretty_print(field.type, @field(value, field.name), writer, depth);
            }
        }
    }

    fn pretty_print_optinal(comptime T: type, value: T, writer: *Writer, depth: usize) !void {
        const info = @typeInfo(T);
        comptime {
            std.debug.assert(info == .optional);
        }
        
        if (value) |inner| {
            try pretty_print(info.optional.child, inner, writer, depth);
        } else {
            try writer.print("(null)", .{});
        }
    }

    fn pretty_print_slice(comptime T: type, value: T, writer: *Writer, depth: usize) !void {
        const info = @typeInfo(T);
        comptime {
            std.debug.assert((info == .pointer and info.pointer.size == .slice) or info == .array);
        }

        const child = if (info == .pointer) info.pointer.child else info.array.child;

        if (child == u8) {
            try writer.print("\"{s}\"", .{value});
            return;
        }

        try writer.print("[\n", .{});
        for (value) |item| {
            try pretty_print_depth(writer, depth + writer_indent);
            try pretty_print(child, item, writer, depth + writer_indent);
            try writer.print("\n", .{});
        }
        try pretty_print_depth(writer, depth);
        try writer.print("]", .{});
    }

    pub fn create(self: *const Snap, value: anytype) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const alloc = arena.allocator();

        const output = try self.create_value_str(@TypeOf(value), value, alloc);

        const dir_str = "src";
        var mod_dir = try std.fs.cwd().openDir(dir_str, .{});
        defer mod_dir.close();

        const file_text =
            try mod_dir.readFileAlloc(alloc, self.location.file, 1024 * 1024);

        var new_text = try std.ArrayList(u8).initCapacity(alloc, file_text.len);

        const src_line = self.location.line;
        var lines = std.mem.splitScalar(u8, file_text, '\n');

        var line_index: usize = 0;
        var offset: usize = 0;
        while (line_index < src_line - 1) {
            const line = lines.next().?;
            offset += line.len + 1; // +1 => '\n'
            line_index += 1;
        }

        // check for @src
        {
            const line = lines.peek().?;
            std.debug.print("{s}\n", .{line});
            if (std.mem.indexOf(u8, line, "@src") == null) {
                std.debug.print("Could not find @src", .{});
                try std.testing.expect(false);
            }
            _ = lines.next().?;
            line_index += 1;
            offset += line.len + 1; // +1 => '\n'
        }

        const start = offset;

        try new_text.appendSlice(file_text[0..start]);

        // check for multiline string
        {
            const line = lines.peek().?;
            if (!is_multiline_string(line)) {
                std.debug.print("Could not find multiline string", .{});
                try std.testing.expect(false);
            }
        }

        var indent: []const u8 = undefined;
        {
            const line = lines.peek().?;
            for (line, 0..) |c, i| {
                switch (c) {
                    ' ' => {},
                    '\\' => {
                        indent = line[0..(i - 1)];
                        break;
                    },
                    else => unreachable,
                }
            }
        }

        {
            var line = lines.next().?;
            while (is_multiline_string(line)) {
                offset += line.len + 1;
                line_index += 1;
                line = lines.next().?;
            }
        }

        const end = offset;

        var new_writer = new_text.writer();
        var out_data_lines = std.mem.splitScalar(u8, output, '\n');
        while (out_data_lines.next()) |line| {
            try new_writer.print("{s}\\\\{s}\n", .{ indent, line });
        }

        try new_text.appendSlice(file_text[end..]);

        try mod_dir.writeFile(.{
            .sub_path = self.location.file,
            .data = new_text.items,
        });

        std.debug.print("Updated", .{});

        return SnapError.Update;
    }
};
