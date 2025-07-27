// Simple version of snapshot testing
// heavily inspired by https://github.com/mnemnion/ohsnap/tree/trunk
// reason to do this was to stay compatible with versions of
// zig the ohsnap library is broken from 0.14.1 because of
// the pretty print dependancy (I think)

const std = @import("std");
const builtin = @import("builtin");
const config = @import("config");

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

        var out_data = try std.ArrayList(u8).initCapacity(alloc, self.expected.len);
        var out_writer = out_data.fixedWriter();

        // the value must have write method
        out_writer.print("{}", .{value});

        try std.testing.expectEqualStrings(self.expected, out_data.items);
    }

    pub fn create(self: *const Snap, value: anytype) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const alloc = arena.allocator();

        var out_data = try std.ArrayList(u8).initCapacity(alloc, self.expected.len);
        var out_writer = out_data.fixedWriter();

        // the value must have write method
        try out_writer.print("{}", .{value});

        // check if it does not need to be created
        try std.testing.expect(!std.mem.eql(u8, self.expected, out_data.items));

        // #stolen from ohsnap
        var maybe_dir_str: ?[]const u8 = null;
        {
            var i: usize = 0;
            while (i < config.module_name.len) : (i += 1) {
                if (std.mem.eql(u8, config.module_name[i], self.location.module)) {
                    maybe_dir_str = config.root_directory[i];
                    break;
                }
            }
        }

        const dir_str = maybe_dir_str orelse "src";
        var mod_dir = try std.fs.cwd().openDir(dir_str, .{});
        defer mod_dir.close();

        const file_text =
            try mod_dir.readFileAlloc(alloc, self.location.file, 1024 * 1024);

        var new_text = try std.ArrayList(u8).initCapacity(alloc, file_text.len);

        const src_line = self.location.line;
        const lines = std.mem.splitScalar(u8, file_text, '\n');

        var line_index: usize = 0;
        var offset: usize = 0;
        while (line_index < src_line) {
            const line = lines[line_index];
            offset += line.len + 1; // +1 => '\n'
            line_index += 1;
        }

        // check for @src
        {
            const line = lines[line_index];
            if (std.mem.indexOf(u8, line, "@src") == null) {
                std.debug.print("Could not find @src");
                std.testing.expect(false);
            }
            line_index += 1;
            offset += line.len + 1; // +1 => '\n'
        }

        const start = offset;

        try new_text.appendSlice(file_text[0..start]);

        // check for multiline string
        {
            const line = lines[line_index];
            if (!is_multiline_string(line)) {
                std.debug.print("Could not find multiline string");
                std.testing.expect(false);
            }
        }

        {
            var line = lines[line_index];
            while (is_multiline_string(line)) {
                offset += line.len + 1; 
                line_index += 1;
                line = lines[line_index];
            }
        }

        const end = offset;


        var new_writer = new_text.writer();
        for (std.mem.splitScalar(u8, out_data.items, "\n")) |line| {
            new_writer.print("\\\\{s}\n", .{line});
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

