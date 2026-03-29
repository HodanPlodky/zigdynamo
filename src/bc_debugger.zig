const std = @import("std");
const Bytecode = @import("bytecode.zig").Bytecode;
const Stack = @import("bc_interpreter.zig").Stack;
const LocalEnv = @import("bc_interpreter.zig").LocalEnv;
const Writer = std.io.Writer;
const rev = @import("utils.zig").ReversedSlice;
const Value = @import("runtime.zig").Value;

const Command = enum {
    nothing,
    step,
    quit,
};

pub fn BytecodeDebugger(comptime Interpret: type) type {
    return struct {
        const Self = @This();
        state: *Interpret,
        writer: *Writer,
        scratch: std.heap.ArenaAllocator,
        last_command: Command = .nothing,

        pub fn init(state: *Interpret, writer: *Writer) Self {
            const arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            return .{
                .state = state,
                .writer = writer,
                .scratch = arena,
            };
        }

        pub fn deinit(self: *Self) void {
            self.scratch.deinit();
        }

        pub fn breakpoint(self: *Self) void {
            _ = self.writer.write("\x1B[2J\x1B[H") catch @panic("write debugger panic");
            self.writer.flush() catch @panic("write debugger panic");
            defer _ = self.scratch.reset(.retain_capacity);
            const alloc = self.scratch.allocator();

            var buffer: [1024]u8 = undefined;
            self.show_state();
            const stdin = std.fs.File.stdin();

            var stdin_reader = stdin.reader(&buffer);
            const reader = &stdin_reader.interface;

            while (true) {
                const line = self.read_line(reader, alloc);
                const comm = self.parse_command(line);
                self.last_command = comm;
                switch (comm) {
                    .step => return,
                    .quit => std.process.exit(0),
                    .nothing => {},
                }
            }
        }

        fn read_line(self: *Self, reader: *std.io.Reader, alloc: std.mem.Allocator) []u8 {
            self.write_msg("> ");
            var line: std.ArrayList(u8) = .{};
            while (true) {
                const byte = reader.takeByte() catch @panic("debugger panic");
                if (byte == '\n') {
                    break;
                }
                line.append(alloc, byte) catch @panic("debugger panic");
                std.Thread.sleep(100);
            }

            return line.items;
        }

        fn parse_command(self: *Self, command_str: []const u8) Command {
            if (std.mem.eql(u8, command_str, "step") or std.mem.eql(u8, command_str, "s")) {
                return .step;
            }
            if (std.mem.eql(u8, command_str, "quit") or std.mem.eql(u8, command_str, "q")) {
                return .quit;
            }
            if (std.mem.eql(u8, command_str, "")) {
                return self.last_command;
            }
            return .nothing;
        }

        fn show_state(self: *Self) void {
            defer self.writer.flush() catch @panic("write debugger panic");

            // this pc is after reading of instruction
            const pc = self.state.pc - 1;
            const bytecode: Bytecode = self.state.bytecode;
            const function_idx = self.state.curr_fn;
            const function = bytecode.get_function(function_idx);
            self.writer.print("pc: {}\n", .{pc}) catch @panic("write debugger panic");
            function.write_with_highlight(self.writer, pc) catch @panic("write debugger panic");

            // print stack
            const stack: Stack = self.state.stack;
            var iter = rev(Value).init(stack.stack.items);

            _ = self.writer.write("Stack:\n[ ") catch @panic("write debugger panic");
            if (iter.next()) |first_val| {
                Value.format(first_val, self.writer) catch @panic("write debugger panic");
                while (iter.next()) |value| {
                    _ = self.writer.write(", ") catch @panic("write debugger panic");
                    Value.format(value, self.writer) catch @panic("write debugger panic");
                }
            }
            _ = self.writer.write(" ]\nEnv:\n") catch @panic("write debugger panic");

            // print local env
            const env: LocalEnv = self.state.env.local;
            const count = env.get_current_count();
            for (0..count) |idx| {
                _ = self.writer.print("|{}: ", .{idx}) catch @panic("write debugger panic");
                const value = env.get(@intCast(idx));
                Value.format(value, self.writer) catch @panic("write debugger panic");
                _ = self.writer.print("| ", .{}) catch @panic("write debugger panic");
            }
            _ = self.writer.write("\n\n") catch @panic("write debugger panic");
        }

        fn write_msg(self: *Self, msg: []const u8) void {
            _ = self.writer.write(msg) catch @panic("debugger panic");
            self.writer.flush() catch @panic("debugger panic");
        }
    };
}
