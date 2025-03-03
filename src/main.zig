const std = @import("std");
const parser = @import("parser.zig");
const ast_intepret = @import("ast_intepret.zig");
const compiler = @import("compiler.zig");
const bc = @import("bc_interpreter.zig");
const ast = @import("ast.zig");

const STACK_SIZE = 4096;
const HEAP_SIZE = 1024 * 15;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var args = std.process.args();
    _ = args.skip();

    const kind: []const u8 = args.next().?;
    const filename: []const u8 = args.next().?;

    const file = try std.fs.cwd().openFile(
        filename,
        std.fs.File.OpenFlags{},
    );

    const input: []const u8 = try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    var p = parser.Parser.new(input, allocator);
    const program = try p.parse();

    if (std.mem.eql(u8, "--ast", kind)) {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        var ast_inter = ast_intepret.Interpret.init(
            try allocator.allocWithOptions(u8, HEAP_SIZE, 16, null),
            gpa.allocator(),
        );
        defer ast_inter.deinit();
        const val = ast_inter.run(program);
        std.debug.print("{}\n", .{val});
    } else if (std.mem.eql(u8, "--bc", kind)) {
        var runtime_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const alloc = runtime_arena.allocator();
        const bytecode = compiler.compile(program, allocator) catch @panic("error");
        var inter = bc.BcIntepreter.init(
            alloc,
            bytecode,
            try allocator.allocWithOptions(u8, HEAP_SIZE, 16, null),
        );
        const val = inter.run();
        std.debug.print("{}\n", .{val});
    } else if (std.mem.eql(u8, "--jit", kind)) {
        var runtime_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const alloc = runtime_arena.allocator();
        const bytecode = compiler.compile(program, allocator) catch @panic("error");
        var inter = bc.JitIntepreter.init(
            alloc,
            bytecode,
            try allocator.allocWithOptions(u8, HEAP_SIZE, 16, null),
        );
        const val = inter.run();
        std.debug.print("{}\n", .{val});
    } else if (std.mem.eql(u8, "--cmp", kind)) {
        const bytecode = compiler.compile(program, allocator) catch @panic("error");
        std.debug.print("{}\n", .{bytecode});
    } else {
        @panic("incorect kind");
    }
}
