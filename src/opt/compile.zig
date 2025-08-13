const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const runtime = @import("../runtime.zig");

pub fn ir_compile(input: *const ast.Function, metadata: runtime.FunctionMetadata, alloc: std.mem.Allocator) !ir.CompiledResult {
    _ = metadata; // autofix
    _ = input; // autofix

    // it would be probably better to have this survive across the calls
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();

    const scratch = scratch_arena.allocator();

    const compiler = Compiler.init(alloc, scratch);
    return compiler.create_result();
}

const Compiler = struct {
    permanent_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    nodes: ir.NodeArray,
    functions: std.ArrayList(ir.Function),

    fn init(permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .permanent_alloc = permanent_alloc,
            .scratch_alloc = scratch_alloc,
            .nodes = ir.NodeArray.init(permanent_alloc),
            .functions = std.ArrayList(ir.Function).init(permanent_alloc),
        };
    }

    fn create_result(self: *const Compiler) ir.CompiledResult {
        return ir.CompiledResult{
            .nodes = self.nodes.data.items,
            .functions = self.functions.items,
        };
    }
};
