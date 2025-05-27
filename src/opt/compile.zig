const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const runtime = @import("../runtime.zig");

pub fn ir_compile(input: *const ast.Function, metadata: runtime.FunctionMetadata, alloc: std.mem.Allocator) !ir.CompiledResult {
    _ = metadata; // autofix
    _ = input; // autofix

    const res = ir.CompiledResult{
        .nodes = try alloc.alloc(ir.Node, 0),
        .functions = try alloc.alloc(ir.Function, 0),
    };
    return res;
}

const Compiler = struct {
    permanent_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    nodes: ir.NodeArray,
    functions: std.ArrayList(ir.Function),

    fn init(permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) Compiler {
        _ = permanent_alloc; // autofix
        _ = scratch_alloc; // autofix
        unreachable;
    }

    fn create_result(self: *const Compiler) ir.CompiledResult {
        return ir.CompiledResult{
            .nodes = self.nodes.data.items,
            .functions = self.functions.items,
        };
    }
};
