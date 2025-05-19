const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const runtime = @import("../runtime.zig");

pub fn ir_compile(input: *const ast.Function, metadata: runtime.FunctionMetadata, alloc: std.mem.Allocator) !ir.CompiledResult {
    _ = metadata; // autofix
    _ = input; // autofix
    const nodes = ir.NodeArray.init(alloc);

    const res = ir.CompiledResult{
        .nodes = nodes,
        .functions = try alloc.alloc(ir.Function, 0),
    };
    return res;
}
