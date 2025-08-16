const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const runtime = @import("../runtime.zig");

pub fn ir_compile(input: *const ast.Function, metadata: runtime.FunctionMetadata, alloc: std.mem.Allocator) !CompiledResult {
    // it would be probably better to have this survive across the calls
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();

    const scratch = scratch_arena.allocator();

    var compiler = Compiler.init(alloc, scratch);
    try compiler.compile(input, metadata);
    return compiler.create_result();
}

const Stores = struct {
    instructions: ir.InstructionDistinct.Multi = .{},
    basicblock: ir.BasicBlockDistinct.ArrayListUn = .{},
    function: ir.FunctionDistinct.ArrayListUn = .{},
    binop: ir.BinOpDistinct.Multi = .{},
    branch: ir.BranchDistinct.Multi = .{},
    phony: ir.PhonyDistinct.Multi = .{},

    const Self = @This();

    fn get_index_type(comptime T: type) type {
        const info = @typeInfo(Self).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return field.type.DistIndex;
            }
        }

        @compileError("could not find proper index");
    }
};

pub const CompiledResult = struct {
    entry_fn: ir.FunctionDistinct.Index = undefined,
    stores: Stores = .{},
};

const Compiler = struct {
    permanent_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    instruction_store: std.MultiArrayList(ir.Instruction),
    result: CompiledResult,

    fn init(permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .permanent_alloc = permanent_alloc,
            .scratch_alloc = scratch_alloc,
            .instruction_store = std.MultiArrayList(ir.Instruction){},
            .result = .{},
        };
    }

    fn compile(self: *Compiler, input: *const ast.Function, metadata: runtime.FunctionMetadata) !void {
        _ = input;
        _ = metadata;

        const bb_idx = try self.create(ir.BasicBlock);
        const fn_idx = try self.create_with(ir.Function, try ir.Function.create(bb_idx, self.permanent_alloc));
        var bb = self.get(ir.BasicBlock, bb_idx);

        const ldi = try self.create_inst(ir.Instruction{ .ldi = 1 });
        try bb.instruction.append(self.permanent_alloc, ldi);
        const ret = try self.create_inst(ir.Instruction{ .ret = ldi });
        try bb.instruction.append(self.permanent_alloc, ret);

        self.result.entry_fn = fn_idx;
    }

    pub fn create(self: *Compiler, comptime T: type) !Stores.get_index_type(T) {
        return self.create_with(T, T{});
    }

    pub fn create_with(self: *Compiler, comptime T: type, value: T) !Stores.get_index_type(T) {
        const info = @typeInfo(Stores).@"struct";
        const Index = Stores.get_index_type(T);

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                const index = @field(self.result.stores, field.name).len();
                try @field(self.result.stores, field.name).data.append(self.permanent_alloc, value);
                return Index.new(@intCast(index));
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get(self: *Compiler, comptime T: type, index: Stores.get_index_type(T)) *T {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self.result.stores, field.name).get_ptr(index);
            }
        }

        @compileError("could not find proper index");
    }

    pub fn create_basicblock(self: *Compiler) !ir.BasicBlockIdx {
        return self.result.stores.create(ir.BasicBlock);
    }

    pub fn create_inst(self: *Compiler, inst: ir.Instruction) !ir.InstructionIdx {
        return self.create_with(ir.Instruction, inst);
    }

    fn create_result(self: *const Compiler) CompiledResult {
        return self.result;
    }
};
