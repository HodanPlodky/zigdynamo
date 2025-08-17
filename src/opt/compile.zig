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

    pub fn format(
        self: *const CompiledResult,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options; // autofix
        _ = fmt;

        try writer.print("Result\n", .{});

        try self.write_fn(self.entry_fn, writer);
    }

    pub fn write_fn(self: *const CompiledResult, idx: ir.FunctionDistinct.Index, writer: anytype) !void {
        const function = self.stores.function.get(idx);
        try writer.print("function {{\n", .{});
        try self.write_bb(function.entry, writer);
        try writer.print("}}\n", .{});
    }

    pub fn write_bb(self: *const CompiledResult, idx: ir.BasicBlockIdx, writer: anytype) !void {
        const basicblock = self.stores.basicblock.get(idx);
        try writer.print("basicblock{}:\n", .{idx.index});
        for (basicblock.instructions.items) |inst_idx| {
            try self.write_inst(inst_idx, writer);
        }
    }

    pub fn write_inst(self: *const CompiledResult, idx: ir.InstructionIdx, writer: anytype) !void {
        const inst = self.stores.instructions.get(idx);
        try writer.print("\t%{} = {s} ", .{ idx.index, inst.opcode() });
        try self.write_payload(inst, writer);
        try writer.print("\n", .{});
    }

    pub fn write_payload(self: *const CompiledResult, inst: ir.Instruction, writer: anytype) !void {
        switch (inst) {
            // immediate ops
            .ldi, .load_global, .arg, .store_global => |num| try writer.print("{}", .{num}),

            // one reg ops
            .ret, .mov => |reg| try writer.print("%{}", .{reg.index}),

            //  binop ops
            .add, .sub, .mul, .div => |binop_idx| {
                const binop: ir.BinOpData = self.stores.binop.get(binop_idx);
                try writer.print("%{}, %{}", .{ binop.left.index, binop.right.index });
            },
            .branch => |branch_idx| {
                const branch: ir.BranchData = self.stores.branch.get(branch_idx);
                try writer.print("%{}, basicblock{}, basicblock{}", .{
                    branch.cond,
                    branch.true_branch,
                    branch.false_branch,
                });
            },
            .jmp => |bb_idx| try writer.print("{}", .{bb_idx.index}),
            .phony => unreachable,
        }
    }
};

const Compiler = struct {
    permanent_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    result: CompiledResult,
    current: ir.BasicBlockIdx,

    fn init(permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .permanent_alloc = permanent_alloc,
            .scratch_alloc = scratch_alloc,
            .current = undefined,
            .result = .{},
        };
    }

    fn compile(self: *Compiler, input: *const ast.Function, metadata: runtime.FunctionMetadata) !void {
        self.result.entry_fn = try self.compile_fn(input, metadata);
    }

    fn compile_fn(self: *Compiler, function: *const ast.Function, metadata: runtime.FunctionMetadata) !ir.FunctionDistinct.Index {
        _ = metadata;
        const bb_idx = try self.create(ir.BasicBlock);
        self.current = bb_idx;
        const fn_idx = try self.create_with(ir.Function, try ir.Function.create(bb_idx, self.permanent_alloc));

        for (function.params, 0..) |_, i| {
            _ = try self.append_inst(ir.Instruction{ .arg = @intCast(i) });
        }

        const ret_reg = try self.compile_expr(function.body);
        _ = try self.append_inst(ir.Instruction{ .ret = ret_reg });

        return fn_idx;
    }

    fn compile_expr(self: *Compiler, expr: *const ast.Ast) !ir.Reg {
        switch (expr.*) {
            .binop => |binop| {
                const left_reg = try self.compile_expr(binop.left);
                const right_reg = try self.compile_expr(binop.right);
                const binop_data = try self.create_with(ir.BinOpData, ir.BinOpData{
                    .left = left_reg,
                    .right = right_reg,
                });
                return switch (binop.op) {
                    '+' => try self.append_inst(ir.Instruction{ .add = binop_data }),
                    '-' => try self.append_inst(ir.Instruction{ .sub = binop_data }),
                    '*' => try self.append_inst(ir.Instruction{ .mul = binop_data }),
                    '/' => try self.append_inst(ir.Instruction{ .div = binop_data }),
                    else => unreachable,
                };
            },
            .number => |num| {
                return try self.append_inst(ir.Instruction{ .ldi = num });
            },
            else => {
                std.debug.print("{}", .{expr});
                unreachable;
            },
        }
    }

    pub fn get_curr(self: *Compiler) *ir.BasicBlock {
        return self.get(ir.BasicBlock, self.current);
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

    pub fn append_inst(self: *Compiler, inst: ir.Instruction) !ir.Reg {
        const inst_idx = try self.create_inst(inst);
        var bb = self.get_curr();
        try bb.instructions.append(self.permanent_alloc, inst_idx);
        return inst_idx;
    }

    fn create_result(self: *const Compiler) CompiledResult {
        return self.result;
    }
};
