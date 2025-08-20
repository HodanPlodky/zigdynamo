const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const runtime = @import("../runtime.zig");

pub fn ir_compile(input: *const ast.Function, metadata: runtime.FunctionMetadata, globals: [][]const u8, alloc: std.mem.Allocator) !CompiledResult {
    // it would be probably better to have this survive across the calls
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();

    const scratch = scratch_arena.allocator();

    var compiler = try Compiler.init(globals, alloc, scratch);
    try compiler.compile(input, metadata);
    return compiler.create_result();
}

const Stores = struct {
    instructions: ir.InstructionDistinct.Multi = .{},
    basicblock: ir.BasicBlockDistinct.ArrayListUn = .{},
    function: ir.FunctionDistinct.ArrayListUn = .{},
    binop: ir.BinOpDistinct.Multi = .{},
    branch: ir.BranchDistinct.Multi = .{},
    set_local: ir.SetLocalDistinct.Multi = .{},
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

        try self.write_fn(self.entry_fn, writer);
    }

    pub fn write_fn(self: *const CompiledResult, idx: ir.FunctionDistinct.Index, writer: anytype) !void {
        const function: ir.Function = self.stores.function.get(idx);
        try writer.print("function {{\n", .{});
        try self.write_bb(function.entry, writer);
        for (function.basicblocks.items) |bb| {
            if (bb.index != function.entry.index) {
                try self.write_bb(bb, writer);
            }
        }
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
        const inst_type = self.get_type(inst);
        if (inst_type == ir.Type.Void) {
            try writer.print("    {s} ", .{inst.opcode()});
        } else {
            try writer.print("    %{} = {s} ", .{ idx.index, inst.opcode() });
        }
        try self.write_payload(inst, writer);
        try writer.print("\n", .{});
    }

    pub fn get_type(self: *const CompiledResult, inst: ir.Instruction) ir.Type {
        return switch (inst) {
            .ldi => ir.Type.Int,
            .mov => |reg| {
                const src_inst = self.stores.instructions.get(reg);
                return self.get_type(src_inst);
            },
            .nil => ir.Type.Nil,
            .true => ir.Type.True,
            .false => ir.Type.False,
            .load_global => ir.Type.Top,
            .store_global => ir.Type.Void,
            .load_env => ir.Type.Top,
            .store_env => ir.Type.Void,
            .add, .sub, .mul, .div => ir.Type.Top,
            .ret, .branch, .jmp => ir.Type.Void,
            .arg => ir.Type.Top,
            // TODO use join
            .phony => ir.Type.Top,
            .get_local => ir.Type.Top,
            .set_local => ir.Type.Void,
        };
    }

    pub fn write_payload(self: *const CompiledResult, inst: ir.Instruction, writer: anytype) !void {
        switch (inst) {
            // immediate ops
            .ldi, .load_global, .arg, .store_global, .load_env => |num| try writer.print("{}", .{num}),

            // TODO
            .store_env => unreachable,

            // empty
            .nil, .true, .false => {},

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
                    branch.cond.index,
                    branch.true_branch.index,
                    branch.false_branch.index,
                });
            },
            .jmp => |bb_idx| try writer.print("{}", .{bb_idx.index}),
            .phony => |phony_idx| {
                const data = self.stores.phony.get(phony_idx);
                if (data.data.len > 0) {
                    try writer.print("{}, %{}", .{data.data[0].label.index, data.data[0].reg.index});
                    for (data.data[1..]) |pair| {
                        try writer.print(", {}, %{}", .{pair.label.index, pair.reg.index});
                    }
                }       
            },
            .get_local => |reg| try writer.print("{}", .{reg}),
            .set_local => |set_local_idx| {
                const set_local: ir.SetLocalData = self.stores.set_local.get(set_local_idx);
                try writer.print("{}, %{}", .{ set_local.local_idx, set_local.value.index });
            },
        }
    }
};

const Locals = struct {
    const Errors = error{
        PopError,
        NotLocal,
    };

    alloc: std.mem.Allocator,
    locals: std.ArrayListUnmanaged(std.StringHashMapUnmanaged(u32)),
    env: std.StringHashMapUnmanaged(u32),
    curr_idx: u32,
    env_idx: u32,

    fn init(alloc: std.mem.Allocator) !Locals {
        var res = Locals{
            .alloc = alloc,
            .locals = .{},
            .env = .{},
            .curr_idx = 0,
            .env_idx = 0,
        };
        try res.push_block();
        return res;
    }

    fn push_block(self: *Locals) !void {
        try self.locals.append(self.alloc, .{});
    }

    fn pop_block(self: *Locals) !void {
        const tmp = self.locals.pop();
        if (tmp == null) {
            return Locals.Errors.PopError;
        }
    }

    fn set(self: *Locals, var_name: []const u8) !u32 {
        try self.locals.items[self.locals.items.len - 1].put(self.alloc, var_name, self.curr_idx);
        self.curr_idx += 1;
        return self.curr_idx - 1;
    }

    fn get(self: *const Locals, var_name: []const u8) ?u32 {
        var idx = self.locals.items.len;
        while (idx > 0) {
            idx -= 1;
            const tmp = self.locals.items[idx].get(var_name);
            if (tmp) |local_idx| {
                return local_idx;
            }
        }
        return null;
    }

    fn set_env(self: *Locals, var_name: []const u8) !u32 {
        try self.env.putNoClobber(self.alloc, var_name, self.env_idx);
        self.env_idx += 1;
        return self.env_idx - 1;
    }

    fn get_env(self: *const Locals, var_name: []const u8) ?u32 {
        return self.env.get(var_name);
    }
};

const Compiler = struct {
    permanent_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    result: CompiledResult,
    current: ir.BasicBlockIdx,
    locals: Locals,
    globals: [][]const u8,
    fn_idx: ir.FunctionDistinct.Index = undefined,

    fn init(globals: [][]const u8, permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) !Compiler {
        return Compiler{
            .permanent_alloc = permanent_alloc,
            .scratch_alloc = scratch_alloc,
            .current = undefined,
            .result = .{},
            .locals = try Locals.init(scratch_alloc),
            .globals = globals,
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
        self.fn_idx = fn_idx;

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
            .let => |let| {
                const value_reg = try self.compile_expr(let.value);
                const local_idx = try self.locals.set(let.target);
                const data = try self.create_with(ir.SetLocalData, ir.SetLocalData{
                    .local_idx = local_idx,
                    .value = value_reg,
                });
                _ = try self.append_inst(ir.Instruction{ .set_local = data });
                return try self.append_inst(ir.Instruction{ .get_local = local_idx });
            },
            .block => |block| {
                try self.locals.push_block();
                if (block.len == 0) {
                    return try self.append_inst(ir.Instruction.nil);
                }

                var res: ir.Reg = undefined;
                for (block) |*item| {
                    res = try self.compile_expr(item);
                }
                try self.locals.pop_block();
                return res;
            },
            .ident => |ident| {
                if (self.locals.get(ident)) |local_idx| {
                    return try self.append_inst(ir.Instruction{ .get_local = local_idx });
                } else if (self.get_global(ident)) |global_idx| {
                    return try self.append_inst(ir.Instruction{ .load_global = global_idx });
                } else if (self.locals.get_env(ident)) |env_idx| {
                    return try self.append_inst(ir.Instruction{ .load_env = env_idx });
                } else {
                    const env_idx = try self.locals.set_env(ident);
                    return try self.append_inst(ir.Instruction{ .load_env = env_idx });
                }
            },
            .condition => |condition| {
                const cond_reg = try self.compile_expr(condition.cond);
                const true_bb = try self.append_basicblock();
                const false_bb = try self.append_basicblock();
                const join_bb = try self.append_basicblock();
                const branch = try self.create_with(ir.BranchData, ir.BranchData{
                    .cond = cond_reg,
                    .true_branch = true_bb,
                    .false_branch = false_bb,
                });
                _ = try self.append_inst(ir.Instruction{ .branch = branch });

                self.current = true_bb;
                const true_reg = try self.compile_expr(condition.then_block);
                _ = try self.append_inst(ir.Instruction{ .jmp = join_bb });
                self.current = false_bb;

                const false_reg = if (condition.else_block) |else_block|
                    try self.compile_expr(else_block)
                else
                    try self.append_inst(ir.Instruction.nil);
                _ = try self.append_inst(ir.Instruction{ .jmp = join_bb });
                self.current = join_bb;

                // insert phony node that merges two result
                // of the condition
                const phony_ops = try self.permanent_alloc.alloc(ir.PhonyData.Pair, 2);
                phony_ops[0] = .{
                    .label = true_bb,
                    .reg = true_reg,
                };
                phony_ops[1] = .{
                    .label = false_bb,
                    .reg = false_reg,
                };
                
                const phony_data = try self.create_with(ir.PhonyData, ir.PhonyData { .data = phony_ops });
                return try self.append_inst(ir.Instruction { .phony = phony_data });
            },
            .bool => |value| if (value) {
                return try self.append_inst(ir.Instruction.true);
            } else {
                return try self.append_inst(ir.Instruction.false);
            },
            else => {
                std.debug.print("{}", .{expr});
                unreachable;
            },
        }
    }

    fn get_global(self: *const Compiler, var_name: []const u8) ?u32 {
        for (self.globals, 0..) |global, idx| {
            if (std.mem.eql(u8, global, var_name)) {
                return @intCast(idx);
            }
        }
        return null;
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

    pub fn create_inst(self: *Compiler, inst: ir.Instruction) !ir.InstructionIdx {
        return self.create_with(ir.Instruction, inst);
    }

    pub fn append_inst(self: *Compiler, inst: ir.Instruction) !ir.Reg {
        const inst_idx = try self.create_inst(inst);
        var bb = self.get_curr();
        try bb.instructions.append(self.permanent_alloc, inst_idx);
        return inst_idx;
    }

    pub fn append_basicblock(self: *Compiler) !ir.BasicBlockIdx {
        const bb_idx = try self.create(ir.BasicBlock);
        const curr_fn = self.get(ir.Function, self.fn_idx);
        try curr_fn.basicblocks.append(self.permanent_alloc, bb_idx);
        return bb_idx;
    }

    fn create_result(self: *const Compiler) CompiledResult {
        return self.result;
    }
};

test "basic" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn() = 1 + 2 - 3 * 4 + 4 / 2;
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    const globals: [][]const u8 = try allocator.alloc([]const u8, 0);
    const res = try ir_compile(function, metadata, globals, allocator);
    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0:
        \\    %0 = ldi 1
        \\    %1 = ldi 2
        \\    %2 = add %0, %1
        \\    %3 = ldi 3
        \\    %4 = ldi 4
        \\    %5 = mul %3, %4
        \\    %6 = sub %2, %5
        \\    %7 = ldi 4
        \\    %8 = ldi 2
        \\    %9 = div %7, %8
        \\    %10 = add %6, %9
        \\    ret %10
        \\}
        \\
    ).equal_fmt(res);
}

test "let" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn() = {
        \\     let x = 1;
        \\     let y = 2;
        \\     x + y + z;
        \\ };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    const globals: [][]const u8 = try allocator.alloc([]const u8, 0);
    const res = try ir_compile(function, metadata, globals, allocator);
    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0:
        \\    %0 = ldi 1
        \\    set_local 0, %0
        \\    %2 = get_local 0
        \\    %3 = ldi 2
        \\    set_local 1, %3
        \\    %5 = get_local 1
        \\    %6 = get_local 0
        \\    %7 = get_local 1
        \\    %8 = add %6, %7
        \\    %9 = load_env 0
        \\    %10 = add %8, %9
        \\    ret %10
        \\}
        \\
    ).equal_fmt(res);
}

test "condition" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn() = 
        \\     if (true) { 1; } else { 1 + 2; };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    const globals: [][]const u8 = try allocator.alloc([]const u8, 0);
    const res = try ir_compile(function, metadata, globals, allocator);
    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0:
        \\    %0 = ldi 1
        \\    set_local 0, %0
        \\    %2 = get_local 0
        \\    %3 = ldi 2
        \\    set_local 1, %3
        \\    %5 = get_local 1
        \\    %6 = get_local 0
        \\    %7 = get_local 1
        \\    %8 = add %6, %7
        \\    %9 = load_env 0
        \\    %10 = add %8, %9
        \\    ret %10
        \\}
        \\
    ).equal_fmt(res);
}
