const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const Stores = @import("stores.zig").Stores;
const runtime = @import("../runtime.zig");
const MakeSSA = @import("passes/make_ssa.zig").MakeSSA;
const PassBase = @import("passes/pass_base.zig").PassBase;
const AnalysisBase = @import("analysis/analysis_base.zig").AnalysisBase;

pub fn ir_compile(input: *const ast.Function, metadata: runtime.FunctionMetadata, globals: [][]const u8, alloc: std.mem.Allocator) !CompiledResult {
    // it would be probably better to have this survive across the calls
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();

    const scratch = scratch_arena.allocator();

    var compiler = try Compiler.init(globals, alloc, scratch);
    try compiler.compile(input, metadata);
    try run_passes(&compiler, scratch);
    return compiler.create_result();
}

pub fn run_passes(compiler: *Compiler, alloc: std.mem.Allocator) !void {
    const passes: [1]type = .{
        MakeSSA,
    };

    for (0..compiler.stores.function.data.items.len) |i| {
        const fn_idx = ir.FunctionIdx.new(@intCast(i));
        const analysis_base = AnalysisBase{
            .compiler = compiler,
            .alloc = alloc,
            .function_idx = fn_idx,
        };
        const pass_base = PassBase{
            .compiler = compiler,
            .alloc = alloc,
            .function_idx = fn_idx,
            .analysis_base = analysis_base,
        };

        inline for (passes) |pass_type| {
            var pass = try pass_type.init(pass_base, compiler.locals.curr_idx);
            try pass.run();
        }
    }
}

pub const CompiledResult = struct {
    entry_fn: ir.FunctionDistinct.Index,
    stores: Stores,

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
        const function: ir.Function = self.stores.get(ir.Function, idx);
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
        const basicblock = self.stores.get(ir.BasicBlock, idx);
        try writer.print("basicblock{}: [", .{idx.index});
        if (basicblock.predecessors.items.len > 0) {
            try writer.print("{}", .{basicblock.predecessors.items[0].index});
            for (basicblock.predecessors.items[1..]) |pred| {
                try writer.print(", {}", .{pred.index});
            }
        }
        try writer.print("]\n", .{});
        for (basicblock.instructions.items) |inst_idx| {
            try self.write_inst(inst_idx, writer);
        }
    }

    pub fn write_inst(self: *const CompiledResult, idx: ir.InstructionIdx, writer: anytype) !void {
        const inst = self.stores.get(ir.Instruction, idx);
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
                const src_inst = self.stores.get(ir.Instruction, reg);
                return self.get_type(src_inst);
            },
            .nil => ir.Type.Nil,
            .true => ir.Type.True,
            .false => ir.Type.False,
            .load_global => ir.Type.Top,
            .store_global => ir.Type.Void,
            .load_env => ir.Type.Top,
            .store_env => ir.Type.Void,
            .add, .sub, .mul, .div, .lt, .gt => ir.Type.Top,
            .ret, .branch, .jmp => ir.Type.Void,
            .arg => ir.Type.Top,
            .nop => ir.Type.Void,
            // TODO use join
            .phony => ir.Type.Top,
            .get_local => ir.Type.Top,
            .set_local => ir.Type.Void,
        };
    }

    pub fn write_payload(self: *const CompiledResult, inst: ir.Instruction, writer: anytype) !void {
        switch (inst) {
            // immediate ops
            .ldi, .load_global, .arg, .load_env => |num| try writer.print("{}", .{num}),

            // TODO
            .store_env, .store_global => |store_idx| {
                const data = self.stores.get(ir.StoreData, store_idx);
                try writer.print("{}, %{}", .{ data.idx, data.value.index });
            },

            // empty
            .nil, .true, .false, .nop => {},

            // one reg ops
            .ret, .mov => |reg| try writer.print("%{}", .{reg.index}),

            //  binop ops
            .add, .sub, .mul, .div, .lt, .gt => |binop_idx| {
                const binop = self.stores.get(ir.BinOpData, binop_idx);
                try writer.print("%{}, %{}", .{ binop.left.index, binop.right.index });
            },
            .branch => |branch_idx| {
                const branch = self.stores.get(ir.BranchData, branch_idx);
                try writer.print("%{}, basicblock{}, basicblock{}", .{
                    branch.cond.index,
                    branch.true_branch.index,
                    branch.false_branch.index,
                });
            },
            .jmp => |bb_idx| try writer.print("{}", .{bb_idx.index}),
            .phony => |phony_idx| {
                const data = self.stores.get(ir.PhonyData, phony_idx);
                if (data.data.len > 0) {
                    try writer.print("{} -> %{}", .{ data.data[0].label.index, data.data[0].reg.index });
                    for (data.data[1..]) |pair| {
                        try writer.print(", {} -> %{}", .{ pair.label.index, pair.reg.index });
                    }
                }
            },
            .get_local => |reg| try writer.print("{}", .{reg}),
            .set_local => |set_local_idx| {
                const set_local = self.stores.get(ir.SetLocalData, set_local_idx);
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

pub const Compiler = struct {
    permanent_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    entry_fn: ir.FunctionDistinct.Index = undefined,
    stores: Stores,
    current: ir.BasicBlockIdx,
    locals: Locals,
    globals: [][]const u8,
    fn_idx: ir.FunctionDistinct.Index = undefined,

    pub fn init(globals: [][]const u8, permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) !Compiler {
        return Compiler{
            .permanent_alloc = permanent_alloc,
            .scratch_alloc = scratch_alloc,
            .current = undefined,
            .stores = .{ .alloc = permanent_alloc },
            .locals = try Locals.init(scratch_alloc),
            .globals = globals,
        };
    }

    fn compile(self: *Compiler, input: *const ast.Function, metadata: runtime.FunctionMetadata) !void {
        self.entry_fn = try self.compile_fn(input, metadata);
    }

    pub fn create_empty(self: *Compiler) !ir.FunctionIdx {
        const builtin = @import("builtin");
        std.debug.assert(builtin.is_test);

        const bb_idx = try self.create(ir.BasicBlock);
        self.current = bb_idx;
        const fn_idx = try self.create_with(ir.Function, try ir.Function.create(bb_idx, self.permanent_alloc));
        self.fn_idx = fn_idx;

        return fn_idx;
    }

    fn compile_fn(self: *Compiler, function: *const ast.Function, metadata: runtime.FunctionMetadata) !ir.FunctionIdx {
        _ = metadata;
        const bb_idx = try self.create(ir.BasicBlock);
        self.current = bb_idx;
        const fn_idx = try self.create_with(ir.Function, try ir.Function.create(bb_idx, self.permanent_alloc));
        self.fn_idx = fn_idx;

        for (function.params, 0..) |_, i| {
            _ = try self.append_inst(ir.Instruction{ .arg = @intCast(i) });
        }

        const ret_reg = try self.compile_expr(function.body);
        try self.append_terminator(ir.Instruction{ .ret = ret_reg });

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
                    '+' => try self.append_inst(.{ .add = binop_data }),
                    '-' => try self.append_inst(.{ .sub = binop_data }),
                    '*' => try self.append_inst(.{ .mul = binop_data }),
                    '/' => try self.append_inst(.{ .div = binop_data }),
                    '<' => try self.append_inst(.{ .lt = binop_data }),
                    '>' => try self.append_inst(.{ .gt = binop_data }),
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
                    .basicblock_idx = self.current,
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
            .assign => |assign| {
                const val_reg = try self.compile_expr(assign.value);
                const place = try self.get_ident_place(assign.target);
                switch (place) {
                    .local => |idx| {
                        const data = try self.create_with(ir.SetLocalData, .{
                            .local_idx = idx,
                            .value = val_reg,
                            .basicblock_idx = self.current,
                        });
                        return try self.append_inst(.{ .set_local = data });
                    },
                    .global => |idx| {
                        const data = try self.create_with(ir.StoreData, .{ .idx = idx, .value = val_reg });
                        return try self.append_inst(.{ .store_global = data });
                    },
                    .env => |idx| {
                        const data = try self.create_with(ir.StoreData, .{ .idx = idx, .value = val_reg });
                        return try self.append_inst(.{ .store_env = data });
                    },
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
                try self.append_terminator(ir.Instruction{ .branch = branch });

                self.current = true_bb;
                const true_reg = try self.compile_expr(condition.then_block);
                try self.append_terminator(ir.Instruction{ .jmp = join_bb });
                self.current = false_bb;

                const false_reg = if (condition.else_block) |else_block|
                    try self.compile_expr(else_block)
                else
                    try self.append_inst(ir.Instruction.nil);
                try self.append_terminator(ir.Instruction{ .jmp = join_bb });
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

                const phony_data = try self.create_with(ir.PhonyData, ir.PhonyData{ .data = phony_ops });
                return try self.append_inst(ir.Instruction{ .phony = phony_data });
            },
            .loop => |loop| {
                const before_bb_idx = self.current;
                const cond_bb_idx = try self.append_basicblock();
                const body_bb_idx = try self.append_basicblock();
                const after_bb_idx = try self.append_basicblock();
                const default_ret = try self.append_inst(.nil);
                try self.append_terminator(.{ .jmp = cond_bb_idx });

                self.set_basicblock(body_bb_idx);
                const body_ret = try self.compile_expr(loop.body);
                try self.append_terminator(.{ .jmp = cond_bb_idx });

                self.set_basicblock(cond_bb_idx);
                const phony = try self.create_phony(before_bb_idx, default_ret, body_bb_idx, body_ret, null);
                const phony_reg = try self.append_inst(phony);
                const cond_reg = try self.compile_expr(loop.cond);
                const branch_data = try self.create_with(ir.BranchData, .{
                    .cond = cond_reg,
                    .true_branch = body_bb_idx,
                    .false_branch = after_bb_idx,
                });
                try self.append_terminator(.{ .branch = branch_data });
                self.set_basicblock(after_bb_idx);

                return phony_reg;
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

    const IdentPlace = union(enum) {
        local: u32,
        global: u32,
        env: u32,
    };

    fn get_ident_place(self: *Compiler, var_name: []const u8) !IdentPlace {
        if (self.locals.get(var_name)) |local_idx| {
            return .{ .local = local_idx };
        } else if (self.get_global(var_name)) |global_idx| {
            return .{ .global = global_idx };
        } else if (self.locals.get_env(var_name)) |env_idx| {
            return .{ .env = env_idx };
        } else {
            const env_idx = try self.locals.set_env(var_name);
            return .{ .env = env_idx };
        }
    }

    pub fn get_curr(self: *Compiler) *ir.BasicBlock {
        return self.get_ptr(ir.BasicBlock, self.current);
    }

    pub fn create_with(self: *Compiler, comptime T: type, value: T) !Stores.get_index_type(T) {
        return self.stores.create_with(T, value);
    }

    pub fn create(self: *Compiler, comptime T: type) !Stores.get_index_type(T) {
        return self.stores.create_with(T, T{});
    }

    pub fn get_ptr(self: *Compiler, comptime T: type, index: Stores.get_index_type(T)) *T {
        return self.stores.get_ptr(T, index);
    }

    pub fn get_const_ptr(self: *const Compiler, comptime T: type, index: Stores.get_index_type(T)) *const T {
        return self.stores.get_const_ptr(T, index);
    }

    pub fn get(self: *const Compiler, comptime T: type, index: Stores.get_index_type(T)) T {
        return self.stores.get(T, index);
    }

    pub fn set(self: *Compiler, comptime T: type, index: Stores.get_index_type(T), value: T) void {
        return self.stores.set(T, index, value);
    }

    pub fn create_inst(self: *Compiler, inst: ir.Instruction) !ir.InstructionIdx {
        return self.stores.create_with(ir.Instruction, inst);
    }

    pub fn append_inst(self: *Compiler, inst: ir.Instruction) !ir.Reg {
        std.debug.assert(!inst.is_terminator());
        const inst_idx = try self.create_inst(inst);
        var bb = self.get_curr();
        try bb.instructions.append(self.permanent_alloc, inst_idx);
        return inst_idx;
    }

    pub fn insert_inst(self: *Compiler, bb_idx: ir.BasicBlockIdx, inst: ir.Instruction, index: usize) !ir.Reg {
        std.debug.assert(!inst.is_terminator());
        const inst_idx = try self.create_inst(inst);
        const bb = self.get_ptr(ir.BasicBlock, bb_idx);
        try bb.instructions.insert(self.permanent_alloc, index, inst_idx);
        return inst_idx;
    }

    pub fn append_terminator(self: *Compiler, inst: ir.Instruction) !void {
        std.debug.assert(inst.is_terminator());
        const inst_idx = try self.create_inst(inst);
        var bb = self.get_curr();
        try bb.instructions.append(self.permanent_alloc, inst_idx);

        // add predecesors
        var succesors = self.get_succesors(self.current);
        while (succesors.next()) |succ_idx| {
            const succesor = self.get_ptr(ir.BasicBlock, succ_idx);
            try succesor.predecessors.append(self.permanent_alloc, self.current);
        }
    }

    pub fn append_basicblock(self: *Compiler) !ir.BasicBlockIdx {
        const bb_idx = try self.create(ir.BasicBlock);
        const curr_fn = self.get_ptr(ir.Function, self.fn_idx);
        try curr_fn.basicblocks.append(self.permanent_alloc, bb_idx);
        return bb_idx;
    }

    pub fn create_phony(
        self: *Compiler,
        label_a: ir.BasicBlockIdx,
        a: ir.Reg,
        label_b: ir.BasicBlockIdx,
        b: ir.Reg,
        origin: ?u32,
    ) !ir.Instruction {
        const phony_ops = try self.permanent_alloc.alloc(ir.PhonyData.Pair, 2);
        phony_ops[0] = .{
            .label = label_a,
            .reg = a,
        };
        phony_ops[1] = .{
            .label = label_b,
            .reg = b,
        };

        const data = try self.create_with(ir.PhonyData, .{ .data = phony_ops, .origin = origin });

        return .{ .phony = data };
    }

    // TODO fix places where this is not used
    pub fn set_basicblock(self: *Compiler, bb_idx: ir.BasicBlockIdx) void {
        self.current = bb_idx;
    }

    fn create_result(self: *const Compiler) CompiledResult {
        return CompiledResult{
            .entry_fn = self.entry_fn,
            .stores = self.stores,
        };
    }

    pub fn get_succesors(self: *const Compiler, bb_idx: ir.BasicBlockIdx) ir.Instruction.LabelIterator {
        const LabelIter = ir.Instruction.LabelIterator;
        const bb = self.stores.get_const_ptr(ir.BasicBlock, bb_idx);
        const last_inst_idx = bb.instructions.items[bb.instructions.items.len - 1];
        const last_inst = self.get(ir.Instruction, last_inst_idx);
        std.debug.assert(last_inst.is_terminator());
        switch (last_inst) {
            .jmp => |next_idx| return LabelIter.create_one(next_idx),
            .branch => |branch_idx| {
                const branch = self.get(ir.BranchData, branch_idx);
                return LabelIter.create_two(branch.true_branch, branch.false_branch);
            },
            .ret => |_| return LabelIter.create_zero(),
            else => unreachable,
        }
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
        \\basicblock0: []
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
        \\basicblock0: []
        \\    %0 = ldi 1
        \\    %1 = mov %0
        \\    %2 = mov %1
        \\    %3 = ldi 2
        \\    %4 = mov %3
        \\    %5 = mov %4
        \\    %6 = mov %1
        \\    %7 = mov %4
        \\    %8 = add %6, %7
        \\    %9 = load_env 0
        \\    %10 = add %8, %9
        \\    ret %10
        \\}
        \\
    ).equal_fmt(res);
}

test "condition1" {
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
        \\basicblock0: []
        \\    %0 = true 
        \\    branch %0, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %2 = ldi 1
        \\    jmp 3
        \\basicblock2: [0]
        \\    %4 = ldi 1
        \\    %5 = ldi 2
        \\    %6 = add %4, %5
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    %8 = phony 1 -> %2, 2 -> %6
        \\    ret %8
        \\}
        \\
    ).equal_fmt(res);
}

test "condition2" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn() = {
        \\     let x = 5;
        \\     if (true) { x = 1; 1; } else { x = 1 + 2; 2;};
        \\     x;
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
        \\basicblock0: []
        \\    %0 = ldi 5
        \\    %1 = mov %0
        \\    %2 = mov %1
        \\    %3 = true 
        \\    branch %3, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %5 = ldi 1
        \\    %6 = mov %5
        \\    %7 = ldi 1
        \\    jmp 3
        \\basicblock2: [0]
        \\    %9 = ldi 1
        \\    %10 = ldi 2
        \\    %11 = add %9, %10
        \\    %12 = mov %11
        \\    %13 = ldi 2
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    %18 = phony 1 -> %6, 2 -> %12
        \\    %15 = phony 1 -> %7, 2 -> %13
        \\    %16 = mov %18
        \\    ret %16
        \\}
        \\
    ).equal_fmt(res);
}

test "loop" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn() = {
        \\     let x = 0;
        \\     let res = 0;
        \\     while(x < 10) {
        \\         res = res + x;
        \\         x = x + 1;
        \\     };
        \\     res;
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
        \\basicblock0: []
        \\    %0 = ldi 0
        \\    %1 = mov %0
        \\    %2 = mov %1
        \\    %3 = ldi 0
        \\    %4 = mov %3
        \\    %5 = mov %4
        \\    %6 = nil 
        \\    jmp 1
        \\basicblock1: [0, 2]
        \\    %25 = phony 0 -> %4, 2 -> %11
        \\    %24 = phony 0 -> %1, 2 -> %15
        \\    %17 = phony 0 -> %6, 2 -> %15
        \\    %18 = mov %24
        \\    %19 = ldi 10
        \\    %20 = lt %18, %19
        \\    branch %20, basicblock2, basicblock3
        \\basicblock2: [1]
        \\    %8 = mov %25
        \\    %9 = mov %24
        \\    %10 = add %8, %9
        \\    %11 = mov %10
        \\    %12 = mov %24
        \\    %13 = ldi 1
        \\    %14 = add %12, %13
        \\    %15 = mov %14
        \\    jmp 1
        \\basicblock3: [1]
        \\    %22 = mov %25
        \\    ret %22
        \\}
        \\
    ).equal_fmt(res);
}
