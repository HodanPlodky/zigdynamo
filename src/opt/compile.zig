const std = @import("std");

const ast = @import("../ast.zig");
const ir = @import("ir.zig");
const Stores = @import("stores.zig").Stores;
const runtime = @import("../runtime.zig");
const MakeSSA = @import("passes/make_ssa.zig").MakeSSA;
const MakeCSSA = @import("passes/make_cssa.zig").MakeCSSA;
const CopyElimPass = @import("passes/copy_elim.zig").CopyElimination;
const MovElim = @import("passes/mov_elim.zig").MovElim;
const UnusedElim = @import("passes/unused_elim.zig").UnusedElim;
const PassBase = @import("passes/pass_base.zig").PassBase;
const AnalysisBase = @import("analysis/analysis_base.zig").AnalysisBase;
const SharedData = @import("analysis/analysis_base.zig").SharedData;
const SerializationPass = @import("passes/parcopy_serialization.zig").SerializationPass;
const OutOfSSAPass = @import("passes/outofssa.zig").OutOfSSAPass;
const CanonicalAnalysis = @import("analysis/canonical_regs_analysis.zig").CanonicalRegsAnalysis;

pub fn ir_compile(
    input: *const ast.Function,
    metadata: *const runtime.FunctionMetadata,
    globals: [][]const u8,
    alloc: std.mem.Allocator,
) !CompiledResult {
    // it would be probably better to have this survive across the calls
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();

    const scratch = scratch_arena.allocator();

    var compiler = try Compiler.init(globals, alloc, scratch);
    try compiler.compile(input, metadata);
    const shared_data = try SharedData.init(&compiler, alloc);
    try run_passes(&compiler, scratch, shared_data);
    _ = scratch_arena.reset(.retain_capacity);
    try outofssa(&compiler, alloc, scratch, shared_data);
    return compiler.create_result();
}

pub fn ir_compile_ssa(
    input: *const ast.Function,
    metadata: *const runtime.FunctionMetadata,
    globals: [][]const u8,
    alloc: std.mem.Allocator,
) !CompiledResult {
    // it would be probably better to have this survive across the calls
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();

    const scratch = scratch_arena.allocator();

    var compiler = try Compiler.init(globals, alloc, scratch);
    try compiler.compile(input, metadata);
    const shared_data = try SharedData.init(&compiler, alloc);
    try run_passes(&compiler, scratch, shared_data);
    return try compiler.create_result_ssa();
}

pub fn run_passes(compiler: *Compiler, alloc: std.mem.Allocator, shared_data: SharedData) !void {
    const passes: [3]type = .{
        MakeSSA,
        MovElim,
        UnusedElim,
    };

    const analysis_base = AnalysisBase{
        .compiler = compiler,
        .alloc = alloc,
        .shared_data = shared_data,
    };
    const pass_base = PassBase{
        .compiler = compiler,
        .alloc = alloc,
        .analysis_base = analysis_base,
    };

    inline for (passes) |pass_type| {
        var pass = try pass_type.init(pass_base);
        try pass.run();
    }
}

pub fn outofssa(compiler: *Compiler, perma_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator, shared_data: SharedData) !void {
    const analysis_base = AnalysisBase{
        .compiler = compiler,
        .alloc = scratch_alloc,
        .shared_data = shared_data,
    };
    const pass_base = PassBase{
        .compiler = compiler,
        .alloc = scratch_alloc,
        .analysis_base = analysis_base,
    };

    var make_cssa = try MakeCSSA.init(pass_base);
    try make_cssa.run();
    var canon = try CanonicalAnalysis.init(pass_base.analysis_base);
    canon.analyze();

    {
        var pass = try CopyElimPass.init(pass_base, canon);
        try pass.run();
    }
    {
        var pass = try UnusedElim.init(pass_base);
        try pass.run();
    }

    // after this pass the ir should not be considered in SSA form
    var out_pass = try OutOfSSAPass.init(pass_base, perma_alloc, canon);
    try out_pass.run();
    compiler.canonical_regs = out_pass.canonical_regs;

    {
        var pass = try SerializationPass.init(pass_base, canon);
        try pass.run();
    }
}

pub const CompiledResult = struct {
    entry_fn: ir.FunctionDistinct.Index,
    stores: Stores,
    canonical_regs: []ir.Reg,

    pub fn format(
        self: *const CompiledResult,
        writer: *std.io.Writer,
    ) !void {
        try self.write_fn(self.entry_fn, writer);
    }

    pub fn write_all_insts(self: *const CompiledResult, writer: anytype) !void {
        var iter = self.stores.idx_iter(ir.Instruction);
        while (iter.next()) |inst_idx| {
            try self.write_inst(inst_idx, writer);
        }
        try writer.print("\n", .{});
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
        const inst_type = self.stores.get_type(inst);
        if (inst_type == ir.Type.Void or std.meta.activeTag(inst) == .copy) {
            try writer.print("    {s}", .{inst.opcode()});
        } else {
            try writer.print("    %{} = {s}", .{
                self.canonical_regs[idx.get_usize()].get_usize(),
                inst.opcode(),
            });
        }
        try self.write_payload(inst, writer);
        try writer.print("\n", .{});
    }

    pub fn write_payload(self: *const CompiledResult, inst: ir.Instruction, writer: anytype) !void {
        switch (inst) {
            // immediate ops
            .ldi, .load_global, .arg, .load_env, .string => |num| try writer.print(" {}", .{num}),

            // stores
            .store_env, .store_global => |store_idx| {
                const data = self.stores.get(ir.StoreData, store_idx);
                try writer.print(" {}, %{}", .{ data.idx, data.value.index });
            },

            // empty
            .nil, .true, .false, .nop => {},

            // one reg ops
            .ret, .mov, .parallel_copy, .regify => |reg| try writer.print(" %{}", .{reg.index}),

            //  binop ops
            .add, .sub, .mul, .div, .lt, .gt => |binop_idx| {
                const binop = self.stores.get(ir.BinOpData, binop_idx);
                try writer.print(" %{}, %{}", .{ binop.left.index, binop.right.index });
            },
            .branch => |branch_idx| {
                const branch = self.stores.get(ir.BranchData, branch_idx);
                try writer.print(" %{}, basicblock{}, basicblock{}", .{
                    branch.cond.index,
                    branch.true_branch.index,
                    branch.false_branch.index,
                });
            },
            .jmp => |bb_idx| try writer.print(" {}", .{bb_idx.index}),
            .phony => |phony_idx| {
                const data = self.stores.get(ir.PhonyData, phony_idx);
                if (data.data.len > 0) {
                    try writer.print(" {} -> %{}", .{ data.data[0].label.index, data.data[0].reg.index });
                    for (data.data[1..]) |pair| {
                        try writer.print(", {} -> %{}", .{ pair.label.index, pair.reg.index });
                    }
                }
            },
            .get_local => |reg| try writer.print(" {}", .{reg}),
            .set_local => |set_local_idx| {
                const set_local = self.stores.get(ir.SetLocalData, set_local_idx);
                try writer.print(" {}, %{}", .{ set_local.local_idx, set_local.value.index });
            },
            .call => |call_idx| {
                const call = self.stores.get(ir.CallData, call_idx);
                try writer.print(" %{}(", .{call.target.index});
                if (call.args.len > 0) {
                    try writer.print("%{}", .{call.args[0].index});
                    for (call.args[1..]) |arg| {
                        try writer.print(", %{}", .{arg.index});
                    }
                }
                try writer.print(")", .{});
            },
            .print => |print_idx| {
                const print = self.stores.get(ir.PrintData, print_idx);
                if (print.args.len > 0) {
                    try writer.print(" %{}", .{print.args[0].index});
                    for (print.args[1..]) |arg| {
                        try writer.print(", %{}", .{arg.index});
                    }
                }
            },
            .copy => |copy_idx| {
                const copy = self.stores.get(ir.CopyData, copy_idx);
                try writer.print(" %{} <- %{}", .{ copy.dst.get_usize(), copy.src.get_usize() });
            },
            .closure => |closure_idx| {
                const closure = self.stores.get(ir.Closure, closure_idx);
                try writer.print(" {}(", .{closure.function_idx});
                if (closure.env.len > 0) {
                    try writer.print("%{}", .{closure.env[0].get_usize()});
                    for (closure.env[1..]) |env_var| {
                        try writer.print(", %{}", .{env_var.get_usize()});
                    }
                }
                try writer.print(")", .{});
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
    env_start: u32,

    fn init(alloc: std.mem.Allocator, env_start: u32) !Locals {
        var res = Locals{
            .alloc = alloc,
            .locals = .{},
            .env = .{},
            .curr_idx = 0,
            .env_idx = 0,
            .env_start = env_start,
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
        return self.env_idx - 1 + self.env_start;
    }

    fn get_env(self: *const Locals, var_name: []const u8) ?u32 {
        return if (self.env.get(var_name)) |res| res + self.env_start else null;
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

    // this is set after the out of ssa transformation
    // and is not used before that
    canonical_regs: []ir.Reg,

    pub fn init(globals: [][]const u8, permanent_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) !Compiler {
        return Compiler{
            .permanent_alloc = permanent_alloc,
            .scratch_alloc = scratch_alloc,
            .current = undefined,
            .stores = .{ .alloc = permanent_alloc },
            .locals = try Locals.init(scratch_alloc, 0),
            .globals = globals,
            .canonical_regs = undefined,
        };
    }

    pub fn compile(self: *Compiler, input: *const ast.Function, metadata: *const runtime.FunctionMetadata) !void {
        self.locals.env_start = input.env_start;
        self.entry_fn = try self.compile_fn(input, metadata);
    }

    pub fn create_empty(self: *Compiler) !ir.FunctionIdx {
        const builtin = @import("builtin");
        std.debug.assert(builtin.is_test);

        const bb_idx = try self.create(ir.BasicBlock);
        self.set_basicblock(bb_idx);
        const fn_idx = try self.create_with(ir.Function, try ir.Function.create(bb_idx, 0, self.permanent_alloc));
        self.fn_idx = fn_idx;

        return fn_idx;
    }

    fn compile_fn(self: *Compiler, function: *const ast.Function, metadata: *const runtime.FunctionMetadata) !ir.FunctionIdx {
        _ = metadata;
        const bb_idx = try self.create(ir.BasicBlock);
        self.set_basicblock(bb_idx);
        const fn_idx = try self.create_with(ir.Function, try ir.Function.create(bb_idx, function.env_start, self.permanent_alloc));
        self.fn_idx = fn_idx;

        for (function.params, 0..) |param_name, i| {
            const arg_reg = try self.append_inst(ir.Instruction{ .arg = @intCast(i) });
            const local_idx = try self.locals.set(param_name.value);

            const data = try self.create_with(ir.SetLocalData, ir.SetLocalData{
                .local_idx = local_idx,
                .value = arg_reg,
                .basicblock_idx = self.current,
            });
            _ = try self.append_inst(ir.Instruction{ .set_local = data });
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
            .string => |string| {
                return try self.append_inst(ir.Instruction{ .string = string.constant_idx });
            },
            .let => |let| {
                const value_reg = try self.compile_expr(let.value);
                const local_idx = try self.locals.set(let.target.value);
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
                if (self.locals.get(ident.value)) |local_idx| {
                    return try self.append_inst(ir.Instruction{ .get_local = local_idx });
                } else if (self.get_global(ident.value)) |global_idx| {
                    return try self.append_inst(ir.Instruction{ .load_global = global_idx });
                } else if (self.locals.get_env(ident.value)) |env_idx| {
                    return try self.append_inst(ir.Instruction{ .load_env = env_idx });
                } else {
                    const env_idx = try self.locals.set_env(ident.value);
                    return try self.append_inst(ir.Instruction{ .load_env = env_idx });
                }
            },
            .assign => |assign| {
                const val_reg = try self.compile_expr(assign.value);
                const place = try self.get_ident_place(assign.target.value);
                switch (place) {
                    .local => |idx| {
                        const data = try self.create_with(ir.SetLocalData, .{
                            .local_idx = idx,
                            .value = val_reg,
                            .basicblock_idx = self.current,
                        });
                        _ = try self.append_inst(.{ .set_local = data });
                    },
                    .global => |idx| {
                        const data = try self.create_with(ir.StoreData, .{ .idx = idx, .value = val_reg });
                        _ = try self.append_inst(.{ .store_global = data });
                    },
                    .env => |idx| {
                        const data = try self.create_with(ir.StoreData, .{ .idx = idx, .value = val_reg });
                        _ = try self.append_inst(.{ .store_env = data });
                    },
                }
                // it retuns value as expression result
                return val_reg;
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

                self.set_basicblock(true_bb);
                const true_reg = try self.compile_expr(condition.then_block);
                try self.append_terminator(ir.Instruction{ .jmp = join_bb });
                self.set_basicblock(false_bb);

                const false_reg = if (condition.else_block) |else_block|
                    try self.compile_expr(else_block)
                else
                    try self.append_inst(ir.Instruction.nil);
                try self.append_terminator(ir.Instruction{ .jmp = join_bb });
                self.set_basicblock(join_bb);

                // insert phony node that merges two result
                // of the condition
                const phony = try self.create_phony(true_bb, true_reg, false_bb, false_reg, null);
                return try self.append_inst(phony);
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
            .call => |call| {
                const args = try self.permanent_alloc.alloc(ir.Reg, call.args.len);
                for (call.args, 0..) |*arg, idx| {
                    args[idx] = try self.compile_expr(arg);
                }

                switch (call.target.*) {
                    .print_fn => {
                        const data = try self.create_with(ir.PrintData, .{ .args = args });
                        _ = try self.append_inst(.{ .print = data });

                        // the semantics of print is that it returns nil
                        return self.append_inst(.nil);
                    },
                    else => {
                        const target = try self.compile_expr(call.target);

                        const data = try self.create_with(ir.CallData, .{ .target = target, .args = args });
                        return self.append_inst(.{ .call = data });
                    },
                }
            },
            .function => |function| {
                const args = try self.permanent_alloc.alloc(ir.Reg, function.env_vars.len);
                for (function.env_vars, 0..) |env_var, idx| {
                    const tmp_ident: ast.Ast = .{ .ident = env_var };
                    args[idx] = try self.compile_expr(&tmp_ident);
                }

                const closure_idx = try self.create_with(ir.Closure, .{
                    .function_idx = function.function_idx,
                    .env = args,
                });

                return self.append_inst(.{ .closure = closure_idx });
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

    pub fn create_ssa_cannonical(self: *Compiler) !void {
        const inst_count = self.stores.get_max_idx(ir.Instruction);
        self.canonical_regs = try self.permanent_alloc.alloc(ir.Reg, inst_count.get_usize());

        var inst_iter = self.stores.idx_iter(ir.Instruction);
        while (inst_iter.next()) |inst_idx| {
            self.canonical_regs[inst_idx.get_usize()] = inst_idx;
        }
    }

    pub fn create_result(self: *const Compiler) CompiledResult {
        return CompiledResult{
            .entry_fn = self.entry_fn,
            .stores = self.stores,
            .canonical_regs = self.canonical_regs,
        };
    }

    pub fn create_result_ssa(self: *const Compiler) !CompiledResult {
        const inst_count = self.stores.get_max_idx(ir.Instruction);
        const canonical = try self.permanent_alloc.alloc(ir.Reg, inst_count.get_usize());

        var inst_iter = self.stores.idx_iter(ir.Instruction);
        while (inst_iter.next()) |inst_idx| {
            canonical[inst_idx.get_usize()] = inst_idx;
        }
        return CompiledResult{
            .entry_fn = self.entry_fn,
            .stores = self.stores,
            .canonical_regs = canonical,
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

    /// Returns canonical register that is selected as a name for the register after
    /// out of ssa translation (so this function assumes it is already after that)
    pub fn get_canonical_output(self: *const Compiler, inst_idx: ir.InstructionIdx) ir.Reg {
        const inst = self.get(ir.Instruction, inst_idx);
        return switch (inst) {
            .copy => |copy_idx| {
                const copy = self.get(ir.CopyData, copy_idx);
                return copy.dst;
            },
            else => self.canonical_regs[inst_idx.get_usize()],
        };
    }

    pub fn get_canon(self: *const Compiler, reg: ir.Reg) ir.Reg {
        return self.canonical_regs[reg.get_usize()];
    }

    pub fn dump_insts(self: *const Compiler) !void {
        var buffer: [1024]u8 = undefined;
        const tmp = try self.create_result_ssa();
        var stdout = std.fs.File.stderr().writer(&buffer);
        const writer = &stdout.interface;
        try tmp.write_all_insts(writer);
        try writer.flush();
    }

    pub fn dump_state(self: *const Compiler) !void {
        var buffer: [1024]u8 = undefined;
        const tmp = try self.create_result_ssa();
        var stdout = std.fs.File.stderr().writer(&buffer);
        const writer = &stdout.interface;
        try tmp.format(writer);
        try writer.flush();
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
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

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
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
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

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = ldi 1
        \\    %3 = ldi 2
        \\    %8 = add %0, %3
        \\    %9 = load_env 4294967295
        \\    %10 = add %8, %9
        \\    ret %10
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = ldi 1
        \\    %3 = ldi 2
        \\    %8 = add %0, %3
        \\    %9 = load_env 4294967295
        \\    %10 = add %8, %9
        \\    ret %10
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
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
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = true
        \\    branch %0, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %2 = ldi 1
        \\    %8 = regify %2
        \\    jmp 3
        \\basicblock2: [0]
        \\    %4 = ldi 1
        \\    %5 = ldi 2
        \\    %8 = add %4, %5
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    ret %8
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
}

test "condition2" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn() = {
        \\     let y = let x = 5;
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

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %5 = true
        \\    branch %5, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %7 = ldi 1
        \\    jmp 3
        \\basicblock2: [0]
        \\    %11 = ldi 1
        \\    %12 = ldi 2
        \\    %13 = add %11, %12
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    %20 = phony 1 -> %7, 2 -> %13
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %5 = true
        \\    branch %5, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %7 = ldi 1
        \\    %20 = regify %7
        \\    jmp 3
        \\basicblock2: [0]
        \\    %11 = ldi 1
        \\    %12 = ldi 2
        \\    %20 = add %11, %12
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
}

test "optimized loop" {
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

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = ldi 0
        \\    %3 = ldi 0
        \\    jmp 1
        \\basicblock1: [0, 2]
        \\    %25 = phony 0 -> %3, 2 -> %10
        \\    %24 = phony 0 -> %0, 2 -> %14
        \\    %19 = ldi 10
        \\    %20 = lt %24, %19
        \\    branch %20, basicblock2, basicblock3
        \\basicblock2: [1]
        \\    %10 = add %25, %24
        \\    %13 = ldi 1
        \\    %14 = add %24, %13
        \\    jmp 1
        \\basicblock3: [1]
        \\    ret %25
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = ldi 0
        \\    %3 = ldi 0
        \\    %25 = regify %3
        \\    %24 = regify %0
        \\    jmp 1
        \\basicblock1: [0, 2]
        \\    %19 = ldi 10
        \\    %20 = lt %24, %19
        \\    branch %20, basicblock2, basicblock3
        \\basicblock2: [1]
        \\    %25 = add %25, %24
        \\    %13 = ldi 1
        \\    %24 = add %24, %13
        \\    jmp 1
        \\basicblock3: [1]
        \\    ret %25
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
}

test "arg basic" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn(x, y) = x + 2 * y;
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = arg 1
        \\    %5 = ldi 2
        \\    %7 = mul %5, %2
        \\    %8 = add %0, %7
        \\    ret %8
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = arg 1
        \\    %5 = ldi 2
        \\    %7 = mul %5, %2
        \\    %8 = add %0, %7
        \\    ret %8
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
}

test "while fib opt compiler" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn(n) = {
        \\     let a = 0;
        \\     let b = 1;
        \\     while (n > 0) {
        \\         let tmp = a + b;
        \\         a = b;
        \\         b = tmp;
        \\         n = n - 1;
        \\     };
        \\     a;
        \\ };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = ldi 0
        \\    %5 = ldi 1
        \\    jmp 1
        \\basicblock1: [0, 2]
        \\    %33 = phony 0 -> %5, 2 -> %12
        \\    %32 = phony 0 -> %2, 2 -> %33
        \\    %31 = phony 0 -> %0, 2 -> %21
        \\    %26 = ldi 0
        \\    %27 = gt %31, %26
        \\    branch %27, basicblock2, basicblock3
        \\basicblock2: [1]
        \\    %12 = add %32, %33
        \\    %20 = ldi 1
        \\    %21 = sub %31, %20
        \\    jmp 1
        \\basicblock3: [1]
        \\    ret %32
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %31 = arg 0
        \\    %2 = ldi 0
        \\    %5 = ldi 1
        \\    %33 = regify %5
        \\    %32 = regify %2
        \\    jmp 1
        \\basicblock1: [0, 2]
        \\    %26 = ldi 0
        \\    %27 = gt %31, %26
        \\    branch %27, basicblock2, basicblock3
        \\basicblock2: [1]
        \\    %12 = add %32, %33
        \\    %20 = ldi 1
        \\    %31 = sub %31, %20
        \\    copy %32 <- %33
        \\    copy %33 <- %12
        \\    jmp 1
        \\basicblock3: [1]
        \\    ret %32
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
}

test "globals opt compile" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn(n) = {
        \\     let tmp = g;
        \\     g = n;
        \\     tmp;
        \\ };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    var globals: [1][]const u8 = .{"g"};
    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = load_global 0
        \\    store_global 0, %0
        \\    ret %2
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, globals[0..], allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = load_global 0
        \\    store_global 0, %0
        \\    ret %2
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, globals[0..], allocator));
}

test "call opt compiler" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn(n) = {
        \\     g(n + 1, 2);
        \\     g(g(n * 2, 1), 1);
        \\     g(1, n);
        \\ };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    var globals: [1][]const u8 = .{"g"};
    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %3 = ldi 1
        \\    %4 = add %0, %3
        \\    %5 = ldi 2
        \\    %6 = load_global 0
        \\    %7 = call %6(%4, %5)
        \\    %9 = ldi 2
        \\    %10 = mul %0, %9
        \\    %11 = ldi 1
        \\    %12 = load_global 0
        \\    %13 = call %12(%10, %11)
        \\    %14 = ldi 1
        \\    %15 = load_global 0
        \\    %16 = call %15(%13, %14)
        \\    %17 = ldi 1
        \\    %19 = load_global 0
        \\    %20 = call %19(%17, %0)
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, globals[0..], allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %3 = ldi 1
        \\    %4 = add %0, %3
        \\    %5 = ldi 2
        \\    %6 = load_global 0
        \\    %7 = call %6(%4, %5)
        \\    %9 = ldi 2
        \\    %10 = mul %0, %9
        \\    %11 = ldi 1
        \\    %12 = load_global 0
        \\    %13 = call %12(%10, %11)
        \\    %14 = ldi 1
        \\    %15 = load_global 0
        \\    %16 = call %15(%13, %14)
        \\    %17 = ldi 1
        \\    %19 = load_global 0
        \\    %20 = call %19(%17, %0)
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, globals[0..], allocator));
}

test "fib recursive opt compile" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn(n) = {
        \\     if (n < 2) n
        \\     else fib(n - 1) + fib(n - 2);
        \\ };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    var globals: [1][]const u8 = .{"fib"};
    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %3 = ldi 2
        \\    %4 = lt %0, %3
        \\    branch %4, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    jmp 3
        \\basicblock2: [0]
        \\    %9 = ldi 1
        \\    %10 = sub %0, %9
        \\    %11 = load_global 0
        \\    %12 = call %11(%10)
        \\    %14 = ldi 2
        \\    %15 = sub %0, %14
        \\    %16 = load_global 0
        \\    %17 = call %16(%15)
        \\    %18 = add %12, %17
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    %20 = phony 1 -> %0, 2 -> %18
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, globals[0..], allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %20 = arg 0
        \\    %3 = ldi 2
        \\    %4 = lt %20, %3
        \\    branch %4, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    jmp 3
        \\basicblock2: [0]
        \\    %9 = ldi 1
        \\    %10 = sub %20, %9
        \\    %11 = load_global 0
        \\    %12 = call %11(%10)
        \\    %14 = ldi 2
        \\    %15 = sub %20, %14
        \\    %16 = load_global 0
        \\    %17 = call %16(%15)
        \\    %20 = add %12, %17
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, globals[0..], allocator));
}

test "conditions vars overlaps more" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ fn(n) = {
        \\     let x = 1;
        \\     let y = if (n) {
        \\         x + 1;
        \\     }
        \\     else { 
        \\         x + 2;
        \\     };
        \\     y + x;
        \\ };
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = ldi 1
        \\    branch %0, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %8 = ldi 1
        \\    %9 = add %2, %8
        \\    jmp 3
        \\basicblock2: [0]
        \\    %12 = ldi 2
        \\    %13 = add %2, %12
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    %15 = phony 1 -> %9, 2 -> %13
        \\    %20 = add %15, %2
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile_ssa(function, &metadata, &.{}, allocator));

    try snap.Snap.init(@src(),
        \\function {
        \\basicblock0: []
        \\    %0 = arg 0
        \\    %2 = ldi 1
        \\    branch %0, basicblock1, basicblock2
        \\basicblock1: [0]
        \\    %8 = ldi 1
        \\    %15 = add %2, %8
        \\    jmp 3
        \\basicblock2: [0]
        \\    %12 = ldi 2
        \\    %15 = add %2, %12
        \\    jmp 3
        \\basicblock3: [1, 2]
        \\    %20 = add %15, %2
        \\    ret %20
        \\}
        \\
    ).equal_fmt(try ir_compile(function, &metadata, &.{}, allocator));
}

test "opt compiler basic closure" {
    const Parser = @import("../parser.zig").Parser;
    const snap = @import("../snap.zig");
    const compiler = @import("../compiler.zig");
    const bc = @import("../bc_interpreter.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input =
        \\ let inc = fn(n) = fn(x) = n + x;
        \\ let inc1 = inc(1);
        \\ inc1(2);
    ;

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();
    const bytecode = try compiler.compile(parse_res, allocator);
    var writer = std.io.Writer.Allocating.init(std.testing.allocator);

    // does not have to run
    const inter = bc.OptJitInterpreter.init(
        allocator,
        bytecode,
        try allocator.allocWithOptions(u8, 1024, std.mem.Alignment.@"16", null),
        &writer.writer,
        .{},
    );

    try std.testing.expectEqual(bytecode.functions.sources.len, 2);
    {
        const source = bytecode.functions.sources[0];
        var meta = inter.function_meta[1];
        try snap.Snap.init(@src(),
            \\function {
            \\basicblock0: []
            \\    %0 = arg 0
            \\    %2 = load_env 1
            \\    %4 = add %2, %0
            \\    ret %4
            \\}
            \\
        ).equal_fmt(try ir_compile(source, &meta, bytecode.globals, allocator));
    }
    {
        const source = bytecode.functions.sources[1];
        var meta = inter.function_meta[2];
        try snap.Snap.init(@src(),
            \\function {
            \\basicblock0: []
            \\    %0 = arg 0
            \\    %3 = closure 1(%0)
            \\    ret %3
            \\}
            \\
        ).equal_fmt(try ir_compile(source, &meta, bytecode.globals, allocator));
    }
}
