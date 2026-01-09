const std = @import("std");
const CompiledResult = @import("compile.zig").CompiledResult;
const runtime = @import("../runtime.zig");
const ir = @import("ir.zig");

/// Interpreter of SSA code
/// this is for debug purpouses
const Interpreter = struct {
    code: CompiledResult,
    globals: []runtime.Value,
    env: []runtime.Value,
    regs: []runtime.Value,
    alloc: std.mem.Allocator,
    args: []runtime.Value = undefined,

    pub fn init(
        code: CompiledResult,
        globals: []runtime.Value,
        env: []runtime.Value,
        alloc: std.mem.Allocator,
    ) !Interpreter {
        const inst_count = code.stores.get_max_idx(ir.Instruction);
        const regs = try alloc.alloc(runtime.Value, inst_count.get_usize());
        @memset(regs, runtime.Value.new_nil());
        return Interpreter{
            .code = code,
            .globals = globals,
            .env = env,
            .regs = regs,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.alloc.free(self.regs);
    }

    pub fn run(self: *Interpreter, args: []runtime.Value) runtime.Value {
        const start = self.code.entry_fn;
        const function = self.code.stores.get(ir.Function, start);
        self.args = args;
        return self.run_fn(function);
    }

    fn run_fn(self: *Interpreter, function: ir.Function) runtime.Value {
        var before: ?ir.BasicBlockIdx = null;
        var curr_idx = function.entry;

        while (true) {
            const curr_bb = self.code.stores.get(ir.BasicBlock, curr_idx);
            for (curr_bb.instructions.items, 0..) |inst_idx, idx| {
                const inst = self.code.stores.get(ir.Instruction, inst_idx);
                const reg = inst_idx;

                switch (inst) {
                    .ldi => |num| self.set_reg(reg, runtime.Value.new_num(num)),

                    // bit different semantics but oh well
                    .mov, .parallel_copy, .regify => |src_reg| self.set_reg(reg, self.get_reg(src_reg)),

                    // should not be in ssa but good to test eitherway
                    .copy => |copy_idx| {
                        const copy = self.code.stores.get(ir.CopyData, copy_idx);
                        self.set_reg(copy.dst, self.get_reg(copy.src));
                    },
                    .nil => self.set_reg(reg, runtime.Value.new_nil()),
                    .true => self.set_reg(reg, runtime.Value.new_bool(true)),
                    .false => self.set_reg(reg, runtime.Value.new_bool(false)),
                    .load_global => |num| {
                        self.set_reg(reg, self.globals[@intCast(num)]);
                    },
                    .store_global => |store_idx| {
                        const store = self.code.stores.get(ir.StoreData, store_idx);
                        const val = self.get_reg(store.value);
                        self.globals[@intCast(store.idx)] = val;
                    },
                    .load_env => |num| self.set_reg(reg, self.env[@intCast(num)]),
                    .store_env => |store_idx| {
                        const store = self.code.stores.get(ir.StoreData, store_idx);
                        const val = self.get_reg(store.value);
                        self.env[@intCast(store.idx)] = val;
                    },
                    .add => |binop_idx| self.run_binop(
                        inst_idx,
                        self.code.stores.get(ir.BinOpData, binop_idx),
                        runtime.Value.add,
                    ),
                    .sub => |binop_idx| self.run_binop(
                        inst_idx,
                        self.code.stores.get(ir.BinOpData, binop_idx),
                        runtime.Value.sub,
                    ),
                    .mul => |binop_idx| self.run_binop(
                        inst_idx,
                        self.code.stores.get(ir.BinOpData, binop_idx),
                        runtime.Value.mul,
                    ),
                    .div => |binop_idx| self.run_binop(
                        inst_idx,
                        self.code.stores.get(ir.BinOpData, binop_idx),
                        runtime.Value.div,
                    ),
                    .lt => |binop_idx| self.run_binop(
                        inst_idx,
                        self.code.stores.get(ir.BinOpData, binop_idx),
                        runtime.Value.lt,
                    ),
                    .gt => |binop_idx| self.run_binop(
                        inst_idx,
                        self.code.stores.get(ir.BinOpData, binop_idx),
                        runtime.Value.gt,
                    ),
                    .ret => |ret_reg| {
                        std.debug.assert(idx == curr_bb.instructions.items.len - 1);
                        return self.get_reg(ret_reg);
                    },
                    .branch => |branch_idx| {
                        std.debug.assert(idx == curr_bb.instructions.items.len - 1);
                        before = curr_idx;
                        const branch = self.code.stores.get(ir.BranchData, branch_idx);
                        const cond = self.get_reg(branch.cond);
                        switch (cond.get_type()) {
                            runtime.ValueType.true => curr_idx = branch.true_branch,
                            runtime.ValueType.false => curr_idx = branch.false_branch,
                            else => @panic("If condition must be boolean"),
                        }
                        break;
                    },
                    .jmp => |label| {
                        std.debug.assert(idx == curr_bb.instructions.items.len - 1);
                        before = curr_idx;
                        curr_idx = label;
                        break;
                    },
                    .arg => |num| self.set_reg(reg, self.args[@intCast(num)]),
                    .nop => {},
                    .phony => |phony_idx| {
                        const phony = self.code.stores.get(ir.PhonyData, phony_idx);
                        var res: ?ir.Reg = null;
                        for (phony.data) |pair| {
                            if (pair.label.index == before.?.index) {
                                res = pair.reg;
                            }
                        }

                        self.set_reg(reg, self.get_reg(res.?));
                    },
                    .call => unreachable,
                    .get_local, .set_local => @panic("after passes this should not be here"),
                }
            }
        }
    }

    fn run_binop(
        self: *Interpreter,
        inst_idx: ir.InstructionIdx,
        binop: ir.BinOpData,
        comptime oper: fn (runtime.Value, runtime.Value) runtime.Value,
    ) void {
        const left = self.get_reg(binop.left);
        const right = self.get_reg(binop.right);
        if (left.get_type() == runtime.ValueType.number and right.get_type() == runtime.ValueType.number) {
            self.set_reg(inst_idx, oper(left, right));
        } else {
            std.debug.print("left: {f}, right: {f}\n", .{ left, right });
            @panic("Unimplemented dispatch");
        }
    }

    fn get_reg(self: *const Interpreter, reg: ir.Reg) runtime.Value {
        const canon = self.code.canonical_regs[reg.get_usize()];
        return self.regs[canon.get_usize()];
    }

    fn set_reg(self: *const Interpreter, reg: ir.Reg, value: runtime.Value) void {
        const canon = self.code.canonical_regs[reg.get_usize()];
        self.regs[canon.get_usize()] = value;
    }
};

fn test_helper(
    input: []const u8,
    args: []runtime.Value,
    global_names: [][]const u8,
    globals: []runtime.Value,
) !runtime.Value {
    return try test_helper_inner(input, args, global_names, globals, false);
}

fn test_helper_inner(
    input: []const u8,
    args: []runtime.Value,
    global_names: [][]const u8,
    globals: []runtime.Value,
    comptime ignore_ssa: bool,
) !runtime.Value {
    const Parser = @import("../parser.zig").Parser;
    const ir_compile = @import("compile.zig").ir_compile;
    const ir_compile_ssa = @import("compile.zig").ir_compile_ssa;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(input, allocator);
    const parse_res = try p.parse();

    // get first function
    const node = parse_res.data[0];

    // first should be function
    const function = &node.function;
    const metadata = runtime.FunctionMetadata{};

    if (!ignore_ssa) {
        const ssa_code = try ir_compile_ssa(function, &metadata, global_names, allocator);

        var interpret_ssa = try Interpreter.init(ssa_code, globals, &.{}, allocator);
        const ssa_result = interpret_ssa.run(args);

        const final_ir_code = try ir_compile(function, &metadata, global_names, allocator);

        var interpret = try Interpreter.init(final_ir_code, globals, &.{}, allocator);

        const final_result = interpret.run(args);
        try std.testing.expectEqualDeep(ssa_result, final_result);

        return final_result;
    } else {
        const final_ir_code = try ir_compile(function, &metadata, global_names, allocator);

        var interpret = try Interpreter.init(final_ir_code, globals, &.{}, allocator);

        const final_result = interpret.run(args);

        return final_result;
    }
}

test "basic" {
    const input =
        \\ fn() = 1 + 2 * 3;
    ;

    const ret = try test_helper(input, &.{}, &.{}, &.{});
    try std.testing.expectEqual(ret.get_number(), 7);
}

test "let" {
    const input =
        \\ fn() = {
        \\     let x = 1;
        \\     let y = 2;
        \\     x = x + 1;
        \\     x + y;
        \\ };
    ;

    const ret = try test_helper(input, &.{}, &.{}, &.{});
    try std.testing.expectEqual(ret.get_number(), 4);
}

test "max function" {
    const input =
        \\ fn(a, b) = if (a < b) b else a;
    ;

    {
        var args: [2]runtime.Value = .{
            runtime.Value.new_num(2),
            runtime.Value.new_num(3),
        };
        const ret = try test_helper(input, args[0..], &.{}, &.{});
        try std.testing.expectEqual(ret.get_number(), 3);
    }

    {
        var args: [2]runtime.Value = .{
            runtime.Value.new_num(5),
            runtime.Value.new_num(3),
        };
        const ret = try test_helper(input, args[0..], &.{}, &.{});
        try std.testing.expectEqual(ret.get_number(), 5);
    }

    {
        var args: [2]runtime.Value = .{
            runtime.Value.new_num(0),
            runtime.Value.new_num(0),
        };
        const ret = try test_helper(input, args[0..], &.{}, &.{});
        try std.testing.expectEqual(ret.get_number(), 0);
    }
}

test "while fib" {
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

    var args: [1]runtime.Value = .{runtime.Value.new_num(35)};
    const ret = try test_helper_inner(input, args[0..], &.{}, &.{}, true);
    try std.testing.expectEqual(ret.get_number(), 9227465);
}

test "interpreter global" {
    const input =
        \\ fn(n) = {
        \\     let tmp = g;
        \\     g = n;
        \\     tmp;
        \\ };
    ;

    var args: [1]runtime.Value = .{runtime.Value.new_num(321)};
    var global_names: [1][]const u8 = .{"g"};
    var globals: [1]runtime.Value = .{runtime.Value.new_num(123)};

    // WARN: there is a problem with over write of a global since it
    // could run twice (ssa and out ir) so no it runs only with
    // ssa no (they should be the same anyway)
    const ret = try test_helper_inner(input, args[0..], global_names[0..], globals[0..], true);
    try std.testing.expectEqual(ret.get_number(), 123);
    try std.testing.expectEqual(globals[0].get_number(), 321);
}
