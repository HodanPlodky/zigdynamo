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
        return Interpreter{
            .code = code,
            .globals = globals,
            .env = env,
            .regs = try alloc.alloc(runtime.Value, inst_count.get_usize()),
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
                const reg = inst_idx.get_usize();
                switch (inst) {
                    .ldi => |num| self.regs[reg] = runtime.Value.new_num(num),
                    .mov => |src_reg| self.regs[reg] = self.regs[src_reg.get_usize()],
                    .nil => self.regs[reg] = runtime.Value.new_nil(),
                    .true => self.regs[reg] = runtime.Value.new_bool(true),
                    .false => self.regs[reg] = runtime.Value.new_bool(false),
                    .load_global => |num| {
                        self.regs[reg] = self.globals[@intCast(num)];
                    },
                    .store_global => |store_idx| {
                        const store = self.code.stores.get(ir.StoreData, store_idx);
                        const val = self.regs[store.value.get_usize()];
                        self.globals[@intCast(store.idx)] = val;
                    },
                    .load_env => |num| self.regs[reg] = self.env[@intCast(num)],
                    .store_env => |store_idx| {
                        const store = self.code.stores.get(ir.StoreData, store_idx);
                        const val = self.regs[store.value.get_usize()];
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
                        return self.regs[ret_reg.get_usize()];
                    },
                    .branch => |branch_idx| {
                        std.debug.assert(idx == curr_bb.instructions.items.len - 1);
                        before = curr_idx;
                        const branch = self.code.stores.get(ir.BranchData, branch_idx);
                        const cond = self.regs[branch.cond.get_usize()];
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
                    .arg => |num| self.regs[reg] = self.args[@intCast(num)],
                    .nop => {},
                    .phony => |phony_idx| {
                        const phony = self.code.stores.get(ir.PhonyData, phony_idx);
                        var res: ?ir.Reg = null;
                        for (phony.data) |pair| {
                            if (pair.label.index == before.?.index) {
                                res = pair.reg;
                            }
                        }

                        self.regs[reg] = self.regs[res.?.get_usize()];
                    },
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
        const left = self.regs[binop.left.get_usize()];
        const right = self.regs[binop.right.get_usize()];
        if (left.get_type() == runtime.ValueType.number and right.get_type() == runtime.ValueType.number) {
            self.regs[inst_idx.get_usize()] = oper(left, right);
        } else {
            std.debug.print("left: {}, right: {}\n", .{ left, right });
            @panic("Unimplemented dispatch");
        }
    }
};

fn test_helper(input: []const u8) !runtime.Value {
    const Parser = @import("../parser.zig").Parser;
    const ir_compile = @import("compile.zig").ir_compile;
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

    const globals: [][]const u8 = try allocator.alloc([]const u8, 0);
    const res = try ir_compile(function, metadata, globals, allocator);

    var interpret = try Interpreter.init(res, &.{}, &.{}, allocator);
    return interpret.run(&.{});
}

test "basic" {
    const input =
        \\ fn() = 1 + 2 * 3;
    ;

    const ret = try test_helper(input);
    try std.testing.expectEqual(ret.get_number(), 7);
}
