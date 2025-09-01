const std = @import("std");
const CompiledResult = @import("compile.zig").CompiledResult;
const BcIntepreter = @import("../bc_interpreter.zig").BcInterpreter;
const runtime = @import("../runtime.zig");
const ir = @import("ir.zig");

/// Interpreter of SSA code
/// this is for debug purpouses
const Interpreter = struct {
    code: CompiledResult,
    bytecode_interpreter: *BcIntepreter,
    env: []runtime.Value,
    regs: []runtime.Value,
    alloc: std.mem.Allocator,
    args: []runtime.Value = undefined,

    pub fn init(
        code: CompiledResult,
        bytecode_interpreter: *BcIntepreter,
        env: []runtime.Value,
        alloc: std.mem.Allocator,
    ) !Interpreter {
        const inst_count = code.stores.get_max_idx(ir.Instruction);
        return Interpreter{
            .code = code,
            .bytecode_interpreter = bytecode_interpreter,
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
        return self.run_fn(function, args);
    }

    fn run_fn(self: *Interpreter, function: ir.Function) runtime.Value {
        var before: ?ir.BasicBlockIdx = null;
        var curr_idx = function.entry;
        //var curr_bb = self.code.stores.get(ir.BasicBlock, curr_idx);

        while (true) {
            var curr_bb = self.code.stores.get(ir.BasicBlock, curr_idx);
            for (curr_bb.instructions, 0..) |inst_idx, idx| {
                const inst = self.code.stores.get(ir.Instruction, inst_idx);
                const reg = inst_idx.get_usize();
                switch (inst) {
                    .ldi => |num| self.regs[reg] = runtime.Value.new_num(num),
                    .mov => |src_reg| self.regs[reg] = self.regs[src_reg.get_usize()],
                    .nil => self.regs[reg] = runtime.Value.new_nil(),
                    .true => self.regs[reg] = runtime.Value.new_bool(true),
                    .false => self.regs[reg] = runtime.Value.new_bool(false),
                    .load_global => |num| self.bytecode_interpreter.env.global[@intCast(num)],
                    .store_global => |store_idx| {
                        const store = self.code.stores.get(ir.StoreData, store_idx);
                        const val = self.regs[store.value.get_usize()];
                        self.bytecode_interpreter.env.global[@intCast(store.idx)] = val;
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
                        std.debug.assert(idx == curr_bb.instructions.items.len);
                        return self.regs[ret_reg.get_usize()];
                    },
                    .branch => |branch_idx| {
                        std.debug.assert(idx == curr_bb.instructions.items.len);
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
                        std.debug.assert(idx == curr_bb.instructions.items.len);
                        before = curr_idx;
                        curr_bb = label;
                        break;
                    },
                    .arg => |num| self.regs[reg] = self.args[@intCast(num)],
                    .nop => {},
                    .phony => |phony_idx| {
                        const phony = self.code.stores.get(ir.PhonyData, phony_idx);
                        const res: ?runtime.Value = null;
                        for (phony.data) |pair| {
                            if (pair.label.index == before.index) {
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
        comptime oper: fn (runtime.Value, runtime.Value) ir.Value,
    ) void {
        const left = self.regs[binop.left.get_usize()];
        const right = self.regs[binop.left.get_usize()];
        if (left.get_type() == runtime.ValueType.number and right.get_type() == runtime.ValueType.number) {
            self.regs[inst_idx.get_usize()] = oper(left, right);
        } else {
            std.debug.print("left: {}, right: {}\n", .{ left, right });
            @panic("Unimplemented dispatch");
        }
    }
};

test "basic" {
    
}
