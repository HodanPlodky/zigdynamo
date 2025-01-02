const std = @import("std");
const runtime = @import("runtime.zig");
const bc = @import("bytecode.zig");

const Value = runtime.Value;
const ValueType = runtime.ValueType;

const GC = struct {
    heap: runtime.Heap,

    pub fn init(heap_data: []u8) GC {
        return GC{ .heap = runtime.Heap.init(heap_data) };
    }

    pub fn alloc(self: *GC, comptime T: type) *T {
        return self.heap.alloc(T);
    }

    pub fn alloc_with_additional(self: *GC, comptime T: type, count: usize) *T {
        return self.heap.alloc_with_additional(T, count);
    }
};

const Stack = struct {
    stack: std.ArrayList(runtime.Value),

    pub fn init(alloc: std.mem.Allocator) Stack {
        return Stack{
            .stack = std.ArrayList(runtime.Value).init(alloc),
        };
    }

    pub fn push(self: *Stack, value: runtime.Value) void {
        self.stack.append(value) catch unreachable;
    }

    pub fn push_unsafe(self: *Stack, value: runtime.Value) void {
        self.stack.appendAssumeCapacity(value);
    }

    pub fn pop(self: *Stack) ?runtime.Value {
        return self.stack.popOrNull();
    }

    pub fn top(self: *const Stack) ?runtime.Value {
        return self.stack.getLastOrNull();
    }

    pub fn slice_top(self: *const Stack, count: u32) []runtime.Value {
        const tmp: usize = @intCast(count);
        return self.stack.items[(self.stack.items.len - tmp)..];
    }

    pub fn pop_n(self: *Stack, count: u32) void {
        const tmp: usize = @intCast(count);
        self.stack.shrinkRetainingCapacity(self.stack.items.len - tmp);
    }
};

const LocalEnv = struct {
    buffer: std.ArrayList(Value),
    alloc: std.mem.Allocator,
    current_ptr: u32,

    pub fn init(alloc: std.mem.Allocator) LocalEnv {
        var buffer = std.ArrayList(Value).init(alloc);
        buffer.ensureTotalCapacity(8) catch unreachable;
        return LocalEnv{
            .buffer = buffer,
            .current_ptr = 0,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *LocalEnv) void {
        self.alloc.free(self.buffer);
    }

    pub fn push_locals(self: *LocalEnv, args: []Value, local_count: u32, ret_pc: u32, ret_const: bc.ConstantIndex) void {
        const old_fp: Value = Value.new_raw(@intCast(self.current_ptr));
        const tmp_pc: usize = @intCast(ret_pc);
        const ret = Value.new_raw(tmp_pc << 32 | ret_const.index);
        const tmp = self.buffer.items.len;
        self.buffer.ensureTotalCapacity(self.buffer.items.len + args.len + local_count + 2) catch unreachable;
        self.buffer.appendSliceAssumeCapacity(args);
        self.buffer.appendNTimesAssumeCapacity(Value.new_nil(), local_count);
        self.buffer.appendAssumeCapacity(old_fp);
        self.buffer.appendAssumeCapacity(ret);
        self.current_ptr = @intCast(tmp);
    }

    pub fn pop_locals(self: *LocalEnv) void {
        const old_fp = self.get_old_fp();
        self.buffer.shrinkRetainingCapacity(self.current_ptr);
        self.current_ptr = old_fp;
    }

    pub fn get_old_fp(self: *const LocalEnv) u32 {
        const old_fp = self.buffer.items[self.buffer.items.len - 2];
        return @intCast(old_fp.data);
    }

    pub fn get_ret(self: *const LocalEnv) struct { ret_pc: u32, ret_const: bc.ConstantIndex } {
        const ret = self.buffer.items[self.buffer.items.len - 1];
        const ret_pc: u32 = @intCast((ret.data >> 32) & 0xffffffff);
        const ret_const: u32 = @intCast(ret.data & 0xffffffff);
        return .{
            .ret_pc = ret_pc,
            .ret_const = bc.ConstantIndex.new(ret_const),
        };
    }

    pub fn get(self: *const LocalEnv, idx: u32) Value {
        return self.buffer.items[@intCast(self.current_ptr + idx)];
    }

    pub fn set(self: *LocalEnv, idx: u32, value: Value) void {
        self.buffer.items[@intCast(self.current_ptr + idx)] = value;
    }
};

const Environment = struct {
    global: []Value,
    local: LocalEnv,

    pub fn init(global_count: usize, alloc: std.mem.Allocator) Environment {
        return Environment{
            .global = alloc.alloc(Value, global_count) catch unreachable,
            .local = LocalEnv.init(alloc),
        };
    }

    pub fn deinit(self: *Environment) void {
        self.local.deinit();
    }

    pub fn set_global(self: *Environment, index: u32, value: Value) void {
        self.global[@intCast(index)] = value;
    }

    pub fn get_global(self: *Environment, index: u32) Value {
        return self.global[@intCast(index)];
    }
};

pub const Interpreter = struct {
    bytecode: bc.Bytecode,
    pc: usize,
    curr_const: bc.ConstantIndex,
    gc: GC,
    stack: Stack,
    env: Environment,

    pub fn init(alloc: std.mem.Allocator, bytecode: bc.Bytecode, heap_data: []u8) Interpreter {
        return Interpreter{
            .bytecode = bytecode,
            .pc = 5,
            .curr_const = bc.ConstantIndex.new(0),
            .gc = GC.init(heap_data),
            .stack = Stack.init(alloc),
            .env = Environment.init(bytecode.global_count, alloc),
        };
    }

    pub fn run(self: *Interpreter) runtime.Value {
        while (true) {
            const inst = self.read_inst();
            //std.debug.print("{}\n", .{inst});
            switch (inst) {
                bc.Instruction.push => {
                    const num = self.read_u32();
                    const val = Value.new_num(num);
                    self.stack.push(val);
                },
                bc.Instruction.pop => {
                    _ = self.stack.pop();
                },
                bc.Instruction.true => {
                    const val = Value.new_true();
                    self.stack.push(val);
                },
                bc.Instruction.false => {
                    const val = Value.new_false();
                    self.stack.push(val);
                },
                bc.Instruction.nil => {
                    const val = Value.new_nil();
                    self.stack.push(val);
                },

                // should not create call
                bc.Instruction.add => self.handle_binop(Value.add),
                bc.Instruction.sub => self.handle_binop(Value.sub),
                bc.Instruction.mul => self.handle_binop(Value.mul),
                bc.Instruction.div => self.handle_binop(Value.div),
                bc.Instruction.gt => self.handle_binop(Value.gt),
                bc.Instruction.lt => self.handle_binop(Value.lt),
                bc.Instruction.ret => {
                    const restore_data = self.env.local.get_ret();
                    self.pc = restore_data.ret_pc;
                    self.curr_const = restore_data.ret_const;
                    self.bytecode.set_curr_const(restore_data.ret_const);
                    self.env.local.pop_locals();
                },
                bc.Instruction.ret_main => {
                    return self.stack.top().?;
                },

                bc.Instruction.set_global => {
                    const value = self.stack.pop().?;
                    const idx = self.read_u32();
                    self.env.set_global(idx, value);
                },
                bc.Instruction.set => {
                    const value = self.stack.pop().?;
                    const idx = self.read_u32();
                    self.env.local.set(idx, value);
                },

                bc.Instruction.get_global => {
                    const idx = self.bytecode.read_u32(self.pc);
                    self.pc += 4;
                    const value = self.env.get_global(idx);
                    self.stack.push(value);
                },
                bc.Instruction.get => {
                    const idx = self.bytecode.read_u32(self.pc);
                    self.pc += 4;
                    const value = self.env.local.get(idx);
                    self.stack.push(value);
                },

                bc.Instruction.jump => {
                    const pc = self.read_u32();
                    self.pc = pc;
                },
                bc.Instruction.branch => {
                    const pc = self.read_u32();
                    const cond = self.stack.pop().?;
                    switch (cond.get_type()) {
                        ValueType.true => self.pc = pc,
                        ValueType.false => {},
                        else => @panic("If condition must be boolean"),
                    }
                },
                bc.Instruction.closure => {
                    const constant_idx = self.read_u32();
                    const unbound_count = self.read_u32();
                    const env = self.stack.slice_top(unbound_count);
                    const closure = self.gc.alloc_with_additional(bc.Closure, unbound_count);
                    closure.env.count = unbound_count;
                    for (env, 0..) |val, idx| {
                        closure.env.set(idx, val);
                    }
                    self.stack.pop_n(unbound_count);
                    closure.constant_idx = bc.ConstantIndex.new(constant_idx);
                    const code = self.bytecode.get_constant(closure.constant_idx);
                    closure.local_count = code.get_u32(5);
                    closure.param_count = code.get_u32(9);
                    const val = Value.new_ptr(bc.Closure, closure, ValueType.closure);
                    self.stack.push(val);
                },
                bc.Instruction.call => {
                    const target = self.stack.pop().?;
                    if (target.get_type() != runtime.ValueType.closure) {
                        @panic("cannot call this object");
                    }
                    const closure = target.get_ptr(bc.Closure);
                    const local_count = closure.local_count;
                    const param_count = closure.param_count;
                    const arg_slice = self.stack.slice_top(param_count);
                    self.env.local.push_locals(arg_slice, local_count, @intCast(self.pc), self.curr_const);
                    for (0..closure.env.count) |idx| {
                        const index: u32 = @intCast(idx);
                        const val = closure.env.get(closure.env.count - idx - 1);
                        self.env.local.set(local_count - index - 1, val);
                    }
                    self.stack.pop_n(param_count);
                    self.bytecode.set_curr_const(closure.constant_idx);
                    self.curr_const = closure.constant_idx;

                    // header size of the closure
                    self.pc = 4 + 1 + 4 + 4;
                },
                bc.Instruction.print => {
                    const arg_count = self.read_u32();
                    const arg_slice = self.stack.slice_top(arg_count);
                    for (arg_slice) |val| {
                        switch (val.get_type()) {
                            ValueType.string => {
                                const idx = bc.ConstantIndex.new(val.get_idx());
                                const string_const = self.bytecode.get_constant(idx);
                                const tmp = string_const.get_slice()[5..];
                                std.debug.print("{s} ", .{tmp});
                            },
                            ValueType.number => {
                                const data = val.get_number();
                                std.debug.print("{} ", .{data});
                            },
                            else => @panic("Cannot print"),
                        }
                    }
                    std.debug.print("\n", .{});
                    self.stack.push(Value.new_nil());
                },
                bc.Instruction.string => {
                    const idx = bc.ConstantIndex.new(self.read_u32());
                    if (self.bytecode.get_type(idx) != bc.ConstantType.string) {
                        @panic("Incorrect string");
                    }
                    const val = Value.new_string(idx.index);
                    self.stack.push(val);
                },
                else => @panic("unimplemented instruction"),
            }
        }
        return runtime.Value.new_nil();
    }

    fn handle_binop(self: *Interpreter, comptime oper: fn (Value, Value) Value) void {
        const right = self.stack.pop().?;
        const left = self.stack.pop().?;
        if (left.get_type() == ValueType.number and right.get_type() == ValueType.number) {
            const res = oper(left, right);
            self.stack.push_unsafe(res);
        } else {
            std.debug.print("left: {}, right: {}\n", .{ left, right });
            @panic("Unimplemented dispatch");
        }
    }

    fn read_inst(self: *Interpreter) bc.Instruction {
        const res = self.bytecode.read_inst(self.pc);
        self.pc += 1;
        return res;
    }

    fn read_u32(self: *Interpreter) u32 {
        const res = self.bytecode.read_u32(self.pc);
        self.pc += 4;
        return res;
    }
};
