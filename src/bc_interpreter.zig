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

    pub fn pop(self: *Stack) ?runtime.Value {
        return self.stack.popOrNull();
    }

    pub fn top(self: *const Stack) ?runtime.Value {
        return self.stack.getLastOrNull();
    }
};

const LocalEnv = struct {
    buffer: std.ArrayList(Value),
    current_local: [*]Value,
    curr_ptr: usize,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) LocalEnv {
        var buffer = std.ArrayList(Value).init(alloc);
        buffer.ensureTotalCapacity(8) catch unreachable;
        return LocalEnv{
            .buffer = buffer,
            .current_local = @ptrCast(buffer.items),
            .curr_ptr = 0,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *LocalEnv) void {
        self.alloc.free(self.buffer);
    }

    pub fn push_locals(self: *LocalEnv, count: usize) void {
        _ = self; // autofix
        _ = count; // autofix

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
    gc: GC,
    stack: Stack,
    env: Environment,

    pub fn init(alloc: std.mem.Allocator, bytecode: bc.Bytecode, heap_data: []u8) Interpreter {
        return Interpreter{
            .bytecode = bytecode,
            .pc = 5,
            .gc = GC.init(heap_data),
            .stack = Stack.init(alloc),
            .env = Environment.init(bytecode.global_count, alloc),
        };
    }

    pub fn run(self: *Interpreter) runtime.Value {
        while (true) {
            const inst = self.read_inst();
            std.debug.print("{}\n", .{inst});
            switch (inst) {
                bc.Instruction.push => {
                    const num = self.read_u32();
                    const val = Value.new_num(num);
                    self.stack.push(val);
                },
                bc.Instruction.pop => {
                    _ = self.stack.pop();
                },
                // should not create call
                bc.Instruction.add => self.handle_binopt(Value.add),
                bc.Instruction.sub => self.handle_binopt(Value.sub),
                bc.Instruction.mul => self.handle_binopt(Value.mul),
                bc.Instruction.div => self.handle_binopt(Value.div),
                bc.Instruction.gt => self.handle_binopt(Value.gt),
                bc.Instruction.lt => self.handle_binopt(Value.lt),
                bc.Instruction.ret => {
                    return self.stack.top().?;
                },

                bc.Instruction.set_global => {
                    const value = self.stack.pop().?;
                    const idx = self.read_u32();
                    self.env.set_global(idx, value);
                },
                bc.Instruction.get_global => {
                    const idx = self.bytecode.read_u32(self.pc);
                    self.pc += 4;
                    const value = self.env.get_global(idx);
                    self.stack.push(value);
                },
                else => @panic("unimplemented instruction"),
            }
        }
        return runtime.Value.new_nil();
    }

    fn handle_binopt(self: *Interpreter, comptime oper: fn (Value, Value) Value) void {
        const right = self.stack.pop().?;
        const left = self.stack.pop().?;
        if (left.get_type() == ValueType.number and right.get_type() == ValueType.number) {
            const res = oper(left, right);
            self.stack.push(res);
        } else {
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
