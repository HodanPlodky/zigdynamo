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

const Environment = struct {
    pub fn init() Environment {
        return Environment{};
    }
};

pub const Interpreter = struct {
    bytecode: bc.Bytecode,
    pc: usize,
    gc: GC,
    stack: Stack,

    pub fn init(alloc: std.mem.Allocator, bytecode: bc.Bytecode, heap_data: []u8) Interpreter {
        return Interpreter{
            .bytecode = bytecode,
            .pc = 5,
            .gc = GC.init(heap_data),
            .stack = Stack.init(alloc),
        };
    }

    pub fn run(self: *Interpreter) runtime.Value {
        while (true) {
            const inst = self.read_inst();
            //std.debug.print("{}\n", .{inst});
            switch (inst) {
                bc.Instruction.push => {
                    const num = self.bytecode.read_u32(self.pc);
                    self.pc += 4;
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
                bc.Instruction.ret => {
                    return self.stack.top().?;
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
};
