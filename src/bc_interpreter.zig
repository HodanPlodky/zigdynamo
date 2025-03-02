const std = @import("std");
const runtime = @import("runtime.zig");
const bc = @import("bytecode.zig");
const jit = @import("jit_compiler.zig");

const Value = runtime.Value;
const ValueType = runtime.ValueType;

const Roots = struct {
    stack: *const Stack,
    env: *const Environment,
};

/// Garbage collection
/// implemeted via copying semispaces
/// size : 48
pub const GC = struct {
    from: runtime.Heap,
    to: runtime.Heap,

    pub fn init(heap_data: []u8) GC {
        // this is to make sure that all the aligns are ok
        std.debug.assert(heap_data.len % 16 == 0);
        return GC{
            .from = runtime.Heap.init(heap_data[0 .. heap_data.len / 2]),
            .to = runtime.Heap.init(heap_data[heap_data.len / 2 ..]),
        };
    }

    pub fn alloc_with_additional(self: *GC, comptime T: type, count: usize, roots: Roots) *T {
        if (!self.from.check_available(T, count)) {
            self.collect(roots);
        }
        return self.from.alloc_with_additional(T, count);
    }

    fn collect(self: *GC, roots: Roots) void {
        const before_size = self.from.curr_ptr;
        self.copy_roots(roots);
        var done_ptr: usize = 0;
        while (done_ptr < self.to.curr_ptr) {
            const tag_bytes: *u32 = @ptrCast(@alignCast(self.to.data[done_ptr..(done_ptr + 4)]));
            const tag = tag_bytes.*;
            const object_type: ValueType = @enumFromInt(tag);
            switch (object_type) {
                ValueType.object => {
                    const object: *bc.Object = @ptrCast(@alignCast(tag_bytes));

                    self.copy_object(&object.prototype);

                    for (0..object.values.count) |idx| {
                        const val = object.values.get_ptr(idx);
                        self.copy_object(val);
                    }

                    done_ptr = GC.next_object(bc.Object, object, done_ptr);
                },
                ValueType.closure => {
                    const closure: *bc.Closure = @ptrCast(@alignCast(tag_bytes));

                    for (0..closure.env.count) |idx| {
                        const val = closure.env.get_ptr(idx);
                        self.copy_object(val);
                    }

                    done_ptr = GC.next_object(bc.Closure, closure, done_ptr);
                },
                else => {
                    std.debug.print("{}\n", .{object_type});
                    @panic("Invalid object on heap");
                },
            }
        }

        std.debug.assert(done_ptr == self.to.curr_ptr);
        const tmp = self.from;
        self.from = self.to;
        self.to = tmp;
        self.to.curr_ptr = 0;
        // worst case scenarion is that all of the vals
        // are reachable in that case it should be equal
        // otherwise it should be lower
        // the + 16 is there for possible differences in alignment
        std.debug.assert(self.from.curr_ptr <= before_size + 16);
    }

    fn copy_roots(self: *GC, roots: Roots) void {
        for (roots.stack.stack.items) |*root| {
            self.copy_object(root);
        }

        for (roots.env.global) |*root| {
            self.copy_object(root);
        }

        if (roots.env.local.get_last_frame()) |tmp| {
            var frame_helper = tmp;
            while (true) {
                for (frame_helper.frame) |*root| {
                    self.copy_object(root);
                }

                if (frame_helper.position == 0) {
                    break;
                }
                frame_helper = roots.env.local.get_next_frame(frame_helper);
            }
        }
    }

    fn next_object(comptime T: type, object: *const T, done: usize) usize {
        var res = done + object.get_size();
        res = (res + (runtime.Heap.heap_align - 1)) & ~(runtime.Heap.heap_align - 1);
        return res;
    }

    fn copy_object(self: *GC, addr: *Value) void {
        if (!addr.is_ptr()) {
            return;
        }

        const origin: Value = addr.*;

        // always copy from from semi space
        const object_addr = origin.get_ptr_raw();
        const object_addr_val: usize = @intFromPtr(object_addr);
        const from_start: usize = @intFromPtr(self.from.data.ptr);
        const from_end = from_start + self.from.data.len;
        std.debug.assert(from_start <= object_addr_val and object_addr_val < from_end);

        // there is already forward ptr in address
        const forward_bytes: [4]u8 = .{ 0xff, 0xff, 0xff, 0xff };
        if (std.mem.eql(u8, object_addr[0..4], forward_bytes[0..])) {
            const forward = origin.get_ptr(bc.Forward);
            const start: usize = @intFromPtr(self.to.data.ptr);
            addr.* = origin.rewrite_ptr(@as(usize, forward.ptr) + start);
            return;
        }

        var to_ptr: u32 = undefined;
        switch (origin.get_type()) {
            ValueType.object => {
                const object = origin.get_ptr(bc.Object);
                std.debug.assert(object.tag == @intFromEnum(ValueType.object));
                const dst = self.to.alloc_with_additional(bc.Object, object.values.count);
                dst.tag = object.tag;
                dst.class_idx = object.class_idx;
                dst.prototype = object.prototype;
                dst.values.count = object.values.count;
                for (0..object.values.count) |idx| {
                    const val = object.values.get(idx);
                    dst.values.set(idx, val);
                }

                addr.* = Value.new_ptr(bc.Object, dst, ValueType.object);
                const tmp: u64 = @intFromPtr(dst) - @intFromPtr(self.to.data.ptr);
                to_ptr = @intCast(tmp);
            },
            ValueType.closure => {
                const closure = origin.get_ptr(bc.Closure);
                std.debug.assert(closure.tag == @intFromEnum(ValueType.closure));
                const dst = self.to.alloc_with_additional(bc.Closure, closure.env.count);
                dst.tag = closure.tag;
                dst.local_count = closure.local_count;
                dst.param_count = closure.param_count;
                dst.constant_idx = closure.constant_idx;
                dst.env.count = closure.env.count;
                for (0..closure.env.count) |idx| {
                    const val = closure.env.get(idx);
                    dst.env.set(idx, val);
                }

                addr.* = Value.new_ptr(bc.Closure, dst, ValueType.closure);
                const tmp: u64 = @intFromPtr(dst) - @intFromPtr(self.to.data.ptr);
                to_ptr = @intCast(tmp);
            },
            else => unreachable,
        }

        const forward_ptr = origin.get_ptr(bc.Forward);
        forward_ptr.tag = bc.FORWARD_TAG;
        forward_ptr.ptr = to_ptr;
    }
};

// size: 40
pub const Stack = struct {
    stack: std.ArrayList(runtime.Value), // 40?

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

    pub fn pop(self: *Stack) runtime.Value {
        return self.stack.pop();
    }

    pub fn top(self: *const Stack) runtime.Value {
        return self.stack.getLast();
    }

    pub fn set_top(self: *Stack, value: runtime.Value) void {
        self.stack.items[self.stack.items.len - 1] = value;
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

/// size: 60 + 4
pub const LocalEnv = struct {
    // [locals] [old fp] [ret]
    buffer: std.ArrayList(Value), // 40
    alloc: std.mem.Allocator, // 16
    current_ptr: u32, // 4

    const FrameHelper = struct {
        frame: []Value,
        position: usize,
    };

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
        self.current_ptr = @intCast(self.buffer.items.len);
        self.buffer.ensureTotalCapacity(self.buffer.items.len + args.len + local_count + 2) catch unreachable;
        self.buffer.appendSliceAssumeCapacity(args);
        self.buffer.appendNTimesAssumeCapacity(Value.new_nil(), local_count);
        self.buffer.appendAssumeCapacity(old_fp);
        self.buffer.appendAssumeCapacity(ret);
    }

    pub fn push_locals_this(self: *LocalEnv, this: Value, args: []Value, local_count: u32, ret_pc: u32, ret_const: bc.ConstantIndex) void {
        const old_fp: Value = Value.new_raw(@intCast(self.current_ptr));
        const tmp_pc: usize = @intCast(ret_pc);
        const ret = Value.new_raw(tmp_pc << 32 | ret_const.index);
        self.current_ptr = @intCast(self.buffer.items.len);
        self.buffer.ensureTotalCapacity(self.buffer.items.len + args.len + local_count + 2 + 1) catch unreachable;
        self.buffer.appendAssumeCapacity(this);
        self.buffer.appendSliceAssumeCapacity(args);
        self.buffer.appendNTimesAssumeCapacity(Value.new_nil(), local_count);
        self.buffer.appendAssumeCapacity(old_fp);
        self.buffer.appendAssumeCapacity(ret);
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

    pub fn get_last_frame(self: *const LocalEnv) ?FrameHelper {
        if (self.buffer.items.len == 0) {
            return null;
        }
        const frame = self.buffer.items[self.current_ptr..(self.buffer.items.len - 2)];
        const position = self.current_ptr;
        return FrameHelper{
            .frame = frame,
            .position = position,
        };
    }

    pub fn get_next_frame(self: *const LocalEnv, frame: FrameHelper) FrameHelper {
        // we must reach outside of values of frame
        const tmp: [*]Value = @ptrCast(frame.frame);
        const fp = tmp[frame.frame.len].data;
        return FrameHelper{
            .frame = self.buffer.items[fp..(frame.position - 2)],
            .position = fp,
        };
    }

    pub fn get_ret(self: *const LocalEnv) struct { ret_pc: u32, ret_const: bc.ConstantIndex } {
        const ret = self.buffer.items[self.buffer.items.len - 1];
        const ret_pc: u32 = @intCast((ret.data >> 32));
        const ret_const: u32 = @intCast(ret.data & 0xffffffff);
        return .{
            .ret_pc = ret_pc,
            .ret_const = bc.ConstantIndex.new(ret_const),
        };
    }

    pub fn get(self: *const LocalEnv, idx: u32) Value {
        return self.buffer.items[@intCast(self.current_ptr + idx)];
    }

    pub fn get0(self: *const LocalEnv) Value {
        return self.buffer.items[@intCast(self.current_ptr)];
    }

    pub fn set(self: *LocalEnv, idx: u32, value: Value) void {
        self.buffer.items[@intCast(self.current_ptr + idx)] = value;
    }
};

/// size: 76
pub const Environment = struct {
    global: []Value, // 16
    local: LocalEnv, // 60

    pub fn init(global_count: usize, alloc: std.mem.Allocator) Environment {
        const res = Environment{
            .global = alloc.alloc(Value, global_count) catch unreachable,
            .local = LocalEnv.init(alloc),
        };

        @memset(res.global, Value.new_nil());

        return res;
    }

    pub fn deinit(self: *Environment) void {
        self.local.deinit();
    }

    pub fn set_global(self: *Environment, index: u32, value: Value) void {
        self.global[@intCast(index)] = value;
    }

    pub fn get_global(self: *const Environment, index: u32) Value {
        return self.global[@intCast(index)];
    }
};

/// size: 136 (0x88)
pub const Interpreter = struct {
    bytecode: bc.Bytecode, // 32
    pc: usize, // 8
    curr_const: bc.ConstantIndex, // 4 + 4
    gc: GC, // 48
    stack: Stack, // 40
    env: Environment, // 76
    jit_compiler: jit.JitCompiler,

    pub fn init(alloc: std.mem.Allocator, bytecode: bc.Bytecode, heap_data: []u8) Interpreter {
        return Interpreter{
            .bytecode = bytecode,
            .pc = 5,
            .curr_const = bc.ConstantIndex.new(0),
            .gc = GC.init(heap_data),
            .stack = Stack.init(alloc),
            .env = Environment.init(bytecode.global_count, alloc),
            .jit_compiler = jit.JitCompiler.init(4096),
        };
    }

    pub fn run(self: *Interpreter) runtime.Value {
        while (true) {
            const inst = self.read_inst();
            std.debug.assert(self.stack.stack.items.len <= self.stack.stack.capacity);
            switch (inst) {
                bc.Instruction.push => {
                    const num = self.read_u32();
                    const val = Value.new_num(num);
                    self.stack.push(val);
                },
                bc.Instruction.push_byte => {
                    const num = self.read_u8();
                    const val = Value.new_num(@intCast(num));
                    self.stack.push(val);
                },
                bc.Instruction.pop => {
                    _ = self.stack.pop();
                },
                bc.Instruction.dup => {
                    self.stack.push(self.stack.top());
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
                bc.Instruction.eq => {
                    const right = self.stack.pop();
                    const left = self.stack.top();
                    self.stack.set_top(Value.eq(left, right));
                },
                bc.Instruction.ne => {
                    const right = self.stack.pop();
                    const left = self.stack.top();
                    self.stack.set_top(Value.ne(left, right));
                },
                bc.Instruction.ret => {
                    const restore_data = self.env.local.get_ret();
                    self.pc = restore_data.ret_pc;
                    self.curr_const = restore_data.ret_const;
                    self.bytecode.set_curr_const(restore_data.ret_const);
                    self.env.local.pop_locals();
                },
                bc.Instruction.ret_main => {
                    return self.stack.top();
                },

                bc.Instruction.set_global => {
                    const value = self.stack.top();
                    const idx = self.read_u32();
                    self.env.set_global(idx, value);
                },
                bc.Instruction.set => {
                    const value = self.stack.top();
                    const idx = self.read_u32();
                    self.env.local.set(idx, value);
                },
                bc.Instruction.set_global_small => {
                    const value = self.stack.top();
                    const idx = self.read_u8();
                    self.env.set_global(@intCast(idx), value);
                },
                bc.Instruction.set_small => {
                    const value = self.stack.top();
                    const idx = self.read_u8();
                    self.env.local.set(@intCast(idx), value);
                },

                bc.Instruction.get_global => {
                    const idx = self.read_u32();
                    const value = self.env.get_global(idx);
                    self.stack.push(value);
                },
                bc.Instruction.get_global_small => {
                    const idx = self.read_u8();
                    const value = self.env.get_global(@intCast(idx));
                    self.stack.push(value);
                },
                bc.Instruction.get => {
                    const idx = self.read_u32();
                    const value = self.env.local.get(idx);
                    self.stack.push(value);
                },
                bc.Instruction.get_small => {
                    const idx = self.read_u8();
                    const value = self.env.local.get(@intCast(idx));
                    self.stack.push(value);
                },

                bc.Instruction.jump => {
                    const pc = self.read_u32();
                    self.pc = pc;
                },
                bc.Instruction.branch => {
                    const pc = self.read_u32();
                    const cond = self.stack.pop();
                    switch (cond.get_type()) {
                        ValueType.true => self.pc = pc,
                        ValueType.false => {},
                        else => @panic("If condition must be boolean"),
                    }
                },
                bc.Instruction.closure => {
                    const constant_idx: u64 = self.read_u32();
                    const unbound_count: u64 = self.read_u32();
                    @call(.always_inline, create_closure, .{ self, constant_idx, unbound_count });
                },
                bc.Instruction.call => {
                    const jit_state = self.get_jit_state();
                    @call(.always_inline, do_call, .{ self, &jit_state });
                },
                bc.Instruction.print => {
                    const arg_count: u64 = @intCast(self.read_u32());
                    @call(.always_inline, do_print, .{ self, arg_count });
                },
                bc.Instruction.string => {
                    const idx = bc.ConstantIndex.new(self.read_u32());
                    if (self.bytecode.get_type(idx) != bc.ConstantType.string) {
                        @panic("Incorrect string");
                    }
                    const val = Value.new_string(idx.index);
                    self.stack.push(val);
                },
                bc.Instruction.object => {
                    const class_idx = bc.ConstantIndex.new(self.read_u32());
                    @call(.always_inline, create_object, .{ self, class_idx });
                },
                bc.Instruction.get_field => {
                    const field_idx = self.read_u32();

                    do_get_field(self, bc.ConstantIndex.new(field_idx));
                },
                bc.Instruction.set_field => {
                    const field_idx = self.read_u32();
                    do_set_field(self, bc.ConstantIndex.new(field_idx));
                },
                bc.Instruction.methodcall => {
                    const field_idx = self.read_u32();
                    const jit_state = self.get_jit_state();
                    do_method_call(self, &jit_state, bc.ConstantIndex.new(field_idx));

                    //const target = self.stack.pop();
                    //if (target.get_type() != ValueType.object) {
                    //@panic("cannot call method on non object");
                    //}
                    //
                    //const object = target.get_ptr(bc.Object);
                    //const field: ?runtime.Value = self.get_field(object, bc.ConstantIndex.new(field_idx));
                    //if (field) |item| {
                    //if (item.get_type() != runtime.ValueType.closure) {
                    //@panic("cannot call this object");
                    //}
                    //const closure = item.get_ptr(bc.Closure);
                    //const local_count = closure.local_count;
                    //const param_count = closure.param_count;
                    //const arg_slice = self.stack.slice_top(param_count);
                    //self.env.local.push_locals_this(target, arg_slice, local_count, @intCast(self.pc), self.curr_const);
                    //for (0..closure.env.count) |idx| {
                    //const index: u32 = @intCast(idx);
                    //const val = closure.env.get(closure.env.count - idx - 1);
                    //self.env.local.set(local_count - index - 1, val);
                    //}
                    //self.stack.pop_n(param_count);
                    //self.bytecode.set_curr_const(closure.constant_idx);
                    //self.curr_const = closure.constant_idx;
                    //
                    //// header size of the closure
                    //self.pc = bc.Constant.function_header_size;
                    //} else {
                    //@panic("non existant field");
                    //}
                },
            }
        }
    }

    fn get_field(self: *const Interpreter, object: *bc.Object, string_field_idx: bc.ConstantIndex) ?runtime.Value {
        var tmp: *bc.Object = object;
        while (true) {
            //const tmp_addr = @intFromPtr(tmp);
            //std.debug.print("{}\n", .{tmp_addr});
            const class_constant = self.bytecode.get_constant(tmp.class_idx);
            const position: ?usize = class_constant.get_class_field_position(string_field_idx);
            if (position) |pos| {
                return tmp.values.get(pos);
            }
            const proto = tmp.prototype;
            if (proto.get_type() != runtime.ValueType.object) {
                return null;
            }
            tmp = proto.get_ptr(bc.Object);
        }
    }

    fn get_field_ptr(self: *const Interpreter, object: *bc.Object, string_field_idx: bc.ConstantIndex) ?*runtime.Value {
        var tmp: *bc.Object = object;
        while (true) {
            const class_constant = self.bytecode.get_constant(tmp.class_idx);
            const position: ?usize = class_constant.get_class_field_position(string_field_idx);
            if (position) |pos| {
                return tmp.values.get_ptr(pos);
            }
            const proto = tmp.prototype;
            if (proto.get_type() != runtime.ValueType.object) {
                return null;
            }
            tmp = proto.get_ptr(bc.Object);
        }
    }

    fn handle_binop(self: *Interpreter, comptime oper: fn (Value, Value) Value) void {
        const right = self.stack.pop();
        const left = self.stack.top();
        if (left.get_type() == ValueType.number and right.get_type() == ValueType.number) {
            const res = oper(left, right);
            self.stack.set_top(res);
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

    fn read_u8(self: *Interpreter) u8 {
        const res = self.bytecode.read_u8(self.pc);
        self.pc += 1;
        return res;
    }

    fn get_roots(self: *const Interpreter) Roots {
        return Roots{
            .stack = &self.stack,
            .env = &self.env,
        };
    }

    fn get_jit_state(self: *const Interpreter) jit.JitState {
        return jit.JitState{
            .intepreter = self,
            .stack = &self.stack,
            .env = &self.env,
            .gc = &self.gc,
            .alloc_stack = &alloc_stack,
            .create_closure = &create_closure,
            .create_object = &create_object,
            .get_field = &do_get_field,
            .set_field = &do_set_field,
            .call = &do_call,
            .method_call = &do_method_call,
            .print = &do_print,
            .dbg = &dbg,
            .dbg_raw = &dbg_raw,
            .dbg_inst = &dbg_inst,
            .binop_panic = &binop_panic,
            .if_condition_panic = &if_condition_panic,
            .string_panic = &string_panic,
        };
    }
};

// JIT helper functions

pub fn alloc_stack(stack: *Stack, new_len: usize) callconv(.C) void {
    stack.stack.ensureTotalCapacity(new_len) catch unreachable;
}

pub fn get_local(env: *const Environment, idx: u32) callconv(.C) Value {
    return env.local.get(idx);
}

pub fn set_local(env: *Environment, idx: u32, value: Value) callconv(.C) void {
    env.local.set(idx, value);
}

pub fn pop_locals(env: *Environment) callconv(.C) void {
    env.local.pop_locals();
}

pub fn push_locals(env: *Environment, args_ptr: u64, args_len: usize, local_count: u32) callconv(.C) void {
    const args_tmp: [*]Value = @ptrFromInt(args_ptr);
    const args = args_tmp[0..args_len];
    env.local.push_locals(args, local_count, 0, bc.ConstantIndex.new(0));
}

pub fn gc_alloc_object(intepreter: *Interpreter, field_count: usize) callconv(.C) *bc.Object {
    return intepreter.gc.alloc_with_additional(bc.Object, field_count, intepreter.get_roots());
}

pub fn gc_alloc_closure(intepreter: *Interpreter, env_size: usize) callconv(.C) *bc.Closure {
    return intepreter.gc.alloc_with_additional(bc.Closure, env_size, intepreter.get_roots());
}

pub fn do_call(noalias interpret: *Interpreter, noalias jit_state: *const jit.JitState) callconv(.C) void {
    const target = interpret.stack.pop();
    do_value_call(interpret, null, jit_state, target);
}

pub fn do_method_call(noalias self: *Interpreter, noalias jit_state: *const jit.JitState, method_idx: bc.ConstantIndex) callconv(.C) void {
    const target = self.stack.pop();
    if (target.get_type() != ValueType.object) {
        @panic("cannot call method on non object");
    }

    const object = target.get_ptr(bc.Object);
    const field: ?runtime.Value = self.get_field(object, method_idx);
    if (field) |item| {
        //@call(.always_inline, do_value_call, .{ self, jit_state, item });
        do_value_call(self, target, jit_state, item);
    } else {
        @panic("non existant field");
    }
}

fn do_value_call(noalias interpret: *Interpreter, this: ?Value, noalias jit_state: *const jit.JitState, target: Value) void {
    if (target.get_type() != runtime.ValueType.closure) {
        std.debug.print("{}\n", .{target.get_type()});
        @panic("cannot call this object");
    }
    const closure = target.get_ptr(bc.Closure);
    const local_count = closure.local_count;
    const param_count = closure.param_count;
    const arg_slice = interpret.stack.slice_top(param_count);
    if (this) |this_val| {
        interpret.env.local.push_locals_this(this_val, arg_slice, local_count, @intCast(interpret.pc), interpret.curr_const);
    } else {
        interpret.env.local.push_locals(arg_slice, local_count, @intCast(interpret.pc), interpret.curr_const);
    }
    for (0..closure.env.count) |idx| {
        const index: u32 = @intCast(idx);
        const val = closure.env.get(closure.env.count - idx - 1);
        interpret.env.local.set(local_count - index - 1, val);
    }
    interpret.stack.pop_n(param_count);

    const function_constant = interpret.bytecode.get_constant(closure.constant_idx);
    const compiled = interpret.jit_compiler.compile_fn(function_constant) catch @panic("could not compile");
    compiled.run(jit_state);
}

pub fn do_print(noalias interpret: *Interpreter, arg_count: u64) callconv(.C) void {
    const arg_count_tmp: u32 = @intCast(arg_count);
    const arg_slice = interpret.stack.slice_top(arg_count_tmp);
    for (arg_slice) |val| {
        switch (val.get_type()) {
            ValueType.string => {
                const idx = bc.ConstantIndex.new(val.get_idx());
                const string_const = interpret.bytecode.get_constant(idx);
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
    interpret.stack.pop_n(arg_count_tmp);
    std.debug.print("\n", .{});
    interpret.stack.push(Value.new_nil());
}

pub fn create_closure(noalias self: *Interpreter, constant_idx: u64, unbound_count: u64) callconv(.C) void {
    const constant_idx_tmp: u32 = @intCast(constant_idx);
    const unbound_count_tmp: u32 = @intCast(unbound_count);
    const env = self.stack.slice_top(unbound_count_tmp);
    const closure = self.gc.alloc_with_additional(bc.Closure, unbound_count_tmp, self.get_roots());
    closure.env.count = unbound_count_tmp;
    for (env, 0..) |val, idx| {
        closure.env.set(idx, val);
    }
    self.stack.pop_n(unbound_count_tmp);
    closure.constant_idx = bc.ConstantIndex.new(constant_idx_tmp);
    const code = self.bytecode.get_constant(closure.constant_idx);
    closure.local_count = code.get_u32(5);
    closure.param_count = code.get_u32(9);
    const val = Value.new_ptr(bc.Closure, closure, ValueType.closure);
    self.stack.push(val);
}

pub fn create_object(noalias self: *Interpreter, class_idx: bc.ConstantIndex) callconv(.C) void {
    const class_constant = self.bytecode.get_constant(class_idx);
    if (class_constant.get_type() != bc.ConstantType.class) {
        @panic("invalid object class");
    }
    const field_count = class_constant.get_class_field_count();
    const values = self.stack.slice_top(field_count);
    const object = self.gc.alloc_with_additional(bc.Object, field_count, self.get_roots());
    self.stack.pop_n(field_count);
    object.values.count = field_count;
    for (values, 0..) |val, idx| {
        object.values.set(idx, val);
    }
    object.class_idx = class_idx;
    const proto = self.stack.top();
    object.prototype = proto;
    self.stack.set_top(Value.new_ptr(bc.Object, object, ValueType.object));
}

pub fn do_get_field(noalias self: *Interpreter, string_idx: bc.ConstantIndex) callconv(.C) void {
    const val = self.stack.top();
    if (val.get_type() != ValueType.object) {
        @panic("invalid object");
    }
    const object = val.get_ptr(bc.Object);
    const field: ?Value = self.get_field(object, string_idx);

    if (field) |result| {
        self.stack.set_top(result);
    } else {
        @panic("non existant field");
    }
}

pub fn do_set_field(noalias self: *Interpreter, string_idx: bc.ConstantIndex) callconv(.C) void {
    const target = self.stack.pop();
    const val = self.stack.top();

    if (target.get_type() != ValueType.object) {
        @panic("cannot set into non object");
    }

    const object = target.get_ptr(bc.Object);
    const field_ptr: ?*Value = self.get_field_ptr(object, string_idx);
    if (field_ptr) |field| {
        field.* = val;
    } else {
        @panic("non existant field");
    }
}

const DBG_VALUE: bool = true;
const DBG_RAW: bool = false;
const DBG_INST: bool = true;

pub fn dbg(value: Value) callconv(.C) void {
    if (DBG_VALUE) {
        std.debug.print("VALUE {x} ", .{value.data});
        std.debug.print("{}\n", .{value});
    }
}

pub fn dbg_raw(val: u64) callconv(.C) void {
    if (DBG_RAW) {
        std.debug.print("RAW {x}\n", .{val});
    }
}

pub fn dbg_inst(inst_val: u64) callconv(.C) void {
    if (DBG_INST) {
        const inst: bc.Instruction = @enumFromInt(inst_val);
        std.debug.print("INST: {}\n", .{inst});
    }
}

pub fn binop_panic(left: Value, right: Value) callconv(.C) void {
    std.debug.print("left: {}, right: {}\n", .{ left, right });
    @panic("Unimplemented dispatch");
}

pub fn if_condition_panic() callconv(.C) void {
    @panic("If condition must be boolean");
}

pub fn string_panic() callconv(.C) void {
    @panic("Incorrect string");
}
