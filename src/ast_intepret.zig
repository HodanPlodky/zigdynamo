const std = @import("std");
const ast = @import("ast.zig");
const runtime = @import("runtime.zig");

const Heap = runtime.Heap;
const FlexibleArr = runtime.FlexibleArr;
const ValueType = runtime.ValueType;
const Value = runtime.Value;

const Closure = packed struct {
    body: *ast.Ast,
    env: *StoreEnv,
    args: FlexibleArr([]const u8),

    pub fn additional_size(count: usize) usize {
        return FlexibleArr([]const u8).additional_size(count);
    }
};

const Field = struct {
    name: []const u8,
    value: Value,
};

const Object = packed struct {
    prototype: Value,
    fields: FlexibleArr(Field),

    pub fn additional_size(count: usize) usize {
        return FlexibleArr(Field).additional_size(count);
    }

    pub fn get_val_ptr(self: *Object, name: []const u8) ?*Value {
        for (0..self.fields.count) |i| {
            const field = self.fields.get_ptr(i);
            if (std.mem.eql(u8, name, field.name)) {
                return field.value;
            }
        }

        return null;
    }

    pub fn get_val(self: *Object, name: []const u8) ?Value {
        for (0..self.fields.count) |i| {
            const field = self.fields.get(i);
            if (std.mem.eql(u8, name, field.name)) {
                return field.value;
            }
        }

        if (self.prototype.get_type() == ValueType.object) {
            const proto = self.prototype.get_ptr(Object);
            return proto.get_val(name);
        } else {
            return null;
        }
    }
};

const String = struct {
    data: []const u8,
};

const StoreVar = struct {
    name: []const u8,
    value: Value,
};

const StoreEnv = packed struct {
    data: FlexibleArr(StoreVar),

    pub fn additional_size(count: usize) usize {
        return FlexibleArr(StoreVar).additional_size(count);
    }
};

const Frame = struct {
    data: std.StringHashMap(Value),

    pub fn new(alloc: std.mem.Allocator) Frame {
        return Frame{ .data = std.StringHashMap(Value).init(alloc) };
    }

    pub fn deinit(self: *Frame) void {
        self.data.deinit();
    }

    pub fn add(self: *Frame, name: []const u8, value: Value) !void {
        try self.data.put(name, value);
    }

    pub fn get_ptr(self: *const Frame, name: []const u8) ?*Value {
        return self.data.getPtr(name);
    }

    pub fn get(self: *const Frame, name: []const u8) ?Value {
        return self.data.get(name);
    }
};

/// Local environment for function
const FnEnvironment = struct {
    frames: std.ArrayList(Frame),
    alloc: std.mem.Allocator,

    pub fn new(alloc: std.mem.Allocator, start_frame: Frame) !FnEnvironment {
        var frames = std.ArrayList(Frame).init(alloc);
        try frames.append(start_frame);
        return FnEnvironment{
            .frames = frames,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *FnEnvironment) void {
        for (self.frames.items) |*frame| {
            frame.deinit();
        }
        self.frames.deinit();
    }

    pub fn add(self: *FnEnvironment, name: []const u8, value: Value) !void {
        var last_frame = &self.frames.items[self.frames.items.len - 1];
        try last_frame.add(name, value);
    }

    pub fn get_ptr(self: *const FnEnvironment, name: []const u8) ?*Value {
        var index: usize = self.frames.items.len;
        while (index != 0) {
            index -= 1;
            if (self.frames.items[index].get_ptr(name)) |res| {
                return res;
            }
        }
        return null;
    }

    pub fn get(self: *const FnEnvironment, name: []const u8) ?Value {
        var index: usize = self.frames.items.len;
        while (index != 0) {
            index -= 1;
            if (self.frames.items[index].get(name)) |res| {
                return res;
            }
        }
        return null;
    }

    pub fn push_frame(self: *FnEnvironment) !void {
        try self.frames.append(Frame.new(self.alloc));
    }

    pub fn pop_frame(self: *FnEnvironment) void {
        var tmp = self.frames.pop();
        tmp.deinit();
    }
};

const Enviroment = struct {
    global: Frame,
    stacks: std.ArrayList(FnEnvironment),
    alloc: std.mem.Allocator,

    pub fn new(alloc: std.mem.Allocator) !Enviroment {
        return Enviroment{
            .global = Frame.new(alloc),
            .stacks = std.ArrayList(FnEnvironment).init(alloc),
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Enviroment) void {
        for (self.stacks.items) |*frame| {
            frame.deinit();
        }
        self.stacks.deinit();
        self.global.deinit();
    }

    pub fn get_ptr(self: *Enviroment, name: []const u8) ?*Value {
        const current = self.get_current();
        if (current) |curr| {
            if (curr.get_ptr(name)) |res| {
                return res;
            }
        }
        if (self.global.get_ptr(name)) |res| {
            return res;
        }
        return null;
    }

    pub fn get(self: *const Enviroment, name: []const u8) Value {
        const current = self.get_current();
        if (current) |curr| {
            if (curr.get(name)) |res| {
                return res;
            }
        }
        if (self.global.get(name)) |res| {
            return res;
        }
        return Value.new_nil();
    }

    pub fn add(self: *Enviroment, name: []const u8, value: Value) !void {
        if (self.get_current()) |curr| {
            try curr.add(name, value);
        } else {
            try self.global.add(name, value);
        }
    }

    pub fn push_frame(self: *Enviroment) !void {
        var last_stack = &self.stacks.items[self.stacks.items.len - 1];
        try last_stack.push_frame();
    }

    pub fn pop_frame(self: *Enviroment) void {
        var last_stack = &self.stacks.items[self.stacks.items.len - 1];
        last_stack.pop_frame();
    }

    pub fn pop_call(self: *Enviroment) void {
        var tmp = self.stacks.pop();
        tmp.deinit();
    }

    pub fn push_call(self: *Enviroment, frame: Frame) !void {
        const fn_env = try FnEnvironment.new(self.alloc, frame);
        try self.stacks.append(fn_env);
    }

    pub fn get_1d_copy(self: *Enviroment) Frame {
        var res_frame = Frame.new(self.alloc);
        if (self.get_current()) |curr| {
            for (curr.frames.items) |*frame| {
                var iter = frame.data.iterator();
                while (iter.next()) |item| {
                    res_frame.add(item.key_ptr.*, item.value_ptr.*) catch unreachable;
                }
            }
        }
        return res_frame;
    }

    pub fn get_current(self: *const Enviroment) ?*FnEnvironment {
        if (self.stacks.items.len == 0) {
            return null;
        }
        return &self.stacks.items[self.stacks.items.len - 1];
    }
};

pub const Interpret = struct {
    heap: Heap,
    environment: Enviroment,
    scratch_alloc: std.mem.Allocator,

    pub fn init(heap_data: []u8, scratch_alloc: std.mem.Allocator) Interpret {
        const env: Enviroment = Enviroment.new(scratch_alloc) catch unreachable;
        return Interpret{
            .heap = Heap.init(heap_data),
            .environment = env,
            .scratch_alloc = scratch_alloc,
        };
    }

    pub fn deinit(self: *Interpret) void {
        self.environment.deinit();
    }

    pub fn run(self: *Interpret, program: ast.Program) u32 {
        var res = Value.new_nil();

        for (program.data) |*item| {
            res = self.eval(item);
        }

        return res.get_number();
    }

    fn eval(self: *Interpret, node: *ast.Ast) Value {
        switch (node.*) {
            ast.Ast.number => |num| return Value.new_num(num),
            ast.Ast.string => |string| {
                var res = self.heap.alloc(String);
                res.data = string;
                return Value.new_ptr(String, res, ValueType.string);
            },
            ast.Ast.nil => return Value.new_nil(),
            ast.Ast.binop => |binop| return self.binop_dispatch(binop),
            ast.Ast.call => |call| return self.handle_call(call),
            ast.Ast.let => |let| return self.handle_let(let),
            ast.Ast.ident => |ident| return self.environment.get(ident),
            ast.Ast.function => |function| return self.handle_function(function),
            ast.Ast.bool => |val| if (val) {
                return Value.new_true();
            } else {
                return Value.new_false();
            },
            ast.Ast.condition => |condition| return self.handle_condition(condition),
            ast.Ast.loop => |loop| return self.handle_loop(loop),
            ast.Ast.object => |object| return self.handle_object(object),
            ast.Ast.field_access => |field_access| return self.handle_field_access(field_access),
            ast.Ast.assign => |assign| return self.handle_assign(assign),
            ast.Ast.field_assign => |assign| return self.handle_field_assign(assign),
            ast.Ast.field_call => |method| return self.handle_method(method),
            ast.Ast.block => |block| return self.handle_block(block),
            else => unreachable,
        }
    }

    fn binop_dispatch(self: *Interpret, binop: ast.BinOp) Value {
        const left = self.eval(binop.left);
        const right = self.eval(binop.right);
        switch (binop.op) {
            'e' => return Value.new_bool(left.data == right.data),
            'n' => return Value.new_bool(left.data != right.data),
            else => {},
        }
        switch (left.get_type()) {
            ValueType.number => {
                if (right.get_type() != ValueType.number) {
                    @panic("cannot do binop on number and non number");
                }

                switch (binop.op) {
                    '+' => return Value.new_raw(left.data + right.data),
                    '-' => return Value.new_raw(((left.data >> 32) - (right.data >> 32)) << 32),
                    '*' => return Value.new_raw(((left.data >> 32) * (right.data >> 32)) << 32),
                    '/' => return Value.new_raw(((left.data >> 32) / (right.data >> 32)) << 32),
                    '<' => return Value.new_bool(left.data < right.data),
                    '>' => return Value.new_bool(left.data > right.data),
                    else => @panic("non existant operator"),
                }
            },
            else => @panic("dispatch"),
        }
    }

    fn handle_call(self: *Interpret, call: ast.Call) Value {
        const target = switch (call.target.*) {
            ast.Ast.print_fn => return self.handle_print(call.args),
            else => self.eval(call.target),
        };
        if (target.get_type() != ValueType.closure) {
            @panic("Cannot call on this object");
        }

        const closure = target.get_ptr(Closure);
        var frame = Frame.new(self.scratch_alloc);

        for (0..closure.env.data.count) |i| {
            const store_var = closure.env.data.get(i);
            frame.add(store_var.name, store_var.value) catch unreachable;
        }

        for (0..closure.args.count) |i| {
            frame.add(closure.args.get(i), self.eval(&call.args[i])) catch unreachable;
        }

        self.environment.push_call(frame) catch unreachable;
        const res = self.eval(closure.body);
        self.environment.pop_call();

        return res;
    }

    fn handle_print(self: *Interpret, args: []ast.Ast) Value {
        for (args) |*arg| {
            const val = self.eval(arg);
            switch (val.get_type()) {
                ValueType.string => {
                    const data = val.get_ptr(String);
                    std.debug.print("{s} ", .{data.data});
                },
                ValueType.number => {
                    const data = val.get_number();
                    std.debug.print("{} ", .{data});
                },
                else => @panic("Cannot print"),
            }
        }
        std.debug.print("\n", .{});
        return Value.new_nil();
    }

    fn handle_let(self: *Interpret, let: ast.Let) Value {
        const value = self.eval(let.value);
        self.environment.add(let.target, value) catch @panic("could not add var");
        return value;
    }

    fn handle_function(self: *Interpret, function: ast.Function) Value {
        var res = self.heap.alloc_with_additional(Closure, function.params.len);
        res.body = function.body;
        res.args.count = function.params.len;
        for (function.params, 0..) |par, index| {
            res.args.set(index, par);
        }

        var store_frame = self.environment.get_1d_copy();
        defer store_frame.deinit();
        const var_count = store_frame.data.count();
        var store_env = self.heap.alloc_with_additional(StoreEnv, var_count);
        store_env.data.count = var_count;
        var iter = store_frame.data.iterator();

        var index: usize = 0;
        while (iter.next()) |item| {
            const store_var = StoreVar{
                .name = item.key_ptr.*,
                .value = item.value_ptr.*,
            };
            store_env.data.set(index, store_var);
            index += 1;
        }
        res.env = store_env;

        return Value.new_ptr(Closure, res, ValueType.closure);
    }

    fn handle_condition(self: *Interpret, condition: ast.Condition) Value {
        const cond = self.eval(condition.cond);
        switch (cond.get_type()) {
            ValueType.true => return self.eval(condition.then_block),
            ValueType.false => if (condition.else_block) |else_block| {
                return self.eval(else_block);
            } else {
                return Value.new_nil();
            },
            else => @panic("If condition must be boolean"),
        }
    }

    fn handle_loop(self: *Interpret, loop: ast.Loop) Value {
        var res = Value.new_nil();
        while (true) {
            const cond = self.eval(loop.cond);
            switch (cond.get_type()) {
                ValueType.true => res = self.eval(loop.body),
                ValueType.false => break,
                else => @panic("loop condition must be boolean"),
            }
        }
        return res;
    }

    fn handle_object(self: *Interpret, object: ast.Object) Value {
        const prototype =
            if (object.prototype) |proto|
            self.eval(proto)
        else
            Value.new_nil();

        var result = self.heap.alloc_with_additional(Object, object.fields.len);
        result.prototype = prototype;
        result.fields.count = object.fields.len;

        for (object.fields, 0..) |field, index| {
            const value = self.eval(field.value);
            result.fields.set(index, Field{
                .name = field.name,
                .value = value,
            });
        }

        return Value.new_ptr(Object, result, ValueType.object);
    }

    fn handle_field_access(self: *Interpret, field_access: ast.FieldAccess) Value {
        const target = self.eval(field_access.target);

        if (target.get_type() != ValueType.object) {
            @panic("target of field access must be object");
        }

        const object = target.get_ptr(Object);

        if (object.get_val(field_access.field)) |res| {
            return res;
        }
        @panic("Non existing field");
    }

    fn handle_block(self: *Interpret, block: []ast.Ast) Value {
        var res = Value.new_nil();
        self.environment.push_frame() catch unreachable;
        for (block) |*item| {
            res = self.eval(item);
        }
        self.environment.pop_frame();
        return res;
    }

    fn handle_assign(self: *Interpret, assign: ast.Assign) Value {
        const place = self.environment.get_ptr(assign.target);
        if (place) |val_ptr| {
            const value = self.eval(assign.value);
            val_ptr.* = value;
            return value;
        } else {
            @panic("cannot assign");
        }
    }

    fn handle_field_assign(self: *Interpret, assing: ast.FieldAssign) Value {
        const object_val = self.eval(assing.object);
        if (object_val.get_type() != ValueType.object) {
            @panic("cannot field assing onto non object");
        }

        const object = object_val.get_ptr(Object);

        for (0..object.fields.count) |i| {
            const field = object.fields.get_ptr(i);
            if (std.mem.eql(u8, field.name, assing.field)) {
                const val = self.eval(assing.value);
                field.value = val;
                return val;
            }
        }
        @panic("Field does not exist");
    }

    fn handle_method(self: *Interpret, method: ast.FieldCall) Value {
        const target = self.eval(method.target);
        if (target.get_type() != ValueType.object) {
            @panic("Invalid target for method call");
        }
        const object = target.get_ptr(Object);
        const field = if (object.get_val(method.field)) |field|
            field
        else
            @panic("Field not found");

        if (field.get_type() != ValueType.closure) {
            @panic("Field is not function");
        }

        const closure = field.get_ptr(Closure);
        var frame = Frame.new(self.scratch_alloc);

        for (0..closure.env.data.count) |i| {
            const store_var = closure.env.data.get(i);
            frame.add(store_var.name, store_var.value) catch unreachable;
        }

        frame.add("this", target) catch unreachable;

        for (0..closure.args.count) |i| {
            frame.add(closure.args.get(i), self.eval(&method.args[i])) catch unreachable;
        }

        self.environment.push_call(frame) catch unreachable;
        const res = self.eval(closure.body);
        self.environment.pop_call();

        return res;
    }
};
