const std = @import("std");

pub const Heap = struct {
    data: []u8,
    curr_ptr: usize,

    // there are some object that are
    // 16 align and I need to be able
    // to parse the heap
    // so that is why I set static
    // align and not do the align in
    // comptime
    pub const heap_align: usize = 16;

    pub fn init(data: []u8) Heap {
        return Heap{
            .data = data,
            .curr_ptr = 0,
        };
    }

    pub fn alloc(self: *Heap, comptime T: type) *T {
        // align without the branch
        self.curr_ptr = (self.curr_ptr + (heap_align - 1)) & ~(heap_align - 1);

        const res: *T = @ptrCast(@alignCast(&self.data[self.curr_ptr]));
        self.curr_ptr += @sizeOf(T);
        return res;
    }

    pub fn alloc_with_additional(self: *Heap, comptime T: type, count: usize) *T {
        // align without the branch
        self.curr_ptr = (self.curr_ptr + (heap_align - 1)) & ~(heap_align - 1);

        const res: *T = @ptrCast(@alignCast(&self.data[self.curr_ptr]));
        self.curr_ptr += @sizeOf(T) + T.additional_size(count);
        return res;
    }

    pub fn check_available(self: *const Heap, comptime T: type, count: usize) bool {
        const pos = (self.curr_ptr + (heap_align - 1)) & ~(heap_align - 1);
        const needed = @sizeOf(T) + T.additional_size(count);
        return self.data.len > needed + pos;
    }
};

pub fn FlexibleArr(comptime T: type) type {
    return packed struct {
        const Self = @This();
        count: usize,

        pub fn additional_size(count: usize) usize {
            return @sizeOf(T) * count;
        }

        pub fn get_ptr(self: *Self, index: usize) *T {
            const place = @intFromPtr(&self.count);
            return @ptrFromInt(place + @sizeOf(usize) + @sizeOf(T) * index);
        }

        pub fn get(self: *Self, index: usize) T {
            return self.get_ptr(index).*;
        }

        pub fn set(self: *Self, index: usize, val: T) void {
            self.get_ptr(index).* = val;
        }
    };
}

pub const ValueType = enum(u4) {
    number = 0,
    nil = 1,
    closure = 2,
    object = 3,
    array = 4,
    string = 7,
    false = 8,
    true = 9, // this is because of better checking
};

// this will assume 64bit size of the pointer
pub const Value = packed struct {
    data: u64,

    pub fn new_raw(val: u64) Value {
        return Value{
            .data = val,
        };
    }

    pub fn new_num(val: u32) Value {
        const tmp: u64 = @intCast(val);
        return Value{
            .data = tmp << 32,
        };
    }

    pub fn new_nil() Value {
        return Value{
            .data = @intFromEnum(ValueType.nil),
        };
    }

    pub fn new_true() Value {
        return Value{ .data = @intFromEnum(ValueType.true) };
    }

    pub fn new_false() Value {
        return Value{ .data = @intFromEnum(ValueType.false) };
    }

    pub fn new_bool(value: bool) Value {
        if (value) {
            return Value.new_true();
        } else {
            return Value.new_false();
        }
    }

    pub fn new_ptr(comptime T: type, val: *T, comptime heap_type: ValueType) Value {
        comptime {
            const info = @typeInfo(T);
            if (info.@"struct".layout != std.builtin.Type.ContainerLayout.@"packed") {
                @compileError("struct on heap must be packed " ++ @typeName(T));
            }
            if (info.@"struct".fields.len == 0) {
                @compileError("struct on heap have tag");
            }
            if (!std.mem.eql(u8, "tag", info.@"struct".fields[0].name)) {
                @compileError("struct on heap have tag as a first field");
            }
        }
        @field(val.*, "tag") = @intFromEnum(heap_type);
        return Value{
            .data = @intFromPtr(val) | @intFromEnum(heap_type),
        };
    }

    pub fn new_string(idx: u32) Value {
        var res = Value.new_num(idx);
        res.data |= @intFromEnum(ValueType.string);
        return res;
    }

    pub fn get_type(self: Value) ValueType {
        //std.debug.print("{x}\n", .{self.data});
        return @enumFromInt(self.data & 0xf);
    }

    pub fn is_ptr(self: Value) bool {
        return switch (self.get_type()) {
            ValueType.closure,
            ValueType.object,
            ValueType.array,
            => true,
            else => false,
        };
    }

    pub fn get_number(self: Value) u32 {
        switch (self.get_type()) {
            ValueType.number => return @intCast(self.data >> 32),
            ValueType.nil => return 0,
            else => @panic("unsupported value type deref"),
        }
    }

    // uncheck if it is even pointer
    pub fn get_ptr(self: Value, comptime T: type) *T {
        const ptr: *T = @ptrFromInt(self.data & (0xfffffffffffffff0));
        return ptr;
    }

    pub fn get_ptr_raw(self: Value) [*]u8 {
        const ptr: [*]u8 = @ptrFromInt(self.data & (0xfffffffffffffff0));
        return ptr;
    }

    // assumes this is ptr
    pub fn rewrite_ptr(self: Value, ptr: usize) Value {
        const data = ptr | (self.data & 0xf);
        return Value.new_raw(data);
    }

    pub fn get_idx(self: Value) u32 {
        return @intCast(self.data >> 32);
    }

    // operations assume number value type

    pub fn add(left: Value, right: Value) Value {
        return Value.new_raw(left.data + right.data);
    }

    pub fn sub(left: Value, right: Value) Value {
        // TODO: check
        return Value.new_raw(left.data - right.data);
    }

    pub fn mul(left: Value, right: Value) Value {
        return Value.new_raw(((left.data >> 32) * (right.data >> 32)) << 32);
    }

    pub fn div(left: Value, right: Value) Value {
        return Value.new_raw(((left.data >> 32) / (right.data >> 32)) << 32);
    }

    pub fn gt(left: Value, right: Value) Value {
        const tmp: u64 = @intFromBool(left.data > right.data);
        return Value.new_raw(tmp + @intFromEnum(ValueType.false));
    }

    pub fn lt(left: Value, right: Value) Value {
        const tmp: u64 = @intFromBool(left.data < right.data);
        return Value.new_raw(tmp + @intFromEnum(ValueType.false));
    }

    // either value or ptr compare
    pub fn eq(left: Value, right: Value) Value {
        return Value.new_bool(left.data == right.data);
    }

    pub fn ne(left: Value, right: Value) Value {
        return Value.new_bool(left.data != right.data);
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options; // autofix
        _ = fmt; // autofix
        try switch (self.get_type()) {
            ValueType.number => writer.print("{}", .{self.get_number()}),
            ValueType.nil => writer.print("nil", .{}),
            ValueType.array => writer.print("array", .{}),
            ValueType.false => writer.print("false", .{}),
            ValueType.true => writer.print("true", .{}),
            ValueType.object => writer.print("object", .{}),
            ValueType.string => writer.print("string", .{}),
            ValueType.closure => writer.print("closure", .{}),
        };
    }
};

pub fn print(values: []Value) void {
    for (values) |val| {
        switch (val.get_type()) {
            ValueType.string => {
                //const data = val.get_ptr(String);
                std.debug.print("TODO ", .{});
            },
            ValueType.number => {
                const data = val.get_number();
                std.debug.print("{} ", .{data});
            },
            else => @panic("Cannot print"),
        }
    }
    std.debug.print("\n", .{});
}
