const std = @import("std");
const runtime = @import("runtime.zig");

pub const Instruction = enum(u8) {
    // literals
    push,
    push_byte,
    pop,
    dup,
    object,
    closure,
    nil,
    true,
    false,
    string,

    // locals
    set,
    set_small,
    get,
    get_small,
    set_global,
    get_global,
    set_global_small,
    get_global_small,

    // math
    add,
    sub,
    mul,
    div,
    lt,
    gt,
    eq,
    ne,

    // calls
    call,
    //static_call,
    print,
    methodcall,
    ret,
    ret_main,

    // objects
    get_field,
    set_field,

    // branching
    branch,
    jump,

    pub fn format(
        self: *const Instruction,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{s}", .{self.get_str()});
    }

    pub fn get_str(self: Instruction) []const u8 {
        return switch (self) {
            Instruction.push => "push",
            Instruction.push_byte => "push_byte",
            Instruction.pop => "pop",
            Instruction.dup => "dup",
            Instruction.object => "object",
            Instruction.closure => "closure",
            Instruction.nil => "nil",
            Instruction.true => "true",
            Instruction.false => "false",
            Instruction.string => "string",
            Instruction.set => "set",
            Instruction.set_small => "set_small",
            Instruction.get => "get",
            Instruction.get_small => "get_small",
            Instruction.set_global => "set_global",
            Instruction.get_global => "get_global",
            Instruction.set_global_small => "set_global_small",
            Instruction.get_global_small => "get_global_small",
            Instruction.add => "add",
            Instruction.sub => "sub",
            Instruction.mul => "mul",
            Instruction.div => "div",
            Instruction.lt => "lt",
            Instruction.gt => "gt",
            Instruction.eq => "eq",
            Instruction.ne => "ne",
            Instruction.call => "call",
            Instruction.print => "print",
            Instruction.methodcall => "methodcall",
            Instruction.ret => "ret",
            Instruction.ret_main => "ret_main",
            Instruction.get_field => "get_field",
            Instruction.set_field => "set_field",
            Instruction.branch => "branch",
            Instruction.jump => "jump",
        };
    }

    pub fn get_extrabytes(self: Instruction) usize {
        return switch (self) {
            Instruction.push_byte,
            Instruction.get_small,
            Instruction.set_small,
            Instruction.get_global_small,
            Instruction.set_global_small,
            => 1,
            Instruction.push,
            Instruction.jump,
            Instruction.branch,
            Instruction.get,
            Instruction.get_global,
            Instruction.get_field,
            Instruction.set,
            Instruction.set_global,
            Instruction.set_field,
            Instruction.methodcall,
            Instruction.print,
            Instruction.string,
            Instruction.object,
            => 4,
            Instruction.closure,
            => 8,
            else => 0,
        };
    }
};

pub const ConstantType = enum(u8) {
    main_function,
    function,
    string,
    class,

    pub fn format(
        self: *const ConstantType,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options; // autofix
        _ = fmt; // autofix
        const tmp = switch (self.*) {
            ConstantType.main_function => "main_function",
            ConstantType.function => "function",
            ConstantType.string => "string",
            ConstantType.class => "class",
        };
        try writer.print("{s}", .{tmp});
    }
};

pub const Constant = struct {
    /// this is length of function header
    ///
    /// Header
    /// len (4 bytes) | type (byte) | locals count (4 bytes)| param count (4 bytes) | jit offset (4 bytes) | instructions...
    /// if the jit offset is zero then you can assume that the
    /// function is not jitted and still in bytecode
    pub const function_header_size: usize = 4 + 1 + 4 + 4 + 4;
    data: [*]u8,

    pub fn new(data: [*]u8) Constant {
        return Constant{ .data = data };
    }

    pub fn get_data(self: *const Constant) [*]u8 {
        return @ptrCast(&self.data[4]);
    }

    /// returns the number of bytes in constant
    /// excluding the bytes used to store the len
    /// it self (4 bytes)
    pub fn get_size(self: *const Constant) usize {
        var res: usize = 0;
        res |= @intCast(self.data[0]);
        res <<= 8;
        res |= @intCast(self.data[1]);
        res <<= 8;
        res |= @intCast(self.data[2]);
        res <<= 8;
        res |= @intCast(self.data[3]);
        return res;
    }

    pub fn get_type(self: *const Constant) ConstantType {
        return @enumFromInt(self.data[4]);
    }

    pub fn get_u32(self: *const Constant, idx: usize) u32 {
        var res: u32 = 0;
        res |= @intCast(self.data[idx]);
        res <<= 8;
        res |= @intCast(self.data[idx + 1]);
        res <<= 8;
        res |= @intCast(self.data[idx + 2]);
        res <<= 8;
        res |= @intCast(self.data[idx + 3]);
        return res;
    }

    pub fn get_slice(self: *const Constant) []const u8 {
        const size = self.get_size() + 4;
        return self.data[0..size];
    }

    pub fn get_class_field_count(self: *const Constant) u32 {
        return @intCast((self.get_size() - 1) / 4);
    }

    pub fn get_class_field(self: *const Constant, idx: u32) u32 {
        return self.get_u32(idx * 4 + 5);
    }

    pub fn get_class_field_position(self: *const Constant, field_idx: ConstantIndex) ?usize {
        for (0..self.get_class_field_count()) |idx| {
            if (field_idx.index == self.get_class_field(@intCast(idx))) {
                return idx;
            }
        }
        return null;
    }

    pub fn format(
        self: *const Constant,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        //try writer.print("{any}", .{self.get_slice()});
        try switch (self.get_type()) {
            ConstantType.function, ConstantType.main_function => self.function_format(writer),
            ConstantType.string => {
                try writer.print("string: {s}\n", .{self.get_slice()[5..]});
            },
            ConstantType.class => {
                try writer.print("class:", .{});
                const field_count = (self.get_size() - 1) / 4;
                for (0..field_count) |index| {
                    try writer.print(" {}", .{self.get_u32(index * 4 + 5)});
                }
            },
        };
    }

    fn function_format(
        self: *const Constant,
        writer: anytype,
    ) !void {
        var i: usize = switch (self.get_type()) {
            ConstantType.function => Constant.function_header_size,
            ConstantType.main_function => 5,
            else => 0,
        };
        const size = self.get_size() + 4;
        while (i < size) {
            const inst: Instruction = @enumFromInt(self.data[i]);
            try writer.print("\t{}: {}", .{ i, inst });
            i += 1;
            for (0..inst.get_extrabytes()) |_| {
                try writer.print(" {}", .{self.data[i]});
                i += 1;
            }
            try writer.print("\n", .{});
        }
    }
};

pub const ConstantIndex = packed struct {
    index: u32,

    pub fn new(index: u32) ConstantIndex {
        return ConstantIndex{ .index = index };
    }
};

pub const FORWARD_TAG = 0xffffffff;
// must be smaller then all other heap objects
pub const Forward = packed struct {
    tag: u32,
    ptr: u32,
};

pub const Closure = packed struct {
    // tag is u32 because of the padding
    tag: u32,
    constant_idx: ConstantIndex,
    param_count: u32,
    local_count: u32,
    env: runtime.FlexibleArr(runtime.Value),

    pub fn additional_size(count: usize) usize {
        return runtime.FlexibleArr(runtime.Value).additional_size(count);
    }

    pub fn get_size(self: *const Closure) usize {
        return @sizeOf(Closure) + Closure.additional_size(self.env.count);
    }
};

pub const Object = packed struct {
    // tag is u32 because of the padding
    tag: u32,
    class_idx: ConstantIndex,
    prototype: runtime.Value,
    values: runtime.FlexibleArr(runtime.Value),

    pub fn additional_size(count: usize) usize {
        return runtime.FlexibleArr(runtime.Value).additional_size(count);
    }

    pub fn get_size(self: *const Object) usize {
        return @sizeOf(Object) + Object.additional_size(self.values.count);
    }
};

pub const Bytecode = struct {
    constants: []Constant,
    current: Constant,
    global_count: usize,

    pub fn read_inst(self: *const Bytecode, pc: usize) Instruction {
        return @enumFromInt(self.current.data[pc]);
    }

    pub fn read_u8(self: *const Bytecode, pc: usize) u8 {
        return self.current.data[pc];
    }

    pub fn read_u32(self: *const Bytecode, pc: usize) u32 {
        const tmp_arr: [4]u8 align(4) = .{
            self.current.data[pc + 3],
            self.current.data[pc + 2],
            self.current.data[pc + 1],
            self.current.data[pc],
        };
        const tmp: *const u32 = @ptrCast(@alignCast(&tmp_arr));
        return tmp.*;
    }

    pub fn get_constant(self: *const Bytecode, idx: ConstantIndex) Constant {
        return self.constants[@intCast(idx.index)];
    }

    pub fn get_type(self: *const Bytecode, idx: ConstantIndex) ConstantType {
        return self.constants[@intCast(idx.index)].get_type();
    }

    pub fn set_curr_const(self: *Bytecode, idx: ConstantIndex) void {
        self.current = self.get_constant(idx);
    }

    pub fn format(
        self: *const Bytecode,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options; // autofix
        _ = fmt; // autofix
        for (self.constants) |c| {
            const len = c.get_size() + 4;
            const typ = c.get_type();
            try writer.print("{} ({} bytes)\n", .{ typ, len });
            try writer.print("{}\n", .{c});
        }
    }
};
