const std = @import("std");
const runtime = @import("runtime.zig");

pub const Instruction = enum(u8) {
    // literals
    push,
    pop,
    object,
    closure,
    nil,
    true,
    false,
    constant,

    // locals
    set,
    get,
    set_global,
    get_global,

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

    // other
    panic,
    assert,
};

pub const ConstantType = enum(u8) {
    function,
    string,
    class,
};

pub const Constant = struct {
    data: [*]const u8,

    pub fn new(data: [*]u8) Constant {
        return Constant{ .data = data };
    }

    pub fn get_data(self: *const Constant) [*]u8 {
        return @ptrCast(&self.data[4]);
    }

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

    pub fn get_slice(self: *const Constant) []u8 {
        const size = self.get_size() + 4;
        return self.data[0..size];
    }

    pub fn format(
        self: *const Constant,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{any}", .{self.get_slice()});
    }
};

pub const ConstantIndex = struct {
    index: u32,

    pub fn new(index: u32) ConstantIndex {
        return ConstantIndex{ .index = index };
    }
};

pub const Closure = struct {
    constant_idx: ConstantIndex,
    param_count: u32,
    local_count: u32,
    env: runtime.FlexibleArr(runtime.Value),

    pub fn additional_size(count: usize) usize {
        return runtime.FlexibleArr(runtime.Value).additional_size(count);
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
        return self.current[pc];
    }

    pub fn read_u32(self: *const Bytecode, pc: usize) u32 {
        const tmp: *const u32 = @ptrCast(@alignCast(&self.current.data[pc]));
        return @byteSwap(tmp.*);
    }

    pub fn get_constant(self: *const Bytecode, idx: ConstantIndex) Constant {
        return self.constants[@intCast(idx.index)];
    }

    pub fn set_curr_const(self: *Bytecode, idx: ConstantIndex) void {
        self.current = self.get_constant(idx);
    }
};
