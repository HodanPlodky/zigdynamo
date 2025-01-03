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
    static_call,
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
            Instruction.get => "get",
            Instruction.set_global => "set_global",
            Instruction.get_global => "get_global",
            Instruction.add => "add",
            Instruction.sub => "sub",
            Instruction.mul => "mul",
            Instruction.div => "div",
            Instruction.lt => "lt",
            Instruction.gt => "gt",
            Instruction.eq => "eq",
            Instruction.ne => "ne",
            Instruction.call => "call",
            Instruction.static_call => "static_call",
            Instruction.print => "print",
            Instruction.methodcall => "methodcall",
            Instruction.ret => "ret",
            Instruction.ret_main => "ret_main",
            Instruction.get_field => "get_field",
            Instruction.set_field => "set_field",
            Instruction.branch => "branch",
            Instruction.jump => "jump",
            Instruction.panic => "panic",
            Instruction.assert => "assert",
        };
    }

    pub fn get_extrabytes(self: Instruction) usize {
        return switch (self) {
            Instruction.push_byte => 1,
            Instruction.push,
            Instruction.jump,
            Instruction.branch,
            Instruction.get,
            Instruction.get_global,
            Instruction.set,
            Instruction.set_global,
            Instruction.print,
            Instruction.string,
            => 4,
            Instruction.closure => 8,
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

    pub fn get_slice(self: *const Constant) []const u8 {
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
        //try writer.print("{any}", .{self.get_slice()});
        var i: usize = switch (self.get_type()) {
            ConstantType.function => 13,
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

pub const Closure = packed struct {
    constant_idx: ConstantIndex,
    param_count: u32,
    local_count: u32,
    paddingdontuse: u32,
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
