const bc_interpret = @import("bc_interpreter.zig");
const bytecode = @import("bytecode.zig");
const runtime = @import("runtime.zig");

pub const GPR64 = enum(u4) {
    rax = 0,
    rbx = 3,
    rcx = 1,
    rdx = 2,
    rsi = 6,
    rdi = 7,
    rsp = 4,
    rbp = 5,

    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
};

pub const Scale = enum(u2) {
    scale1 = 0b00,
    scale2 = 0b01,
    scale4 = 0b10,
    scale8 = 0b11,

    pub fn from_size(size: usize) Scale {
        return switch (size) {
            0 => Scale.scale1,
            2 => Scale.scale2,
            4 => Scale.scale4,
            8 => Scale.scale8,
            else => @panic("invalid scale"),
        };
    }
};

/// struct representing state of interpreter
/// for jit compiled code this is necessary because
/// normal structs in zig do not gurantee order of
/// fields and jit code needs different set of fields
/// then the bytecode interpreter
pub const JitState = extern struct {
    intepreter: *const bc_interpret.JitInterpreter,
    stack: *const bc_interpret.Stack,
    env: *const bc_interpret.Environment,
    gc: *const bc_interpret.GC,

    // basic
    alloc_stack: *const fn (*bc_interpret.Stack, usize) callconv(.C) void,

    // objects handle
    create_closure: *const fn (noalias *bc_interpret.JitInterpreter, u64, u64) callconv(.C) void,
    create_object: *const fn (noalias *bc_interpret.JitInterpreter, bytecode.ConstantIndex) callconv(.C) void,
    get_field: *const fn (noalias *bc_interpret.JitInterpreter, bytecode.ConstantIndex) callconv(.C) void,
    set_field: *const fn (noalias *bc_interpret.JitInterpreter, bytecode.ConstantIndex) callconv(.C) void,

    // calls
    call: *const fn (noalias *bc_interpret.JitInterpreter, noalias *const JitState) callconv(.C) void,
    method_call: *const fn (noalias *bc_interpret.JitInterpreter, noalias *const JitState, bytecode.ConstantIndex) callconv(.C) void,
    print: *const fn (noalias *bc_interpret.JitInterpreter, arg_count: u64) callconv(.C) void,

    // debug
    dbg: *const fn (runtime.Value) callconv(.C) void,
    dbg_raw: *const fn (u64) callconv(.C) void,
    dbg_inst: *const fn (u64) callconv(.C) void,

    // panics
    binop_panic: *const fn (runtime.Value, runtime.Value) callconv(.C) void,
    if_condition_panic: *const fn () callconv(.C) void,
    string_panic: *const fn () callconv(.C) void,

    pub fn get_offset(comptime field_name: []const u8) u32 {
        return @offsetOf(JitState, field_name);
    }
};

pub const JitFunction = struct {
    code: [*]u8,

    pub fn run(self: *const JitFunction, state: *const JitState) void {
        const f: *const fn (*const JitState) callconv(.C) void = @alignCast(@ptrCast(self.code));
        f(state);
    }
};
