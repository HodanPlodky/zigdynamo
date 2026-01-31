const std = @import("std");

pub const Program = struct {
    data: []Ast,
};

pub const Ast = union(enum) {
    number: u32,
    string: String,
    bool: bool,
    nil,
    ident: String,
    binop: BinOp,
    call: Call,
    function: Function,
    assign: Assign,
    block: []Ast,
    let: Let,
    condition: Condition,
    loop: Loop,
    object: Object,
    field_access: FieldAccess,
    field_assign: FieldAssign,
    field_call: FieldCall,

    // builtins
    print_fn,
};

pub const String = struct {
    value: []const u8,
    // is overriden during bc compilation
    constant_idx: u32 = std.math.maxInt(u32),
};

pub const BinOp = struct {
    op: u8, // as str
    left: *Ast,
    right: *Ast,
};

pub const Call = struct {
    target: *Ast,
    args: []Ast,
};

pub const Function = struct {
    params: []String,
    body: *Ast,

    // calculated after the parsing
    function_idx: u32 = std.math.maxInt(u32),
    env_vars: []String = &.{},
    env_start: u32 = std.math.maxInt(u32),
    is_method: bool = false,
};

pub const Assign = struct {
    target: String,
    value: *Ast,
};

pub const Let = struct {
    target: String,
    value: *Ast,
};

pub const Field = struct {
    name: String,
    value: *Ast,
};

pub const Object = struct {
    prototype: ?*Ast,
    fields: []Field,

    // set afterwards
    class_idx: u32 = std.math.maxInt(u32),
};

pub const FieldAccess = struct {
    target: *Ast,
    field: String,
};

pub const FieldCall = struct {
    target: *Ast,
    field: String,
    args: []Ast,
};

pub const FieldAssign = struct {
    object: *Ast,
    field: String,
    value: *Ast,
};

pub const Condition = struct {
    cond: *Ast,
    then_block: *Ast,
    else_block: ?*Ast,
};

pub const Loop = struct {
    cond: *Ast,
    body: *Ast,
};
