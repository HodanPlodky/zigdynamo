pub const Program = struct {
    data: []Ast,
};

pub const Ast = union(enum) {
    number: u32,
    string: []const u8,
    bool: bool,
    nil,
    ident: []const u8,
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
    params: [][]const u8,
    body: *Ast,
};

pub const Assign = struct {
    target: []const u8,
    value: *Ast,
};

pub const Let = struct {
    target: []const u8,
    value: *Ast,
};

pub const Field = struct {
    name: []const u8,
    value: *Ast,
};

pub const Object = struct {
    prototype: ?*Ast,
    fields: []Field,
};

pub const FieldAccess = struct {
    target: *Ast,
    field: []const u8,
};

pub const FieldCall = struct {
    target: *Ast,
    field: []const u8,
    args: []Ast,
};

pub const FieldAssign = struct {
    object: *Ast,
    field: []const u8,
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
