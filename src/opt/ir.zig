const std = @import("std");

pub const Node = struct {
    inputs: std.ArrayList(*Node),
    node_type: NodeType,
    data_idx: u32,
};

pub const NodeType = enum(u8) {
    // ctrl
    start,
    ret,

    // pure
    number,
    add,
    sub,
    mul,
    div,
};
