const std = @import("std");
const utils = @import("../utils.zig");

pub const NodeDisctinct = utils.DistinctData(u32, Node);
pub const NodeIdx = NodeDisctinct.Index;
pub const NodeArray = NodeDisctinct.ArrayList;

const NONE_NODE: NodeIdx = NodeIdx.new(0);

pub const DataIdx = utils.DistinctIndex(u32);


pub const Node = struct {
    cfg_parent: NodeIdx,
    inputs: std.ArrayList(NodeIdx),
    outputs: std.ArrayList(NodeIdx),
    node_type: NodeType,
    data_idx: DataIdx,

    pub fn get_type() DataType {
        return DataType.top;
    }
};

pub const Function = struct {
    start: NodeIdx,
};

pub const CompiledResult = struct {
    nodes: NodeArray,
    functions: []Function,
};

pub const NodeType = enum(u8) {
    // ctrl
    start,
    ret,

    // meta
    none,
    multi_node,
    projection_node,

    // pure
    number,
    constant,
    add,
    sub,
    mul,
    div,
};

pub const DataType = enum(u8) {
    // meta for lattice
    top,
    bottom,

    number,
    string,
    object,
};
