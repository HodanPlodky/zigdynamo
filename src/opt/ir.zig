const std = @import("std");
const utils = @import("../utils.zig");

// Data Stores
pub const InstructionDistinct = utils.DistinctData(u32, Instruction);
pub const InstructionIdx = InstructionDistinct.Index;
pub const InstructionArray = InstructionDistinct.ArrayList;

pub const Reg = InstructionIdx;

pub const BasicBlockDistinct = utils.DistinctData(u32, BasicBlock);
pub const BasicBlockIdx = BasicBlockDistinct.Index;
pub const BasicBlockArray = BasicBlockDistinct.ArrayList;

pub const FunctionDistinct = utils.DistinctData(u32, Function);

pub const BinOpData = struct {
    left: Reg,
    right: Reg,
};
pub const BinOpDistinct = utils.DistinctData(u32, BinOpData);
pub const BinOpIdx = BinOpDistinct.Index;

pub const BranchData = struct {
    cond: Reg,
    true_branch: BasicBlockIdx,
    false_branch: BasicBlockIdx,
};
pub const BranchDistinct = utils.DistinctData(u32, BranchData);
pub const BranchIdx = BranchDistinct.Index;

pub const PhonyData = struct {
    const Pair = struct {
        label: BasicBlockIdx,
        reg: Reg,
    };
    data: []Pair,
};
pub const PhonyDistinct = utils.DistinctData(u32, BranchData);
pub const PhonyIdx = PhonyDistinct.Index;

// taged union with max payload
// of size 4 bytes (u32)
pub const Instruction = union(enum) {
    ldi: u32,
    mov: Reg,

    load_global: u32,
    store_global: u32,

    // ops
    add: BinOpIdx,
    sub: BinOpIdx,
    mul: BinOpIdx,
    div: BinOpIdx,

    // terminators
    ret: Reg,
    branch: BranchIdx,
    jmp: BasicBlockIdx,

    arg: u32,
    phony: PhonyIdx,

    pub fn opcode(self: Instruction) []const u8 {
        return switch (self) {
            .ldi => "ldi",
            .mov => "mov",
            .load_global => "load_global",
            .store_global => "store_global",
            .add => "add",
            .sub => "sub",
            .mul => "mul",
            .div => "div",
            .ret => "ret",
            .branch => "branch",
            .jmp => "jmp",
            .arg => "arg",
            .phony => "phony",
        };
    }
};

comptime {
    const type_info = @typeInfo(Instruction);
    for (type_info.@"union".fields) |field| {
        if (@sizeOf(field.type) > 4) {
            @compileError("payload is too big");
        }
    }
}

pub const BasicBlock = struct {
    instructions: std.ArrayListUnmanaged(InstructionIdx) = .{},
};

pub const Function = struct {
    entry: BasicBlockIdx,
    basicblocks: std.ArrayListUnmanaged(BasicBlockIdx),

    pub fn create(entry: BasicBlockIdx, alloc: std.mem.Allocator) !Function {
        var array = std.ArrayListUnmanaged(BasicBlockIdx){};
        try array.append(alloc, entry);
        return Function{
            .entry = entry,
            .basicblocks = array,
        };
    }
};
