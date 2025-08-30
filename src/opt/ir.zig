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
pub const FunctionIdx = FunctionDistinct.Index;

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
    pub const Pair = struct {
        label: BasicBlockIdx,
        reg: Reg,
    };
    data: []Pair,

    // which original variable was
    // this phony, it could be the case
    // that this phony is create not from
    // the local but from different
    // branching like conditional
    origin: ?u32 = null,
};
pub const PhonyDistinct = utils.DistinctData(u32, PhonyData);
pub const PhonyIdx = PhonyDistinct.Index;

pub const SetLocalData = struct {
    local_idx: u32,
    value: Reg,

    // this will help with the make ssa pass
    basicblock_idx: BasicBlockIdx,
};
pub const SetLocalDistinct = utils.DistinctData(u32, SetLocalData);
pub const SetLocalIdx = SetLocalDistinct.Index;

pub const StoreData = struct {
    idx: u32,
    value: Reg,
};
pub const StoreDataDistinct = utils.DistinctData(u32, StoreData);
pub const StoreDataIdx = StoreDataDistinct.Index;

// taged union with max payload
// of size 4 bytes (u32)
pub const Instruction = union(enum) {
    ldi: u32,
    mov: Reg,
    nil,
    true,
    false,

    load_global: u32,
    store_global: StoreDataIdx,
    load_env: u32,
    store_env: StoreDataIdx,

    // ops
    add: BinOpIdx,
    sub: BinOpIdx,
    mul: BinOpIdx,
    div: BinOpIdx,
    lt: BinOpIdx,
    gt: BinOpIdx,

    // terminators
    ret: Reg,
    branch: BranchIdx,
    jmp: BasicBlockIdx,

    arg: u32,
    phony: PhonyIdx,
    nop,

    // this instruction should be removed
    // before jit
    get_local: u32,
    set_local: SetLocalIdx,

    pub fn opcode(self: Instruction) []const u8 {
        return switch (self) {
            .ldi => "ldi",
            .mov => "mov",
            .nil => "nil",
            .true => "true",
            .false => "false",
            .load_global => "load_global",
            .store_global => "store_global",
            .load_env => "load_env",
            .store_env => "store_env",
            .add => "add",
            .sub => "sub",
            .mul => "mul",
            .div => "div",
            .lt => "lt",
            .gt => "gt",
            .ret => "ret",
            .branch => "branch",
            .jmp => "jmp",
            .arg => "arg",
            .nop => "nop",
            .phony => "phony",
            .get_local => "get_local",
            .set_local => "set_local",
        };
    }

    pub fn is_terminator(self: Instruction) bool {
        return switch (self) {
            .ret, .branch, .jmp => true,
            else => false,
        };
    }

    pub const LabelIterator = struct {
        // max number of labels in terminator
        data: [2]BasicBlockIdx,
        len: u8,
        curr: u8 = 0,

        pub fn create_zero() LabelIterator {
            return LabelIterator{
                .data = undefined,
                .len = 0,
            };
        }

        pub fn create_one(bb_idx: BasicBlockIdx) LabelIterator {
            return LabelIterator{
                .data = .{ bb_idx, undefined },
                .len = 1,
            };
        }

        pub fn create_two(a_idx: BasicBlockIdx, b_idx: BasicBlockIdx) LabelIterator {
            return LabelIterator{
                .data = .{ a_idx, b_idx },
                .len = 2,
            };
        }

        pub fn next(self: *LabelIterator) ?BasicBlockIdx {
            std.debug.assert(self.len <= 2);
            if (self.curr >= self.len) {
                return null;
            }
            const res = self.data[self.curr];
            self.curr += 1;
            return res;
        }
    };
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
    predecessors: std.ArrayListUnmanaged(BasicBlockIdx) = .{},
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

pub const Type = enum {
    Top,
    Bottom,
    Int,
    Nil,
    True,
    False,
    Void,
};
