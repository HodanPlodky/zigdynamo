const std = @import("std");
const utils = @import("../utils.zig");
const Builtin = @import("../builtins.zig").Builtin;

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

pub const CallData = struct {
    target: Reg,
    args: []Reg,
};
pub const CallDataDistinct = utils.DistinctData(u32, CallData);
pub const CallDataIdx = CallDataDistinct.Index;

pub const BuiltinData = struct {
    builtin: Builtin,
    args: []Reg,
};
pub const BuiltinDataDistinct = utils.DistinctData(u32, BuiltinData);
pub const BuiltinDataIdx = BuiltinDataDistinct.Index;

pub const ParallelCopy = struct {
    reg: Reg,

    /// from which phony inst it originates
    origin: Reg,
};
pub const ParallelCopyDistinct = utils.DistinctData(u32, CopyData);
pub const ParallelCopyIdx = ParallelCopyDistinct.Index;

pub const CopyData = struct {
    src: Reg,
    dst: Reg,
};
pub const CopyDataDistinct = utils.DistinctData(u32, CopyData);
pub const CopyIdx = CopyDataDistinct.Index;

pub const Closure = struct {
    function_idx: u32,
    env: []Reg,
};
pub const ClosureDistinct = utils.DistinctData(u32, Closure);
pub const ClosureIdx = ClosureDistinct.Index;

pub const Object = struct {
    class_idx: u32,
    proto: Reg,
    fields: []Reg,
};
pub const ObjectDistinct = utils.DistinctData(u32, Object);
pub const ObjectIdx = ObjectDistinct.Index;

pub const GetField = struct {
    object: Reg,
    // string idx
    field: u32,
};
pub const GetFieldDistinct = utils.DistinctData(u32, GetField);
pub const GetFieldIdx = GetFieldDistinct.Index;

pub const SetField = struct {
    object: Reg,
    value: Reg,

    // string idx
    field: u32,
};
pub const SetFieldDistinct = utils.DistinctData(u32, SetField);
pub const SetFieldIdx = SetFieldDistinct.Index;

pub const MethodCall = struct {
    object: Reg,
    args: []Reg,

    // string idx
    field: u32,
};
pub const MethodCallDistinct = utils.DistinctData(u32, MethodCall);
pub const MethodCallIdx = MethodCallDistinct.Index;

// taged union with max payload
// of size 4 bytes (u32)
pub const Instruction = union(enum) {
    ldi: u32,
    mov: Reg,
    nil,
    true,
    false,
    string: u32,
    closure: ClosureIdx,
    object: ObjectIdx,
    // forces constant to be in register
    // this is usefull when moving out of ssa
    // to use correct reg alloc and also
    // allow to handle constant as constants
    // if they dont have to be handled as assings
    // to mutable reg/mem place
    regify: Reg,

    load_global: u32,
    store_global: StoreDataIdx,
    load_env: u32,
    store_env: StoreDataIdx,

    // object manipulation
    get_field: GetFieldIdx,
    set_field: SetFieldIdx,

    // ops
    add: BinOpIdx,
    sub: BinOpIdx,
    mul: BinOpIdx,
    div: BinOpIdx,
    lt: BinOpIdx,
    gt: BinOpIdx,
    eq: BinOpIdx,
    ne: BinOpIdx,

    // terminators
    ret: Reg,
    branch: BranchIdx,
    jmp: BasicBlockIdx,

    arg: u32,
    phony: PhonyIdx,
    parallel_copy: Reg,
    nop,

    // calls
    call: CallDataIdx,
    method_call: MethodCallIdx,
    builtin: BuiltinDataIdx,

    // this instruction should be removed
    // before jit
    get_local: u32,
    set_local: SetLocalIdx,

    // not ssa instruction
    // should be inserted only
    // at the end of the compilation
    copy: CopyIdx,

    pub fn opcode(self: Instruction) []const u8 {
        return switch (self) {
            .ldi => "ldi",
            .mov => "mov",
            .nil => "nil",
            .true => "true",
            .false => "false",
            .string => "string",
            .object => "object",
            .closure => "closure",
            .regify => "regify",
            .load_global => "load_global",
            .store_global => "store_global",
            .load_env => "load_env",
            .store_env => "store_env",
            .get_field => "get_field",
            .set_field => "set_field",
            .add => "add",
            .sub => "sub",
            .mul => "mul",
            .div => "div",
            .lt => "lt",
            .gt => "gt",
            .eq => "eq",
            .ne => "ne",
            .ret => "ret",
            .branch => "branch",
            .jmp => "jmp",
            .arg => "arg",
            .nop => "nop",
            .phony => "phony",
            .parallel_copy => "parallel_copy",
            .call => "call",
            .method_call => "method_call",
            .builtin => "print",
            .get_local => "get_local",
            .set_local => "set_local",
            .copy => "copy",
        };
    }

    pub fn is_terminator(self: Instruction) bool {
        return switch (self) {
            .ret, .branch, .jmp => true,
            else => false,
        };
    }

    pub fn is_constant(self: Instruction) bool {
        return switch (self) {
            .ldi, .nil, .true, .false, .string => true,
            else => false,
        };
    }

    pub fn possible_sideeffect(self: Instruction) bool {
        if (self.is_terminator()) {
            return true;
        }
        return switch (self) {
            .store_env,
            .store_global,
            .set_local,
            .call,
            .method_call,
            .builtin,
            .set_field,
            => true,
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

    pub fn has_output(self: Instruction) bool {
        if (self.is_terminator()) {
            return false;
        }
        return switch (self) {
            .store_global, .nop, .set_local, .store_env => false,
            else => true,
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
    predecessors: std.ArrayListUnmanaged(BasicBlockIdx) = .{},
    instructions: std.ArrayListUnmanaged(InstructionIdx) = .{},
};

pub const Function = struct {
    entry: BasicBlockIdx,
    basicblocks: std.ArrayListUnmanaged(BasicBlockIdx),
    env_start: u32,

    pub fn create(entry: BasicBlockIdx, env_start: u32, alloc: std.mem.Allocator) !Function {
        var array = std.ArrayListUnmanaged(BasicBlockIdx){};
        try array.append(alloc, entry);
        return Function{
            .entry = entry,
            .basicblocks = array,
            .env_start = env_start,
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
