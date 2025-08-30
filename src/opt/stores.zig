const std = @import("std");
const ir = @import("ir.zig");

/// Structure that has the ownership of all
/// data in the compiled code
pub const Stores = struct {
    instructions: ir.InstructionDistinct.Multi = .{},
    basicblock: ir.BasicBlockDistinct.ArrayListUn = .{},
    function: ir.FunctionDistinct.ArrayListUn = .{},
    binop: ir.BinOpDistinct.Multi = .{},
    branch: ir.BranchDistinct.Multi = .{},
    set_local: ir.SetLocalDistinct.Multi = .{},
    store_data: ir.StoreDataDistinct.Multi = .{},
    phony: ir.PhonyDistinct.Multi = .{},
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn get_index_type(comptime T: type) type {
        const info = @typeInfo(Self).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return field.type.DistIndex;
            }
        }

        @compileError("could not find proper index");
    }

    pub fn create(self: *Stores, comptime T: type) !Stores.get_index_type(T) {
        return self.create_with(T, T{});
    }

    pub fn create_with(self: *Stores, comptime T: type, value: T) !Stores.get_index_type(T) {
        const info = @typeInfo(Stores).@"struct";
        const Index = Stores.get_index_type(T);

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                const index = @field(self, field.name).len();
                try @field(self, field.name).data.append(self.alloc, value);
                return Index.new(@intCast(index));
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get_ptr(self: *Stores, comptime T: type, index: Stores.get_index_type(T)) *T {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self, field.name).get_ptr(index);
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get_const_ptr(self: *const Stores, comptime T: type, index: Stores.get_index_type(T)) *const T {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self, field.name).get_ptr_const(index);
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get(self: *const Stores, comptime T: type, index: Stores.get_index_type(T)) T {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self, field.name).get(index);
            }
        }

        @compileError("could not find proper index");
    }

    pub fn set(self: *Stores, comptime T: type, index: Stores.get_index_type(T), value: T) void {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self, field.name).set(index, value);
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get_max_idx(self: *const Stores, comptime T: type) Stores.get_index_type(T) {
        const info = @typeInfo(Stores).@"struct";
        const Index = Stores.get_index_type(T);

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                const index = @field(self, field.name).len();
                return Index.new(@intCast(index));
            }
        }

        @compileError("could not find proper index");
    }

    const RegIter = struct {
        const OtherData = struct {
            // max number of the regs in non phony
            regs: [2]ir.Reg = undefined,
            len: u8,
        };

        const IterTypes = union(enum) {
            phony_iter: []ir.PhonyData.Pair,
            other: OtherData,
        };

        data: IterTypes,
        current: usize = 0,

        fn create_phony(phony_data: ir.PhonyData) RegIter {
            return RegIter{
                .data = .{ .phony_iter = phony_data.data },
            };
        }

        fn create_empty() RegIter {
            return RegIter{
                .data = .{ .other = .{ .len = 0 } },
            };
        }

        fn create_one(reg: ir.Reg) RegIter {
            var data: OtherData = .{ .len = 1 };
            data.regs[0] = reg;
            return RegIter{ .data = .{ .other = data } };
        }

        fn create_two(a_reg: ir.Reg, b_reg: ir.Reg) RegIter {
            var data: OtherData = .{ .len = 2 };
            data.regs[0] = a_reg;
            data.regs[1] = b_reg;
            return RegIter{ .data = .{ .other = data } };
        }

        fn get_len(self: *const RegIter) usize {
            return switch (self.data) {
                .phony_iter => |pairs| pairs.len,
                .other => |data| @intCast(data.len),
            };
        }

        pub fn next(self: *RegIter) ?ir.Reg {
            if (self.current >= self.get_len()) {
                return null;
            }
            const res = switch (self.data) {
                .phony_iter => |pairs| pairs[self.current].reg,
                .other => |data| data.regs[self.current],
            };
            self.current += 1;
            return res;
        }
    };

    pub fn get_reg_iter(self: *const Stores, inst_idx: ir.InstructionIdx) RegIter {
        const inst = self.get(ir.Instruction, inst_idx);

        switch (inst) {
            // no regs
            .ldi, .load_global, .arg, .load_env, .nil, .true, .false, .nop, .jmp => return RegIter.create_empty(),

            // TODO
            .store_env, .store_global => |store_idx| {
                const data = self.get(ir.StoreData, store_idx);
                return RegIter.create_one(data.value);
            },

            // one reg ops
            .ret, .mov => |reg| return RegIter.create_one(reg),

            //  binop ops
            .add, .sub, .mul, .div, .lt, .gt => |binop_idx| {
                const binop = self.get(ir.BinOpData, binop_idx);
                return RegIter.create_two(binop.left, binop.right);
            },
            .branch => |branch_idx| {
                const branch = self.get(ir.BranchData, branch_idx);
                return RegIter.create_one(branch.cond);
            },
            .phony => |phony_idx| {
                const data = self.get(ir.PhonyData, phony_idx);
                return RegIter.create_phony(data);
            },
            .get_local => return RegIter.create_empty(),
            .set_local => |set_local_idx| {
                const set_local = self.get(ir.SetLocalData, set_local_idx);
                return RegIter.create_one(set_local.value);
            },
        }
    }
};
