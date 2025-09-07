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
    call: ir.CallDataDistinct.Multi = .{},
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

    pub fn get_field_elem_type(comptime T: type) type {
        const info = @typeInfo(Self).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return field.type.MultiArr.Field;
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get_field_ptr(
        self: *Stores,
        comptime T: type,
        comptime R: type,
        comptime elem_field: Stores.get_field_elem_type(T),
        index: Stores.get_index_type(T),
    ) *R {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self, field.name).get_field_ptr(R, elem_field, index);
            }
        }

        @compileError("could not find proper index");
    }

    pub fn get_field_reg_ptr(
        self: *Stores,
        comptime T: type,
        comptime elem_field: Stores.get_field_elem_type(T),
        index: Stores.get_index_type(T),
    ) *ir.Reg {
        return self.get_field_ptr(T, ir.Reg, elem_field, index);
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

    pub fn get_type(self: *const Stores, inst: ir.Instruction) ir.Type {
        return switch (inst) {
            .ldi => ir.Type.Int,
            .mov => |reg| {
                const src_inst = self.get(ir.Instruction, reg);
                return self.get_type(src_inst);
            },
            .nil => ir.Type.Nil,
            .true => ir.Type.True,
            .false => ir.Type.False,
            .load_global => ir.Type.Top,
            .store_global => ir.Type.Void,
            .load_env => ir.Type.Top,
            .store_env => ir.Type.Void,
            .add, .sub, .mul, .div, .lt, .gt => ir.Type.Top,
            .ret, .branch, .jmp => ir.Type.Void,
            .arg => ir.Type.Top,
            .nop => ir.Type.Void,
            // TODO use join
            .phony => ir.Type.Top,
            .call => ir.Type.Top,
            .get_local => ir.Type.Top,
            .set_local => ir.Type.Void,
        };
    }

    fn IdxIter(comptime Index: type) type {
        return struct {
            const IterType = @This();

            curr: usize,
            count: usize,

            pub fn init(count: Index) IterType {
                return IterType{
                    .curr = 0,
                    .count = count.get_usize(),
                };
            }

            pub fn next(self: *IterType) ?Index {
                if (self.curr >= self.count) {
                    return null;
                }

                const res = self.curr;
                self.curr += 1;
                return Index.new(@intCast(res));
            }
        };
    }

    pub fn idx_iter(self: *const Stores, comptime T: type) IdxIter(Stores.get_index_type(T)) {
        const count = self.get_max_idx(T);
        return IdxIter(Stores.get_index_type(T)).init(count);
    }

    const RegIter = struct {
        const OtherData = struct {
            // max number of the regs in non phony
            regs: [2]ir.Reg = undefined,
            len: u8,
        };

        const IterTypes = union(enum) {
            phony_iter: []ir.PhonyData.Pair,
            call_iter: ir.CallData,
            other: OtherData,
        };

        data: IterTypes,
        current: usize = 0,

        fn create_phony(phony_data: ir.PhonyData) RegIter {
            return RegIter{
                .data = .{ .phony_iter = phony_data.data },
            };
        }

        fn create_call(calldata: ir.CallData) RegIter {
            return RegIter{
                .data = .{ .call_iter = calldata },
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
                .call_iter => |calldata| calldata.args.len + 1,
                .other => |data| @intCast(data.len),
            };
        }

        pub fn next(self: *RegIter) ?ir.Reg {
            if (self.current >= self.get_len()) {
                return null;
            }
            const res = switch (self.data) {
                .phony_iter => |pairs| pairs[self.current].reg,
                .call_iter => |calldata| if (self.current == 0)
                    calldata.target
                else
                    calldata.args[self.current - 1],
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
            .call => |call_idx| {
                const data = self.get(ir.CallData, call_idx);
                return RegIter.create_call(data);
            },
            .get_local => return RegIter.create_empty(),
            .set_local => |set_local_idx| {
                const set_local = self.get(ir.SetLocalData, set_local_idx);
                return RegIter.create_one(set_local.value);
            },
        }
    }

    const RegIterPtr = struct {
        const OtherData = struct {
            // max number of the regs in non phony
            regs: [2]*ir.Reg = undefined,
            len: u8,
        };

        const CallData = struct {
            target: *ir.Reg,
            args: []ir.Reg,
        };

        const IterTypes = union(enum) {
            phony_iter: []ir.PhonyData.Pair,
            call_iter: CallData,
            other: OtherData,
        };

        data: IterTypes,
        current: usize = 0,

        fn create_phony(phony_data: ir.PhonyData) RegIterPtr {
            return RegIterPtr{
                .data = .{ .phony_iter = phony_data.data },
            };
        }

        fn create_call(target: *ir.Reg, args: []ir.Reg) RegIterPtr {
            return RegIterPtr{
                .data = .{ .call_iter = .{
                    .target = target,
                    .args = args,
                } },
            };
        }

        fn create_empty() RegIterPtr {
            return RegIterPtr{
                .data = .{ .other = .{ .len = 0 } },
            };
        }

        fn create_one(reg: *ir.Reg) RegIterPtr {
            var data: OtherData = .{ .len = 1 };
            data.regs[0] = reg;
            return RegIterPtr{ .data = .{ .other = data } };
        }

        fn create_two(a_reg: *ir.Reg, b_reg: *ir.Reg) RegIterPtr {
            var data: OtherData = .{ .len = 2 };
            data.regs[0] = a_reg;
            data.regs[1] = b_reg;
            return RegIterPtr{ .data = .{ .other = data } };
        }

        fn get_len(self: *const RegIterPtr) usize {
            return switch (self.data) {
                .phony_iter => |pairs| pairs.len,
                .call_iter => |calldata| calldata.args.len + 1,
                .other => |data| @intCast(data.len),
            };
        }

        pub fn next(self: *RegIterPtr) ?*ir.Reg {
            if (self.current >= self.get_len()) {
                return null;
            }
            const res = switch (self.data) {
                .phony_iter => |pairs| &pairs[self.current].reg,
                .call_iter => |calldata| if (self.current == 0)
                    calldata.target
                else
                    &calldata.args[self.current - 1],
                .other => |data| data.regs[self.current],
            };
            self.current += 1;
            return res;
        }
    };

    pub fn get_reg_iter_ptr(self: *Stores, inst_idx: ir.InstructionIdx) RegIterPtr {
        const inst = self.get(ir.Instruction, inst_idx);

        switch (inst) {
            // no regs
            .ldi,
            .load_global,
            .arg,
            .load_env,
            .nil,
            .true,
            .false,
            .nop,
            .jmp,
            => return RegIterPtr.create_empty(),

            // TODO
            .store_env, .store_global => |store_idx| {
                const data = self.get_field_reg_ptr(ir.StoreData, .value, store_idx);
                return RegIterPtr.create_one(data);
            },

            // one reg ops
            .ret => {
                const reg = &self.instructions.data.items(.data)[inst_idx.get_usize()].ret;
                return RegIterPtr.create_one(reg);
            },
            .mov => {
                const reg = &self.instructions.data.items(.data)[inst_idx.get_usize()].mov;
                return RegIterPtr.create_one(reg);
            },

            //  binop ops
            .add, .sub, .mul, .div, .lt, .gt => |binop_idx| {
                const left = self.get_field_reg_ptr(ir.BinOpData, .left, binop_idx);
                const right = self.get_field_reg_ptr(ir.BinOpData, .right, binop_idx);
                return RegIterPtr.create_two(left, right);
            },
            .branch => |branch_idx| {
                const cond = self.get_field_reg_ptr(ir.BranchData, .cond, branch_idx);
                return RegIterPtr.create_one(cond);
            },
            .phony => |phony_idx| {
                const data = self.get(ir.PhonyData, phony_idx);
                return RegIterPtr.create_phony(data);
            },
            .call => |call_idx| {
                const data = self.get(ir.CallData, call_idx);
                const target = self.get_field_reg_ptr(ir.CallData, .target, call_idx);
                return RegIterPtr.create_call(target, data.args);
            },
            .get_local => return RegIterPtr.create_empty(),
            .set_local => |set_local_idx| {
                const value = self.get_field_reg_ptr(ir.SetLocalData, .value, set_local_idx);
                return RegIterPtr.create_one(value);
            },
        }
    }
};
