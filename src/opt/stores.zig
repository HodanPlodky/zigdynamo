const std = @import("std");
const ir = @import("ir.zig");

pub const Stores = struct {
    instructions: ir.InstructionDistinct.Multi = .{},
    basicblock: ir.BasicBlockDistinct.ArrayListUn = .{},
    function: ir.FunctionDistinct.ArrayListUn = .{},
    binop: ir.BinOpDistinct.Multi = .{},
    branch: ir.BranchDistinct.Multi = .{},
    set_local: ir.SetLocalDistinct.Multi = .{},
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

    pub fn get(self: *Stores, comptime T: type, index: Stores.get_index_type(T)) *T {
        const info = @typeInfo(Stores).@"struct";

        inline for (info.fields) |field| {
            if (field.type.Inner == T) {
                return @field(self, field.name).get_ptr(index);
            }
        }

        @compileError("could not find proper index");
    }
};
