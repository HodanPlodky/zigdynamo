const std = @import("std");
const Compiler = @import("compile.zig").Compiler;
const ir = @import("ir.zig");

pub const MakeSSA = struct {
    compiler: *Compiler,
    alloc: std.mem.Allocator,

    pub fn init(compiler: *Compiler, alloc: std.mem.Allocator) MakeSSA {
        return MakeSSA{
            .compiler = compiler,
            .alloc = alloc,
        };
    }

    pub fn run(self: *MakeSSA) !void {
        _ = self;
        //_ = try self.compiler.append_inst(ir.Instruction.nil);
    }
};
