const std = @import("std");
const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");
const I = bytecode.Instruction;

pub fn compile(program: ast.Program, alloc: std.mem.Allocator) !bytecode.Bytecode {
    var compiler = Compiler.init(alloc);
    return compiler.compile(program);
}

const CompilerFrame = struct {
    vars: std.ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator) CompilerFrame {
        return CompilerFrame{
            .vars = std.ArrayList([]const u8).init(alloc),
        };
    }

    pub fn get_index(self: *const CompilerFrame, var_name: []const u8) ?usize {
        for (self.vars.items, 0..) |item, index| {
            if (std.mem.eql(u8, item, var_name)) {
                return index;
            }
        }
        return null;
    }

    pub fn len(self: *const CompilerFrame) usize {
        return self.vars.items.len;
    }

    pub fn add_var(self: *CompilerFrame, var_name: []const u8) !void {
        if (self.get_index(var_name) != null) {
            @panic("non existant");
        }
        try self.vars.append(var_name);
    }
};

const Place = union(enum) {
    global: usize,
    local: usize,
};

const CompilerEnv = struct {
    frames: std.ArrayList(CompilerFrame),
    global: CompilerFrame,
    max_size: usize,
    current_size: usize,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) CompilerEnv {
        return CompilerEnv{
            .frames = std.ArrayList(CompilerFrame).init(alloc),
            .global = CompilerFrame.init(alloc),
            .max_size = 0,
            .current_size = 0,
            .alloc = alloc,
        };
    }

    pub fn get_place(self: *const CompilerEnv, var_name: []const u8) ?Place {
        var offset: usize = 0;
        for (self.frames) |frame| {
            if (frame.get_index(var_name)) |index| {
                return Place{ .local = offset + index };
            }
            offset += frame.len();
        }

        if (self.global.get_index(var_name)) |index| {
            return Place{ .global = index };
        }

        return null;
    }

    pub fn add_var(self: *CompilerEnv, var_name: []const u8) !void {
        var current = self.get_current();
        try current.add_var(var_name);
        self.current_size += 1;
        if (self.current_size > self.max_size) {
            self.max_size = self.current_size;
        }
    }

    pub fn add_global(self: *CompilerEnv, var_name: []const u8) !void {
        try self.global.add_var(var_name);
    }

    pub fn add_frame(self: *CompilerEnv, frame: CompilerFrame) !void {
        try self.frames.append(frame);
    }

    pub fn add_new_frame(self: *CompilerEnv) !void {
        self.add_frame(CompilerFrame.init(self.alloc));
    }

    pub fn pop_frame(self: *CompilerEnv) void {
        std.debug.assert(self.frames.len > 0);
        _ = self.frames.pop();
    }

    pub fn get_current(self: *const Compiler) *Compiler {
        std.debug.assert(self.frames.len > 0);
        return &self.frames.items[self.frames.items.len - 1];
    }
};

const Compiler = struct {
    alloc: std.mem.Allocator,
    constant_buffers: std.ArrayList(std.ArrayList(u8)),
    env: CompilerEnv,

    pub fn init(alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .alloc = alloc,
            .constant_buffers = std.ArrayList(std.ArrayList(u8)).init(alloc),
            .env = CompilerEnv.init(alloc),
        };
    }

    pub fn compile(self: *Compiler, program: ast.Program) !bytecode.Bytecode {
        // needs to be gather before the
        // run since the global env
        // behaves dynamically
        try self.gather_globals(program);
        const main_buffer = self.create_constant(bytecode.ConstantType.function);
        for (program.data, 0..) |expr, i| {
            switch (expr) {
                ast.Ast.let => unreachable,
                else => self.compile_expr(&expr, main_buffer),
            }
            if (program.data.len - 1 > i) {
                Compiler.add_inst(main_buffer, I.pop);
            }
        }
        Compiler.add_inst(main_buffer, I.ret);
        Compiler.patch_len(main_buffer);

        var constants = try self.alloc.alloc(bytecode.Constant, self.constant_buffers.items.len);

        for (self.constant_buffers.items, 0..) |item, index| {
            constants[index] = bytecode.Constant.new(@ptrCast(item.items));
        }

        const res = bytecode.Bytecode{
            .constants = constants,
            .current = constants[0],
        };
        return res;
    }

    pub fn compile_fn_body(self: *Compiler, exprs: []ast.Ast) void {
        _ = exprs; // autofix
        _ = self; // autofix

        unreachable;
    }

    pub fn compile_expr(self: *Compiler, expr: *const ast.Ast, buffer: *std.ArrayList(u8)) void {
        switch (expr.*) {
            ast.Ast.number => |num| {
                Compiler.add_inst(buffer, I.push);
                Compiler.add_u32(buffer, num);
            },
            ast.Ast.nil => {
                Compiler.add_inst(buffer, I.nil);
            },
            ast.Ast.binop => |binop| {
                self.compile_expr(binop.left, buffer);
                self.compile_expr(binop.right, buffer);
                switch (binop.op) {
                    '+' => Compiler.add_inst(buffer, I.add),
                    '-' => Compiler.add_inst(buffer, I.sub),
                    '*' => Compiler.add_inst(buffer, I.mul),
                    '/' => Compiler.add_inst(buffer, I.div),
                    '<' => Compiler.add_inst(buffer, I.lt),
                    '>' => Compiler.add_inst(buffer, I.gt),
                    'e' => Compiler.add_inst(buffer, I.eq),
                    'n' => Compiler.add_inst(buffer, I.ne),
                    else => unreachable,
                }
            },
            else => @panic("unimplemented"),
        }
    }

    pub fn add_inst(buffer: *std.ArrayList(u8), inst: bytecode.Instruction) void {
        buffer.append(@intFromEnum(inst)) catch unreachable;
    }

    pub fn add_u32(buffer: *std.ArrayList(u8), value: u32) void {
        buffer.append(@intCast((value >> 24) & 0xff)) catch unreachable;
        buffer.append(@intCast((value >> 16) & 0xff)) catch unreachable;
        buffer.append(@intCast((value >> 8) & 0xff)) catch unreachable;
        buffer.append(@intCast(value & 0xff)) catch unreachable;
    }

    pub fn create_constant(self: *Compiler, const_type: bytecode.ConstantType) *std.ArrayList(u8) {
        self.constant_buffers.append(std.ArrayList(u8).init(self.alloc)) catch unreachable;
        var res = &self.constant_buffers.items[self.constant_buffers.items.len - 1];
        // pad length
        Compiler.add_u32(res, 0);
        res.append(@intFromEnum(const_type)) catch unreachable;
        return res;
    }

    pub fn patch_len(buffer: *std.ArrayList(u8)) void {
        const len: u32 = @intCast(buffer.items.len - 4);
        buffer.items[0] = @intCast((len >> 24) & 0xff);
        buffer.items[1] = @intCast((len >> 16) & 0xff);
        buffer.items[2] = @intCast((len >> 8) & 0xff);
        buffer.items[3] = @intCast(len & 0xff);
    }

    pub fn gather_globals(self: *Compiler, program: ast.Program) !void {
        for (program.data) |item| {
            switch (item) {
                ast.Ast.let => |let| try self.env.add_global(let.target),
                else => {},
            }
        }
    }
};

const ohsnap = @import("ohsnap");
test "basic compiler" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new("1 +   2 * 2 - 3;", allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\bytecode.Bytecode{ .constants = { { 0, 0, 0, 25, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 14, 12, 0, 0, 0, 0, 3, 13, 23 } }, .current = { 0, 0, 0, 25, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 14, 12, 0, 0, 0, 0, 3, 13, 23 } }
    ).expectEqualFmt(res);
}
