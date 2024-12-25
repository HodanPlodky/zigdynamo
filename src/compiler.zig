const std = @import("std");
const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");
const I = bytecode.Instruction;

pub fn compile(program: ast.Program, alloc: std.mem.Allocator) !bytecode.Bytecode {
    var scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer scratch_arena.deinit();
    const scratch_allocator = scratch_arena.allocator();
    var compiler = Compiler.init(alloc, scratch_allocator);
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
    global: u32,
    local: u32,

    pub fn get_index(self: Place) u32 {
        return switch (self) {
            Place.global => |num| num,
            Place.local => |num| num,
        };
    }
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

    pub fn get_global_count(self: *const CompilerEnv) usize {
        return self.global.len();
    }

    pub fn get_place(self: *const CompilerEnv, var_name: []const u8) ?Place {
        var offset: usize = 0;
        for (self.frames.items) |frame| {
            if (frame.get_index(var_name)) |index| {
                return Place{ .local = @intCast(offset + index) };
            }
            offset += frame.len();
        }

        if (self.global.get_index(var_name)) |index| {
            return Place{ .global = @intCast(index) };
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

const Label = struct {
    idx: usize,
};

const LabelData = struct {
    position: u32,
    uses: std.ArrayList(u32),

    pub fn init(alloc: std.mem.Allocator) LabelData {
        return LabelData{
            .position = 0,
            .uses = std.ArrayList(u32).init(alloc),
        };
    }

    pub fn add_use(self: *LabelData, pos: u32) void {
        self.uses.append(pos) catch unreachable;
    }
};

const ConstantBuffer = struct {
    buffer: std.ArrayList(u8),
    labels: std.ArrayList(LabelData),
    label_alloc: std.mem.Allocator,

    pub fn init(buffer_alloc: std.mem.Allocator, label_alloc: std.mem.Allocator) ConstantBuffer {
        return ConstantBuffer{
            .buffer = std.ArrayList(u8).init(buffer_alloc),
            .labels = std.ArrayList(LabelData).init(label_alloc),
            .label_alloc = label_alloc,
        };
    }

    pub fn add_inst(self: *ConstantBuffer, inst: bytecode.Instruction) void {
        self.buffer.append(@intFromEnum(inst)) catch unreachable;
    }

    pub fn add_u32(self: *ConstantBuffer, value: u32) void {
        self.buffer.append(@intCast((value >> 24) & 0xff)) catch unreachable;
        self.buffer.append(@intCast((value >> 16) & 0xff)) catch unreachable;
        self.buffer.append(@intCast((value >> 8) & 0xff)) catch unreachable;
        self.buffer.append(@intCast(value & 0xff)) catch unreachable;
    }

    pub fn set_u32(self: *const ConstantBuffer, idx: u32, value: u32) void {
        self.buffer.items[idx] = @intCast((value >> 24) & 0xff);
        self.buffer.items[idx + 1] = @intCast((value >> 16) & 0xff);
        self.buffer.items[idx + 2] = @intCast((value >> 8) & 0xff);
        self.buffer.items[idx + 3] = @intCast(value & 0xff);
    }

    fn create_label(self: *ConstantBuffer) Label {
        const idx = self.labels.items.len;
        self.labels.append(LabelData.init(self.label_alloc)) catch unreachable;
        return Label{ .idx = idx };
    }

    fn set_label_position(self: *ConstantBuffer, label: Label) void {
        const pos = self.buffer.items.len;
        self.labels.items[label.idx].position = @intCast(pos);
    }

    fn add_label_use(self: *ConstantBuffer, label: Label) void {
        const pos = self.buffer.items.len;
        self.labels.items[label.idx].add_use(@intCast(pos));
        // dummy label value
        self.add_u32(0xfefefefe);
    }

    pub fn patch_len(self: *const ConstantBuffer) void {
        const len: u32 = @intCast(self.buffer.items.len - 4);
        self.set_u32(0, len);
    }

    pub fn patch_labels(self: *const ConstantBuffer) void {
        for (self.labels.items) |label| {
            for (label.uses.items) |use| {
                self.set_u32(use, label.position);
            }
        }
    }
};

const Compiler = struct {
    pernament_alloc: std.mem.Allocator,
    scratch_alloc: std.mem.Allocator,
    constant_buffers: std.ArrayList(ConstantBuffer),
    env: CompilerEnv,

    pub fn init(pernament_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .pernament_alloc = pernament_alloc,
            .scratch_alloc = scratch_alloc,
            .constant_buffers = std.ArrayList(ConstantBuffer).init(pernament_alloc),
            .env = CompilerEnv.init(pernament_alloc),
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
                ast.Ast.let => |let| {
                    self.compile_expr(main_buffer, let.value);
                    main_buffer.add_inst(I.set_global);
                    const target_place = self.env.get_place(let.target).?;
                    std.debug.assert(std.meta.activeTag(target_place) == Place.global);
                    const target_idx = target_place.get_index();
                    main_buffer.add_u32(target_idx);
                },
                else => self.compile_expr(main_buffer, &expr),
            }
            if (program.data.len - 1 > i) {
                main_buffer.add_inst(I.pop);
            }
        }
        main_buffer.add_inst(I.ret);

        for (self.constant_buffers.items) |*buffer| {
            buffer.patch_len();
            buffer.patch_labels();
        }

        var constants = try self.pernament_alloc.alloc(bytecode.Constant, self.constant_buffers.items.len);

        for (self.constant_buffers.items, 0..) |item, index| {
            constants[index] = bytecode.Constant.new(@ptrCast(item.buffer.items));
        }

        const res = bytecode.Bytecode{
            .constants = constants,
            .current = constants[0],
            .global_count = self.env.get_global_count(),
        };
        return res;
    }

    pub fn compile_fn_body(self: *Compiler, exprs: []ast.Ast) void {
        _ = exprs; // autofix
        _ = self; // autofix

        unreachable;
    }

    pub fn compile_expr(self: *Compiler, buffer: *ConstantBuffer, expr: *const ast.Ast) void {
        switch (expr.*) {
            ast.Ast.number => |num| {
                buffer.add_inst(I.push);
                buffer.add_u32(num);
            },
            ast.Ast.nil => {
                buffer.add_inst(I.nil);
            },
            ast.Ast.bool => |val| {
                if (val) {
                    buffer.add_inst(I.true);
                } else {
                    buffer.add_inst(I.false);
                }
            },
            ast.Ast.binop => |binop| {
                self.compile_expr(buffer, binop.left);
                self.compile_expr(buffer, binop.right);
                switch (binop.op) {
                    '+' => buffer.add_inst(I.add),
                    '-' => buffer.add_inst(I.sub),
                    '*' => buffer.add_inst(I.mul),
                    '/' => buffer.add_inst(I.div),
                    '<' => buffer.add_inst(I.lt),
                    '>' => buffer.add_inst(I.gt),
                    'e' => buffer.add_inst(I.eq),
                    'n' => buffer.add_inst(I.ne),
                    else => unreachable,
                }
            },
            ast.Ast.ident => |ident| {
                const place = self.env.get_place(ident).?;
                switch (place) {
                    Place.local => buffer.add_inst(I.get),
                    Place.global => buffer.add_inst(I.get_global),
                }
                const idx = place.get_index();
                buffer.add_u32(idx);
            },
            ast.Ast.condition => |condition| {
                self.compile_expr(buffer, condition.cond);
                buffer.add_inst(I.branch);
                const then_label = buffer.create_label();
                const after_label = buffer.create_label();
                buffer.add_label_use(then_label);
                if (condition.else_block) |else_block| {
                    self.compile_expr(buffer, else_block);
                } else {
                    buffer.add_inst(I.nil);
                }
                buffer.add_inst(I.jump);
                buffer.add_label_use(after_label);
                buffer.set_label_position(then_label);
                self.compile_expr(buffer, condition.then_block);
                buffer.set_label_position(after_label);
            },
            else => @panic("unimplemented"),
        }
    }

    pub fn create_constant(self: *Compiler, const_type: bytecode.ConstantType) *ConstantBuffer {
        const constant_buffer = ConstantBuffer.init(self.pernament_alloc, self.scratch_alloc);
        self.constant_buffers.append(constant_buffer) catch unreachable;
        var res = &self.constant_buffers.items[self.constant_buffers.items.len - 1];
        // pad length
        res.add_u32(0);
        res.buffer.append(@intFromEnum(const_type)) catch unreachable;
        return res;
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
        \\bytecode.Bytecode{ .constants = { { 0, 0, 0, 25, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 14, 12, 0, 0, 0, 0, 3, 13, 23 } }, .current = { 0, 0, 0, 25, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 14, 12, 0, 0, 0, 0, 3, 13, 23 }, .global_count = 0 }
    ).expectEqualFmt(res);
}

test "let compiler" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let x = 1;
        \\ x + 1;
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\bytecode.Bytecode{ .constants = { { 0, 0, 0, 24, 0, 0, 0, 0, 0, 1, 10, 0, 0, 0, 0, 1, 11, 0, 0, 0, 0, 0, 0, 0, 0, 1, 12, 23 } }, .current = { 0, 0, 0, 24, 0, 0, 0, 0, 0, 1, 10, 0, 0, 0, 0, 1, 11, 0, 0, 0, 0, 0, 0, 0, 0, 1, 12, 23 }, .global_count = 1 }
    ).expectEqualFmt(res);
}
