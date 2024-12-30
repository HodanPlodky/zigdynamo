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

    pub fn add_var(self: *CompilerFrame, var_name: []const u8) void {
        if (self.get_index(var_name) == null) {
            self.vars.append(var_name) catch unreachable;
        }
    }
};

const CompilerFnFrame = struct {
    frames: std.ArrayList(CompilerFrame),
    max_size: usize,
    current_size: usize,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) CompilerFnFrame {
        var frames = std.ArrayList(CompilerFrame).init(alloc);
        frames.append(CompilerFrame.init(alloc)) catch unreachable;
        return CompilerFnFrame{
            .frames = frames,
            .max_size = 0,
            .current_size = 0,
            .alloc = alloc,
        };
    }

    pub fn get_place(self: *const CompilerFnFrame, var_name: []const u8) ?Place {
        var offset: usize = 0;
        for (self.frames.items) |frame| {
            if (frame.get_index(var_name)) |idx| {
                return Place{ .local = @intCast(offset + idx) };
            }
            offset += frame.len();
        }
        return null;
    }

    pub fn add_var(self: *CompilerFnFrame, var_name: []const u8) u32 {
        self.get_current().add_var(var_name);
        self.current_size += 1;
        if (self.current_size > self.max_size) {
            self.max_size = self.current_size;
        }
        return @intCast(self.current_size);
    }

    pub fn get_current(self: *CompilerFnFrame) *CompilerFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    pub fn get_current_const(self: *const CompilerFnFrame) *const CompilerFrame {
        return &self.frames.items[self.frames.items.len - 1];
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
    function_frames: std.ArrayList(CompilerFnFrame),
    global: CompilerFrame,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) CompilerEnv {
        return CompilerEnv{
            .function_frames = std.ArrayList(CompilerFnFrame).init(alloc),
            .global = CompilerFrame.init(alloc),
            .alloc = alloc,
        };
    }

    pub fn get_global_count(self: *const CompilerEnv) usize {
        return self.global.len();
    }

    pub fn get_place(self: *const CompilerEnv, var_name: []const u8) ?Place {
        if (self.get_current_const()) |frame| {
            if (frame.get_place(var_name)) |place| {
                return place;
            }
        }

        if (self.global.get_index(var_name)) |index| {
            return Place{ .global = @intCast(index) };
        }

        return null;
    }

    pub fn add_var(self: *CompilerEnv, var_name: []const u8) u32 {
        var current = self.get_current().?;
        return current.add_var(var_name);
    }

    pub fn add_global(self: *CompilerEnv, var_name: []const u8) void {
        self.global.add_var(var_name);
    }

    pub fn get_current(self: *CompilerEnv) ?*CompilerFnFrame {
        if (self.function_frames.items.len > 0) {
            return &self.function_frames.items[self.function_frames.items.len - 1];
        }
        return null;
    }

    pub fn get_current_const(self: *const CompilerEnv) ?*const CompilerFnFrame {
        if (self.function_frames.items.len > 0) {
            return &self.function_frames.items[self.function_frames.items.len - 1];
        }
        return null;
    }

    pub fn push(self: *CompilerEnv) void {
        self.function_frames.append(CompilerFnFrame.init(self.alloc)) catch unreachable;
    }

    pub fn pop(self: *CompilerEnv) void {
        _ = self.function_frames.pop();
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

    pub fn fix_locals(self: *ConstantBuffer, unbound_vars: *const std.ArrayList(UnboundIdent), offset: u32) void {
        for (unbound_vars.items, 0..) |unbound, order| {
            const tmp: u32 = @intCast(order);
            for (unbound.positions.items) |pos| {
                self.set_u32(pos, offset + tmp);
            }
        }
    }
};

const UnboundIdent = struct {
    ident: []const u8,
    positions: std.ArrayListUnmanaged(u32),

    pub fn init(ident: []const u8) UnboundIdent {
        return UnboundIdent{
            .ident = ident,
            .positions = std.ArrayListUnmanaged(u32){},
        };
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
            .env = CompilerEnv.init(scratch_alloc),
        };
    }

    pub fn compile(self: *Compiler, program: ast.Program) !bytecode.Bytecode {
        // needs to be gather before the
        // run since the global env
        // behaves dynamically
        self.gather_globals(program);
        const main_buffer = self.get_constant(self.create_constant(bytecode.ConstantType.function));
        var unbound_vars = std.ArrayList(UnboundIdent).init(self.scratch_alloc);
        for (program.data, 0..) |expr, i| {
            switch (expr) {
                ast.Ast.let => |let| {
                    self.compile_expr(main_buffer, &unbound_vars, let.value);
                    main_buffer.add_inst(I.set_global);
                    const target_place = self.env.get_place(let.target).?;
                    std.debug.assert(std.meta.activeTag(target_place) == Place.global);
                    const target_idx = target_place.get_index();
                    main_buffer.add_u32(target_idx);
                },
                else => self.compile_expr(main_buffer, &unbound_vars, &expr),
            }
            if (program.data.len - 1 > i) {
                main_buffer.add_inst(I.pop);
            }
        }
        main_buffer.add_inst(I.ret_main);

        for (self.constant_buffers.items) |*buffer| {
            buffer.patch_len();
            buffer.patch_labels();
        }

        std.debug.assert(unbound_vars.items.len == 0);

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

    pub fn compile_fn(self: *Compiler, buffer: *ConstantBuffer, function: ast.Function) void {
        const function_constant_idx = self.create_constant(bytecode.ConstantType.function);
        const function_constant = self.get_constant(function_constant_idx);
        // local count padding
        function_constant.add_u32(0);
        function_constant.add_u32(@intCast(function.params.len));
        var unbound_vars = std.ArrayList(UnboundIdent).init(self.scratch_alloc);
        self.env.push();
        for (function.params) |param| {
            _ = self.env.add_var(param);
        }
        self.compile_expr(function_constant, &unbound_vars, function.body);
        const max_size: u32 = @intCast(self.env.get_current().?.max_size);
        self.env.pop();

        const unbound_count: u32 = @intCast(unbound_vars.items.len);
        function_constant.set_u32(5, max_size + unbound_count);
        function_constant.add_inst(I.ret);

        for (unbound_vars.items) |unbound| {
            if (self.compile_ident(buffer, unbound.ident)) {
                std.debug.print("{s}\n", .{unbound.ident});
                @panic("non existant var");
            }
        }

        function_constant.fix_locals(&unbound_vars, max_size);

        buffer.add_inst(I.closure);
        buffer.add_u32(function_constant_idx.index);
        buffer.add_u32(@intCast(unbound_vars.items.len));
    }

    pub fn compile_expr(self: *Compiler, buffer: *ConstantBuffer, unbound_vars: *std.ArrayList(UnboundIdent), expr: *const ast.Ast) void {
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
                self.compile_expr(buffer, unbound_vars, binop.left);
                self.compile_expr(buffer, unbound_vars, binop.right);
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
                if (self.compile_ident(buffer, ident)) {
                    buffer.add_inst(I.get);
                    self.set_unbound(buffer, unbound_vars, ident);
                    // dummy place
                    buffer.add_u32(0xfcfcfcfc);
                }
            },
            ast.Ast.condition => |condition| {
                self.compile_expr(buffer, unbound_vars, condition.cond);
                buffer.add_inst(I.branch);
                const then_label = buffer.create_label();
                const after_label = buffer.create_label();
                buffer.add_label_use(then_label);
                if (condition.else_block) |else_block| {
                    self.compile_expr(buffer, unbound_vars, else_block);
                } else {
                    buffer.add_inst(I.nil);
                }
                buffer.add_inst(I.jump);
                buffer.add_label_use(after_label);
                buffer.set_label_position(then_label);
                self.compile_expr(buffer, unbound_vars, condition.then_block);
                buffer.set_label_position(after_label);
            },
            ast.Ast.function => |function| self.compile_fn(buffer, function),
            ast.Ast.call => |call| {
                for (call.args) |*arg| {
                    self.compile_expr(buffer, unbound_vars, arg);
                }
                switch (call.target.*) {
                    ast.Ast.print_fn => {
                        buffer.add_inst(I.print);
                        buffer.add_u32(@intCast(call.args.len));
                    },
                    else => {
                        self.compile_expr(buffer, unbound_vars, call.target);
                        buffer.add_inst(I.call);
                    },
                }
            },
            ast.Ast.string => |string| {
                const string_idx = self.create_constant(bytecode.ConstantType.string);
                const string_buffer = self.get_constant(string_idx);
                string_buffer.buffer.appendSlice(string) catch unreachable;
                buffer.add_inst(I.string);
                buffer.add_u32(string_idx.index);
            },
            ast.Ast.loop => |loop| {
                self.compile_expr(buffer, unbound_vars, loop.cond);
                const after_label = buffer.create_label();
                const body_label = buffer.create_label();
                buffer.add_inst(I.branch);
                buffer.add_label_use(body_label);
                buffer.add_inst(I.jump);
                buffer.add_label_use(after_label);
                buffer.set_label_position(body_label);
                self.compile_expr(buffer, unbound_vars, loop.body);
                buffer.set_label_position(after_label);
            },
            ast.Ast.block => |exprs| {
                for (exprs[0..(exprs.len - 1)]) |*item| {
                    self.compile_expr(buffer, unbound_vars, item);
                    buffer.add_inst(I.pop);
                }
                self.compile_expr(buffer, unbound_vars, &exprs[exprs.len - 1]);
            },
            ast.Ast.let => |let| {
                self.compile_expr(buffer, unbound_vars, let.value);
                const idx = self.env.add_var(let.target);
                buffer.add_inst(I.set);
                buffer.add_u32(idx);
            },
            ast.Ast.assign => |assign| {
                self.compile_expr(buffer, unbound_vars, assign.value);
                if (self.env.get_place(assign.target)) |place| {
                    switch (place) {
                        Place.local => buffer.add_inst(I.set),
                        Place.global => buffer.add_inst(I.set_global),
                    }
                    buffer.add_u32(place.get_index());
                } else {
                    buffer.add_inst(I.set);
                    self.set_unbound(buffer, unbound_vars, assign.target);
                    // dummy idx
                    buffer.add_u32(0xfbfbfbfb);
                }
            },
            else => {
                std.debug.print("{}\n", .{expr});
                @panic("unimplemented");
            },
        }
    }

    fn compile_ident(self: *Compiler, buffer: *ConstantBuffer, ident: []const u8) bool {
        if (self.env.get_place(ident)) |place| {
            switch (place) {
                Place.local => buffer.add_inst(I.get),
                Place.global => buffer.add_inst(I.get_global),
            }
            const idx = place.get_index();
            buffer.add_u32(idx);
            return false;
        }
        return true;
    }

    fn set_unbound(self: *const Compiler, buffer: *const ConstantBuffer, unbound_vars: *std.ArrayList(UnboundIdent), ident: []const u8) void {
        const position: u32 = @intCast(buffer.buffer.items.len);
        for (unbound_vars.items) |*unbound| {
            if (std.mem.eql(u8, unbound.ident, ident)) {
                unbound.positions.append(self.scratch_alloc, position) catch unreachable;
            }
        }

        var new_unbound = UnboundIdent.init(ident);
        new_unbound.positions.append(self.scratch_alloc, position) catch unreachable;
        unbound_vars.append(new_unbound) catch unreachable;
    }

    pub fn create_constant(self: *Compiler, const_type: bytecode.ConstantType) bytecode.ConstantIndex {
        const constant_buffer = ConstantBuffer.init(self.pernament_alloc, self.scratch_alloc);
        self.constant_buffers.append(constant_buffer) catch unreachable;
        var tmp = &self.constant_buffers.items[self.constant_buffers.items.len - 1];
        // pad length
        tmp.add_u32(0);
        tmp.buffer.append(@intFromEnum(const_type)) catch unreachable;
        return bytecode.ConstantIndex.new(@intCast(self.constant_buffers.items.len - 1));
    }

    pub fn get_constant(self: *Compiler, idx: bytecode.ConstantIndex) *ConstantBuffer {
        return &self.constant_buffers.items[@intCast(idx.index)];
    }

    pub fn gather_globals(self: *Compiler, program: ast.Program) void {
        for (program.data) |item| {
            switch (item) {
                ast.Ast.let => |let| self.env.add_global(let.target),
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
        \\bytecode.Bytecode{ .constants = { [ push, 0, 0, 25, 0, push, 0, 0, 0, 1, push, 0, 0, 0, 2, push, 0, 0, 0, 2, mul, add, push, 0, 0, 0, 3, sub, ret_main, ] }, .current = [ push, 0, 0, 25, 0, push, 0, 0, 0, 1, push, 0, 0, 0, 2, push, 0, 0, 0, 2, mul, add, push, 0, 0, 0, 3, sub, ret_main, ], .global_count = 0 }
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
        \\bytecode.Bytecode{ .constants = { [ push, 0, 0, 24, 0, push, 0, 0, 0, 1, set_global, 0, 0, 0, 0, pop, get_global, 0, 0, 0, 0, push, 0, 0, 0, 1, add, ret_main, ] }, .current = [ push, 0, 0, 24, 0, push, 0, 0, 0, 1, set_global, 0, 0, 0, 0, pop, get_global, 0, 0, 0, 0, push, 0, 0, 0, 1, add, ret_main, ], .global_count = 1 }
    ).expectEqualFmt(res);
}

test "condition compiler" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let x = true;
        \\ if (x) 1 else 2;
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\bytecode.Bytecode{ .constants = { [ push, 0, 0, 34, 0, true, set_global, 0, 0, 0, 0, pop, get_global, 0, 0, 0, 0, branch, 0, 0, 0, 32, push, 0, 0, 0, 2, jump, 0, 0, 0, 37, push, 0, 0, 0, 1, ret_main, ] }, .current = [ push, 0, 0, 34, 0, true, set_global, 0, 0, 0, 0, pop, get_global, 0, 0, 0, 0, branch, 0, 0, 0, 32, push, 0, 0, 0, 2, jump, 0, 0, 0, 37, push, 0, 0, 0, 1, ret_main, ], .global_count = 1 }
    ).expectEqualFmt(res);
}
