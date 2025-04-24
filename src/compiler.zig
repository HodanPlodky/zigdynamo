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
        const res = self.current_size;
        self.current_size += 1;
        if (self.current_size > self.max_size) {
            self.max_size = self.current_size;
        }
        return @intCast(res);
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

    pub fn init(buffer_alloc: std.mem.Allocator) ConstantBuffer {
        return ConstantBuffer{
            .buffer = std.ArrayList(u8).init(buffer_alloc),
        };
    }

    pub fn add_u32(self: *ConstantBuffer, value: u32) void {
        self.buffer.ensureTotalCapacity(self.buffer.items.len + 4) catch unreachable;
        self.buffer.appendAssumeCapacity(@intCast((value >> 24) & 0xff));
        self.buffer.appendAssumeCapacity(@intCast((value >> 16) & 0xff));
        self.buffer.appendAssumeCapacity(@intCast((value >> 8) & 0xff));
        self.buffer.appendAssumeCapacity(@intCast(value & 0xff));
    }

    pub fn add_u8(self: *ConstantBuffer, value: u8) void {
        self.buffer.append(value) catch unreachable;
    }

    pub fn patch_len(self: *const ConstantBuffer) void {
        const len: u32 = @intCast(self.buffer.items.len - 4);
        self.set_u32(0, len);
    }
};

const FunctionBuffer = struct {
    buffer: std.ArrayList(u8),
    labels: std.ArrayList(LabelData),
    label_alloc: std.mem.Allocator,

    pub fn init(buffer_alloc: std.mem.Allocator, label_alloc: std.mem.Allocator) FunctionBuffer {
        return FunctionBuffer{
            .buffer = std.ArrayList(u8).init(buffer_alloc),
            .labels = std.ArrayList(LabelData).init(label_alloc),
            .label_alloc = label_alloc,
        };
    }

    pub fn add_inst(self: *FunctionBuffer, inst: bytecode.Instruction) void {
        self.buffer.append(@intFromEnum(inst)) catch unreachable;
    }

    pub fn add_u32(self: *FunctionBuffer, value: u32) void {
        self.buffer.ensureTotalCapacity(self.buffer.items.len + 4) catch unreachable;
        self.buffer.appendAssumeCapacity(@intCast((value >> 24) & 0xff));
        self.buffer.appendAssumeCapacity(@intCast((value >> 16) & 0xff));
        self.buffer.appendAssumeCapacity(@intCast((value >> 8) & 0xff));
        self.buffer.appendAssumeCapacity(@intCast(value & 0xff));
    }

    pub fn add_u8(self: *FunctionBuffer, value: u8) void {
        self.buffer.append(value) catch unreachable;
    }

    pub fn set_u32(self: *const FunctionBuffer, idx: u32, value: u32) void {
        self.buffer.items[idx] = @intCast((value >> 24) & 0xff);
        self.buffer.items[idx + 1] = @intCast((value >> 16) & 0xff);
        self.buffer.items[idx + 2] = @intCast((value >> 8) & 0xff);
        self.buffer.items[idx + 3] = @intCast(value & 0xff);
    }

    fn create_label(self: *FunctionBuffer) Label {
        const idx = self.labels.items.len;
        self.labels.append(LabelData.init(self.label_alloc)) catch unreachable;
        return Label{ .idx = idx };
    }

    fn set_label_position(self: *FunctionBuffer, label: Label) void {
        const pos = self.buffer.items.len;
        self.labels.items[label.idx].position = @intCast(pos);
    }

    fn add_label_use(self: *FunctionBuffer, label: Label) void {
        const pos = self.buffer.items.len;
        self.labels.items[label.idx].add_use(@intCast(pos));
        // dummy label value
        self.add_u32(0xfefefefe);
    }

    pub fn patch_len(self: *const FunctionBuffer) void {
        const len: u32 = @intCast(self.buffer.items.len - 4);
        self.set_u32(0, len);
    }

    pub fn patch_labels(self: *const FunctionBuffer) void {
        for (self.labels.items) |label| {
            for (label.uses.items) |use| {
                self.set_u32(use, label.position);
            }
        }
    }

    pub fn fix_locals(self: *FunctionBuffer, unbound_vars: *const std.ArrayList(UnboundIdent), offset: u32) void {
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
    function_buffers: std.ArrayList(FunctionBuffer),
    env: CompilerEnv,

    pub fn init(pernament_alloc: std.mem.Allocator, scratch_alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .pernament_alloc = pernament_alloc,
            .scratch_alloc = scratch_alloc,
            .constant_buffers = std.ArrayList(ConstantBuffer).init(pernament_alloc),
            .function_buffers = std.ArrayList(FunctionBuffer).init(pernament_alloc),
            .env = CompilerEnv.init(scratch_alloc),
        };
    }

    pub fn compile(self: *Compiler, program: ast.Program) !bytecode.Bytecode {
        // needs to be gather before the
        // run since the global env
        // behaves dynamically
        self.gather_globals(program);
        // padding for main (it is replaced later)
        self.function_buffers.append(FunctionBuffer.init(self.scratch_alloc, self.scratch_alloc)) catch unreachable;
        var main_buffer = self.create_function_buffer();
        var unbound_vars = std.ArrayList(UnboundIdent).init(self.scratch_alloc);
        for (program.data, 0..) |expr, i| {
            switch (expr) {
                ast.Ast.let => |let| {
                    self.compile_expr(&main_buffer, &unbound_vars, false, let.value);
                    main_buffer.add_inst(I.set_global);
                    const target_place = self.env.get_place(let.target).?;
                    std.debug.assert(std.meta.activeTag(target_place) == Place.global);
                    const target_idx = target_place.get_index();
                    main_buffer.add_u32(target_idx);
                },
                else => self.compile_expr(&main_buffer, &unbound_vars, false, &expr),
            }
            if (program.data.len - 1 > i) {
                main_buffer.add_inst(I.pop);
            }
        }
        main_buffer.add_inst(I.ret_main);
        self.add_constant_main(main_buffer);

        for (self.function_buffers.items) |*buffer| {
            buffer.patch_len();
            buffer.patch_labels();
        }

        std.debug.assert(unbound_vars.items.len == 0);

        var constants = try self.pernament_alloc.alloc(bytecode.Constant, self.constant_buffers.items.len);

        for (self.constant_buffers.items, 0..) |item, index| {
            constants[index] = bytecode.Constant.new(@ptrCast(item.buffer.items));
        }

        const res = bytecode.Bytecode{
            .functions = try self.pernament_alloc.alloc(bytecode.Function, 0),
            .constants = constants,
            .current = constants[0],
            .global_count = self.env.get_global_count(),
        };
        return res;
    }

    pub fn compile_fn(self: *Compiler, buffer: *FunctionBuffer, method: bool, function: ast.Function) void {
        var function_constant = self.create_function_buffer();
        // local count padding
        function_constant.add_u32(0);
        function_constant.add_u32(@intCast(function.params.len));
        // jit offset set to 0 which is always panic handler
        function_constant.add_u32(0);
        var unbound_vars = std.ArrayList(UnboundIdent).init(self.scratch_alloc);
        self.env.push();
        if (method) {
            _ = self.env.add_var("this");
        }
        for (function.params) |param| {
            _ = self.env.add_var(param);
        }
        self.compile_expr(&function_constant, &unbound_vars, true, function.body);
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

        const function_constant_idx = self.add_function(function_constant);

        buffer.add_inst(I.closure);
        buffer.add_u32(function_constant_idx.index);
        buffer.add_u32(@intCast(unbound_vars.items.len));
    }

    pub fn compile_expr(self: *Compiler, buffer: *FunctionBuffer, unbound_vars: *std.ArrayList(UnboundIdent), tailcall: bool, expr: *const ast.Ast) void {
        switch (expr.*) {
            ast.Ast.number => |num| {
                if (num >= 256) {
                    buffer.add_inst(I.push);
                    buffer.add_u32(num);
                } else {
                    buffer.add_inst(I.push_byte);
                    buffer.add_u8(@intCast(num));
                }
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
                self.compile_expr(buffer, unbound_vars, false, binop.left);
                self.compile_expr(buffer, unbound_vars, false, binop.right);
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
                self.compile_expr(buffer, unbound_vars, false, condition.cond);
                buffer.add_inst(I.branch);
                const then_label = buffer.create_label();
                const after_label = buffer.create_label();
                buffer.add_label_use(then_label);
                if (condition.else_block) |else_block| {
                    self.compile_expr(buffer, unbound_vars, false, else_block);
                } else {
                    buffer.add_inst(I.nil);
                }
                if (tailcall) {
                    buffer.add_inst(I.ret);
                } else {
                    buffer.add_inst(I.jump);
                    buffer.add_label_use(after_label);
                }
                buffer.set_label_position(then_label);
                self.compile_expr(buffer, unbound_vars, false, condition.then_block);
                buffer.set_label_position(after_label);
            },
            ast.Ast.function => |function| self.compile_fn(buffer, false, function),
            ast.Ast.call => |call| {
                for (call.args) |*arg| {
                    self.compile_expr(buffer, unbound_vars, false, arg);
                }
                switch (call.target.*) {
                    ast.Ast.print_fn => {
                        buffer.add_inst(I.print);
                        buffer.add_u32(@intCast(call.args.len));
                    },
                    else => {
                        self.compile_expr(buffer, unbound_vars, false, call.target);
                        buffer.add_inst(I.call);
                    },
                }
            },
            ast.Ast.string => |string| {
                const string_idx = self.create_string_constant(string);
                buffer.add_inst(I.string);
                buffer.add_u32(string_idx.index);
            },
            ast.Ast.loop => |loop| {
                const cond_label = buffer.create_label();
                const after_label = buffer.create_label();
                const body_label = buffer.create_label();
                buffer.add_inst(I.nil);
                buffer.set_label_position(cond_label);
                self.compile_expr(buffer, unbound_vars, false, loop.cond);
                buffer.add_inst(I.branch);
                buffer.add_label_use(body_label);
                buffer.add_inst(I.jump);
                buffer.add_label_use(after_label);
                buffer.set_label_position(body_label);
                buffer.add_inst(I.pop);
                self.compile_expr(buffer, unbound_vars, false, loop.body);
                buffer.add_inst(I.jump);
                buffer.add_label_use(cond_label);
                buffer.set_label_position(after_label);
            },
            ast.Ast.block => |exprs| {
                if (exprs.len == 0) {
                    buffer.add_inst(I.nil);
                    return;
                }
                for (exprs[0..(exprs.len - 1)]) |*item| {
                    self.compile_expr(buffer, unbound_vars, false, item);
                    buffer.add_inst(I.pop);
                }
                self.compile_expr(buffer, unbound_vars, false, &exprs[exprs.len - 1]);
            },
            ast.Ast.let => |let| {
                self.compile_expr(buffer, unbound_vars, false, let.value);
                const idx = self.env.add_var(let.target);
                buffer.add_inst(I.set);
                buffer.add_u32(idx);
            },
            ast.Ast.assign => |assign| {
                self.compile_expr(buffer, unbound_vars, false, assign.value);
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
            ast.Ast.object => |object| {
                if (object.prototype) |proto| {
                    self.compile_expr(buffer, unbound_vars, false, proto);
                } else {
                    buffer.add_inst(I.nil);
                }
                var class_const = self.create_constant(bytecode.ConstantType.class);
                for (object.fields) |field| {
                    const string_idx = self.create_string_constant(field.name);
                    class_const.add_u32(string_idx.index);
                    switch (field.value.*) {
                        ast.Ast.function => |function| self.compile_fn(buffer, true, function),
                        else => self.compile_expr(buffer, unbound_vars, false, field.value),
                    }
                }
                const class_idx = self.add_constant(class_const);
                buffer.add_inst(I.object);
                buffer.add_u32(class_idx.index);
            },
            ast.Ast.field_access => |access| {
                self.compile_expr(buffer, unbound_vars, false, access.target);
                const string_idx = self.create_string_constant(access.field);
                buffer.add_inst(I.get_field);
                buffer.add_u32(string_idx.index);
            },
            ast.Ast.field_assign => |assign| {
                self.compile_expr(buffer, unbound_vars, false, assign.value);
                self.compile_expr(buffer, unbound_vars, false, assign.object);
                const string_idx = self.create_string_constant(assign.field);
                buffer.add_inst(I.set_field);
                buffer.add_u32(string_idx.index);
            },
            ast.Ast.field_call => |methodcall| {
                for (methodcall.args) |*arg| {
                    self.compile_expr(buffer, unbound_vars, false, arg);
                }
                self.compile_expr(buffer, unbound_vars, false, methodcall.target);
                const string_idx = self.create_string_constant(methodcall.field);
                buffer.add_inst(I.methodcall);
                buffer.add_u32(string_idx.index);
            },
            else => {
                std.debug.print("{}\n", .{expr});
                @panic("unimplemented");
            },
        }
    }

    fn compile_ident(self: *Compiler, buffer: *FunctionBuffer, ident: []const u8) bool {
        if (self.env.get_place(ident)) |place| {
            const idx = place.get_index();
            switch (place) {
                Place.local => if (idx < 256)
                    buffer.add_inst(I.get_small)
                else
                    buffer.add_inst(I.get),
                Place.global => if (idx < 256)
                    buffer.add_inst(I.get_global_small)
                else
                    buffer.add_inst(I.get_global),
            }
            if (idx < 256) {
                buffer.add_u8(@intCast(idx));
            } else {
                buffer.add_u32(idx);
            }
            return false;
        }
        return true;
    }

    fn set_unbound(self: *const Compiler, buffer: *const FunctionBuffer, unbound_vars: *std.ArrayList(UnboundIdent), ident: []const u8) void {
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

    fn create_function_buffer(self: *Compiler) FunctionBuffer {
        const buffer = FunctionBuffer.init(self.pernament_alloc, self.scratch_alloc);
        return buffer;
    }

    fn create_constant(self: *Compiler, const_type: bytecode.ConstantType) ConstantBuffer {
        var constant_buffer = ConstantBuffer.init(self.pernament_alloc);
        // pad length
        constant_buffer.add_u32(0);
        constant_buffer.buffer.append(@intFromEnum(const_type)) catch unreachable;
        return constant_buffer;
    }

    fn create_string_constant(self: *Compiler, string: []const u8) bytecode.ConstantIndex {
        var string_buffer = self.create_constant(bytecode.ConstantType.string);
        string_buffer.buffer.appendSlice(string) catch unreachable;
        return self.add_constant(string_buffer);
    }

    fn add_constant(self: *Compiler, constant_buffer: ConstantBuffer) bytecode.ConstantIndex {
        if (self.dedupe_constant(constant_buffer)) |idx| {
            return idx;
        }
        self.constant_buffers.append(constant_buffer) catch unreachable;
        return bytecode.ConstantIndex.new(@intCast(self.constant_buffers.items.len - 1));
    }

    fn add_function(self: *Compiler, function: FunctionBuffer) bytecode.FunctionIndex {
        self.function_buffers.append(function) catch unreachable;
        return bytecode.FunctionIndex.new(@intCast(self.constant_buffers.items.len - 1));
    }

    fn add_constant_main(self: *Compiler, constant_buffer: FunctionBuffer) void {
        self.function_buffers.items[0] = constant_buffer;
    }

    fn dedupe_constant(self: *Compiler, checked_constant: ConstantBuffer) ?bytecode.ConstantIndex {
        for (self.constant_buffers.items, 0..) |constant, idx| {
            if (std.mem.eql(u8, checked_constant.buffer.items, constant.buffer.items)) {
                return bytecode.ConstantIndex.new(@intCast(idx));
            }
        }
        return null;
    }

    fn get_constant(self: *Compiler, idx: bytecode.ConstantIndex) *FunctionBuffer {
        return &self.constant_buffers.items[@intCast(idx.index)];
    }

    fn gather_globals(self: *Compiler, program: ast.Program) void {
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
        \\main_function (17 bytes)
        \\	5: push_byte 1
        \\	7: push_byte 2
        \\	9: push_byte 2
        \\	11: mul
        \\	12: add
        \\	13: push_byte 3
        \\	15: sub
        \\	16: ret_main
        \\
        \\
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
        \\main_function (19 bytes)
        \\	5: push_byte 1
        \\	7: set_global 0 0 0 0
        \\	12: pop
        \\	13: get_global_small 0
        \\	15: push_byte 1
        \\	17: add
        \\	18: ret_main
        \\
        \\
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
        \\ if (x) 1000 else 2;
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\main_function (32 bytes)
        \\	5: true
        \\	6: set_global 0 0 0 0
        \\	11: pop
        \\	12: get_global_small 0
        \\	14: branch 0 0 0 26
        \\	19: push_byte 2
        \\	21: jump 0 0 0 31
        \\	26: push 0 0 3 232
        \\	31: ret_main
        \\
        \\
    ).expectEqualFmt(res);
}

test "object compiler" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let o = object {
        \\      a: 1,
        \\      val: "x",
        \\ };
        \\ print(o.a + 1);
        \\ print(o.val);
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\main_function (53 bytes)
        \\	5: nil
        \\	6: push_byte 1
        \\	8: string 0 0 0 3
        \\	13: object 0 0 0 4
        \\	18: set_global 0 0 0 0
        \\	23: pop
        \\	24: get_global_small 0
        \\	26: get_field 0 0 0 1
        \\	31: push_byte 1
        \\	33: add
        \\	34: print 0 0 0 1
        \\	39: pop
        \\	40: get_global_small 0
        \\	42: get_field 0 0 0 2
        \\	47: print 0 0 0 1
        \\	52: ret_main
        \\
        \\string (6 bytes)
        \\string: a
        \\
        \\string (8 bytes)
        \\string: val
        \\
        \\string (6 bytes)
        \\string: x
        \\
        \\class (13 bytes)
        \\class: 1 2
        \\
    ).expectEqualFmt(res);
}

test "object 2 compiler" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let o = object {
        \\      a: 1,
        \\      val: "x",
        \\ };
        \\ print(o.a + 1);
        \\ print(o.val);
        \\ o.a = 2;
        \\ print(o.a + 1);
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\main_function (79 bytes)
        \\	5: nil
        \\	6: push_byte 1
        \\	8: string 0 0 0 3
        \\	13: object 0 0 0 4
        \\	18: set_global 0 0 0 0
        \\	23: pop
        \\	24: get_global_small 0
        \\	26: get_field 0 0 0 1
        \\	31: push_byte 1
        \\	33: add
        \\	34: print 0 0 0 1
        \\	39: pop
        \\	40: get_global_small 0
        \\	42: get_field 0 0 0 2
        \\	47: print 0 0 0 1
        \\	52: pop
        \\	53: push_byte 2
        \\	55: get_global_small 0
        \\	57: set_field 0 0 0 1
        \\	62: pop
        \\	63: get_global_small 0
        \\	65: get_field 0 0 0 1
        \\	70: push_byte 1
        \\	72: add
        \\	73: print 0 0 0 1
        \\	78: ret_main
        \\
        \\string (6 bytes)
        \\string: a
        \\
        \\string (8 bytes)
        \\string: val
        \\
        \\string (6 bytes)
        \\string: x
        \\
        \\class (13 bytes)
        \\class: 1 2
        \\
    ).expectEqualFmt(res);
}

test "object 3 compiler" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let o = object {
        \\      a: 1,
        \\      val: "x",
        \\      f: fn(x) = {
        \\          print(this.a + x);
        \\      },
        \\ };
        \\ o.f(1);
        \\ print(o.val);
        \\ o.a = 2;
        \\ o.f(2);
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\main_function (76 bytes)
        \\	5: nil
        \\	6: push_byte 1
        \\	8: string 0 0 0 3
        \\	13: closure 0 0 0 5 0 0 0 0
        \\	22: object 0 0 0 6
        \\	27: set_global 0 0 0 0
        \\	32: pop
        \\	33: push_byte 1
        \\	35: get_global_small 0
        \\	37: methodcall 0 0 0 4
        \\	42: pop
        \\	43: get_global_small 0
        \\	45: get_field 0 0 0 2
        \\	50: print 0 0 0 1
        \\	55: pop
        \\	56: push_byte 2
        \\	58: get_global_small 0
        \\	60: set_field 0 0 0 1
        \\	65: pop
        \\	66: push_byte 2
        \\	68: get_global_small 0
        \\	70: methodcall 0 0 0 4
        \\	75: ret_main
        \\
        \\string (6 bytes)
        \\string: a
        \\
        \\string (8 bytes)
        \\string: val
        \\
        \\string (6 bytes)
        \\string: x
        \\
        \\string (6 bytes)
        \\string: f
        \\
        \\function (33 bytes)
        \\	17: get_small 0
        \\	19: get_field 0 0 0 1
        \\	24: get_small 1
        \\	26: add
        \\	27: print 0 0 0 1
        \\	32: ret
        \\
        \\class (17 bytes)
        \\class: 1 2 4
        \\
    ).expectEqualFmt(res);
}

test "linked list" {
    const Parser = @import("parser.zig").Parser;
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let createnode = fn(val) = object {
        \\     val : val,
        \\     next: nil,
        \\ };
        \\ 
        \\ let createlist = fn() = object {
        \\     head: nil,
        \\ 
        \\     append: fn(val) = {
        \\         if (this.head == nil) {
        \\             this.head = createnode(val);
        \\         } else {
        \\             let tmp = this.head;
        \\             while (tmp.next != nil) {
        \\                 tmp = tmp.next;
        \\             };
        \\             tmp.next = createnode(val);
        \\         };
        \\     },
        \\ 
        \\     prepend: fn(val) = {
        \\         let tmp = this.head;
        \\         this.head = createnode(val);
        \\         this.head.next = tmp;
        \\     },
        \\ 
        \\     pop: fn() = {
        \\         if (this.head == nil) {
        \\ 
        \\         } else if (this.head.next == nil) {
        \\             let tmp = this.head;
        \\             this.head == nil;
        \\             tmp.val;
        \\         } else {
        \\             let tmp = this.head;
        \\             while (tmp.next.next != nil) {
        \\                 tmp = tmp.next;
        \\             };
        \\             let res = tmp.val;
        \\             tmp.next = nil;
        \\             res;
        \\         };
        \\     },
        \\ 
        \\     debug: fn() = {
        \\         let tmp = this.head;
        \\         while (tmp != nil) {
        \\             print(tmp.val);
        \\             tmp = tmp.next;
        \\         };
        \\     },
        \\ };
        \\ 
        \\ let list = createlist();
        \\ 
        \\ let i = 0;
        \\ while (i < 100) {
        \\     list.append(1);
        \\     list.append(2);
        \\     list.append(3);
        \\     list.prepend(42);
        \\     print(list.pop());
        \\     print(list.pop());
        \\     print(list.pop());
        \\     i = i + 1;
        \\ };
        \\ 
        \\ list.debug();
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try oh.snap(@src(),
        \\main_function (172 bytes)
        \\	5: closure 0 0 0 4 0 0 0 0
        \\	14: set_global 0 0 0 0
        \\	19: pop
        \\	20: closure 0 0 0 15 0 0 0 0
        \\	29: set_global 0 0 0 1
        \\	34: pop
        \\	35: get_global_small 1
        \\	37: call
        \\	38: set_global 0 0 0 2
        \\	43: pop
        \\	44: push_byte 0
        \\	46: set_global 0 0 0 3
        \\	51: pop
        \\	52: nil
        \\	53: get_global_small 3
        \\	55: push_byte 100
        \\	57: lt
        \\	58: branch 0 0 0 68
        \\	63: jump 0 0 0 163
        \\	68: pop
        \\	69: push_byte 1
        \\	71: get_global_small 2
        \\	73: methodcall 0 0 0 6
        \\	78: pop
        \\	79: push_byte 2
        \\	81: get_global_small 2
        \\	83: methodcall 0 0 0 6
        \\	88: pop
        \\	89: push_byte 3
        \\	91: get_global_small 2
        \\	93: methodcall 0 0 0 6
        \\	98: pop
        \\	99: push_byte 42
        \\	101: get_global_small 2
        \\	103: methodcall 0 0 0 8
        \\	108: pop
        \\	109: get_global_small 2
        \\	111: methodcall 0 0 0 10
        \\	116: print 0 0 0 1
        \\	121: pop
        \\	122: get_global_small 2
        \\	124: methodcall 0 0 0 10
        \\	129: print 0 0 0 1
        \\	134: pop
        \\	135: get_global_small 2
        \\	137: methodcall 0 0 0 10
        \\	142: print 0 0 0 1
        \\	147: pop
        \\	148: get_global_small 3
        \\	150: push_byte 1
        \\	152: add
        \\	153: set_global 0 0 0 3
        \\	158: jump 0 0 0 53
        \\	163: pop
        \\	164: get_global_small 2
        \\	166: methodcall 0 0 0 12
        \\	171: ret_main
        \\
        \\string (8 bytes)
        \\string: val
        \\
        \\string (9 bytes)
        \\string: next
        \\
        \\class (13 bytes)
        \\class: 1 2
        \\function (27 bytes)
        \\	17: nil
        \\	18: get_small 0
        \\	20: nil
        \\	21: object 0 0 0 3
        \\	26: ret
        \\
        \\string (9 bytes)
        \\string: head
        \\
        \\string (11 bytes)
        \\string: append
        \\
        \\function (113 bytes)
        \\	17: get_small 0
        \\	19: get_field 0 0 0 5
        \\	24: nil
        \\	25: eq
        \\	26: branch 0 0 0 100
        \\	31: get_small 0
        \\	33: get_field 0 0 0 5
        \\	38: set 0 0 0 2
        \\	43: pop
        \\	44: nil
        \\	45: get_small 2
        \\	47: get_field 0 0 0 2
        \\	52: nil
        \\	53: ne
        \\	54: branch 0 0 0 64
        \\	59: jump 0 0 0 82
        \\	64: pop
        \\	65: get_small 2
        \\	67: get_field 0 0 0 2
        \\	72: set 0 0 0 2
        \\	77: jump 0 0 0 45
        \\	82: pop
        \\	83: get_small 1
        \\	85: get_global_small 0
        \\	87: call
        \\	88: get_small 2
        \\	90: set_field 0 0 0 2
        \\	95: jump 0 0 0 112
        \\	100: get_small 1
        \\	102: get_global_small 0
        \\	104: call
        \\	105: get_small 0
        \\	107: set_field 0 0 0 5
        \\	112: ret
        \\
        \\string (12 bytes)
        \\string: prepend
        \\
        \\function (58 bytes)
        \\	17: get_small 0
        \\	19: get_field 0 0 0 5
        \\	24: set 0 0 0 2
        \\	29: pop
        \\	30: get_small 1
        \\	32: get_global_small 0
        \\	34: call
        \\	35: get_small 0
        \\	37: set_field 0 0 0 5
        \\	42: pop
        \\	43: get_small 2
        \\	45: get_small 0
        \\	47: get_field 0 0 0 5
        \\	52: set_field 0 0 0 2
        \\	57: ret
        \\
        \\string (8 bytes)
        \\string: pop
        \\
        \\function (173 bytes)
        \\	17: get_small 0
        \\	19: get_field 0 0 0 5
        \\	24: nil
        \\	25: eq
        \\	26: branch 0 0 0 171
        \\	31: get_small 0
        \\	33: get_field 0 0 0 5
        \\	38: get_field 0 0 0 2
        \\	43: nil
        \\	44: eq
        \\	45: branch 0 0 0 136
        \\	50: get_small 0
        \\	52: get_field 0 0 0 5
        \\	57: set 0 0 0 1
        \\	62: pop
        \\	63: nil
        \\	64: get_small 1
        \\	66: get_field 0 0 0 2
        \\	71: get_field 0 0 0 2
        \\	76: nil
        \\	77: ne
        \\	78: branch 0 0 0 88
        \\	83: jump 0 0 0 106
        \\	88: pop
        \\	89: get_small 1
        \\	91: get_field 0 0 0 2
        \\	96: set 0 0 0 1
        \\	101: jump 0 0 0 64
        \\	106: pop
        \\	107: get_small 1
        \\	109: get_field 0 0 0 1
        \\	114: set 0 0 0 2
        \\	119: pop
        \\	120: nil
        \\	121: get_small 1
        \\	123: set_field 0 0 0 2
        \\	128: pop
        \\	129: get_small 2
        \\	131: jump 0 0 0 166
        \\	136: get_small 0
        \\	138: get_field 0 0 0 5
        \\	143: set 0 0 0 3
        \\	148: pop
        \\	149: get_small 0
        \\	151: get_field 0 0 0 5
        \\	156: nil
        \\	157: eq
        \\	158: pop
        \\	159: get_small 1
        \\	161: get_field 0 0 0 1
        \\	166: jump 0 0 0 172
        \\	171: nil
        \\	172: ret
        \\
        \\string (10 bytes)
        \\string: debug
        \\
        \\function (77 bytes)
        \\	17: get_small 0
        \\	19: get_field 0 0 0 5
        \\	24: set 0 0 0 1
        \\	29: pop
        \\	30: nil
        \\	31: get_small 1
        \\	33: nil
        \\	34: ne
        \\	35: branch 0 0 0 45
        \\	40: jump 0 0 0 76
        \\	45: pop
        \\	46: get_small 1
        \\	48: get_field 0 0 0 1
        \\	53: print 0 0 0 1
        \\	58: pop
        \\	59: get_small 1
        \\	61: get_field 0 0 0 2
        \\	66: set 0 0 0 1
        \\	71: jump 0 0 0 31
        \\	76: ret
        \\
        \\class (25 bytes)
        \\class: 5 6 8 10 12
        \\function (61 bytes)
        \\	17: nil
        \\	18: nil
        \\	19: closure 0 0 0 7 0 0 0 0
        \\	28: closure 0 0 0 9 0 0 0 0
        \\	37: closure 0 0 0 11 0 0 0 0
        \\	46: closure 0 0 0 13 0 0 0 0
        \\	55: object 0 0 0 14
        \\	60: ret
        \\
        \\
    ).expectEqualFmt(res);
}
