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

    pub fn set_u32(self: *const ConstantBuffer, idx: u32, value: u32) void {
        self.buffer.items[idx] = @intCast((value >> 24) & 0xff);
        self.buffer.items[idx + 1] = @intCast((value >> 16) & 0xff);
        self.buffer.items[idx + 2] = @intCast((value >> 8) & 0xff);
        self.buffer.items[idx + 3] = @intCast(value & 0xff);
    }

    pub fn patch_len(self: *const ConstantBuffer) void {
        const len: u32 = @intCast(self.buffer.items.len - 4);
        self.set_u32(0, len);
    }
};

const FunctionBuffer = struct {
    const BufferType = std.ArrayListAligned(u8, @alignOf(bytecode.Function));
    const functionHeader: usize = 12;

    buffer: BufferType,
    labels: std.ArrayList(LabelData),
    label_alloc: std.mem.Allocator,
    source: ?*const ast.Function,

    pub fn init(
        buffer_alloc: std.mem.Allocator,
        label_alloc: std.mem.Allocator,
        source: ?*const ast.Function,
    ) FunctionBuffer {
        var res = FunctionBuffer{
            .buffer = BufferType.init(buffer_alloc),
            .labels = std.ArrayList(LabelData).init(label_alloc),
            .label_alloc = label_alloc,
            .source = source,
        };

        // param count
        res.buffer.appendNTimes(0, 4) catch unreachable;
        // local count
        res.buffer.appendNTimes(0, 4) catch unreachable;
        // size padding
        res.buffer.appendNTimes(0, 4) catch unreachable;
        return res;
    }

    pub fn get_fn_ptr(self: *const FunctionBuffer) *const bytecode.Function {
        return @ptrCast(@alignCast(self.buffer.items.ptr));
    }

    pub fn get_fn_ptr_mut(self: *FunctionBuffer) *bytecode.Function {
        return @ptrCast(@alignCast(self.buffer.items.ptr));
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
        self.labels.items[label.idx].position = @intCast(pos - functionHeader);
    }

    fn add_label_use(self: *FunctionBuffer, label: Label) void {
        const pos = self.buffer.items.len;
        self.labels.items[label.idx].add_use(@intCast(pos));
        // dummy label value
        self.add_u32(0xfefefefe);
    }

    pub fn patch_len(self: *FunctionBuffer) void {
        const len: u32 = @intCast(self.buffer.items.len - functionHeader);
        var tmp = self.get_fn_ptr_mut();
        tmp.code.count = len;
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
            .constant_buffers = std.ArrayList(ConstantBuffer).init(scratch_alloc),
            .function_buffers = std.ArrayList(FunctionBuffer).init(scratch_alloc),
            .env = CompilerEnv.init(scratch_alloc),
        };
    }

    pub fn compile(self: *Compiler, program: ast.Program) !bytecode.Bytecode {
        // needs to be gather before the
        // run since the global env
        // behaves dynamically
        self.gather_globals(program);
        var main_buffer = self.create_main_buffer();
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

        for (self.constant_buffers.items) |*buffer| {
            buffer.patch_len();
        }

        std.debug.assert(unbound_vars.items.len == 0);

        var constants = try self.pernament_alloc.alloc(bytecode.Constant, self.constant_buffers.items.len);
        var functions = try self.pernament_alloc.alloc(*const bytecode.Function, self.function_buffers.items.len);
        var sources = try self.pernament_alloc.alloc(*const ast.Function, self.function_buffers.items.len - 1);

        for (self.constant_buffers.items, 0..) |item, index| {
            constants[index] = bytecode.Constant.new(@ptrCast(item.buffer.items));
        }

        for (self.function_buffers.items, 0..) |*item, index| {
            functions[index] = item.get_fn_ptr();
            if (item.source) |source| {
                // only main should not have source
                std.debug.assert(index > 0);
                sources[index - 1] = source;
            }
        }

        const res = bytecode.Bytecode{
            .functions = bytecode.Functions.new(functions, sources),
            .constants = constants,
            .current = functions[0].code.get_unchecked_slice_const(),
            .global_count = self.env.get_global_count(),
        };
        return res;
    }

    pub fn compile_fn(self: *Compiler, buffer: *FunctionBuffer, method: bool, function: *const ast.Function) void {
        var function_constant = self.create_function_buffer(function);
        // local count padding
        function_constant.get_fn_ptr_mut().param_count = @intCast(function.params.len);
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
        function_constant.get_fn_ptr_mut().locals_count = max_size + unbound_count;
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
            ast.Ast.function => |*function| self.compile_fn(buffer, false, function),
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
                        ast.Ast.function => |*function| self.compile_fn(buffer, true, function),
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

    fn create_function_buffer(self: *Compiler, function: *const ast.Function) FunctionBuffer {
        const buffer = FunctionBuffer.init(self.pernament_alloc, self.scratch_alloc, function);
        return buffer;
    }

    fn create_main_buffer(self: *Compiler) FunctionBuffer {
        std.debug.assert(self.function_buffers.items.len == 0);
        const buffer = FunctionBuffer.init(self.pernament_alloc, self.scratch_alloc, null);
        // add padding for later addition
        _ = self.add_function(buffer);
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
        return bytecode.FunctionIndex.new(@intCast(self.function_buffers.items.len - 1));
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

test "basic compiler" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new("1 +   2 * 2 - 3;", allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try snap.Snap.init(@src(),
        \\function (12 bytes)
        \\    0: push_byte 1
        \\    2: push_byte 2
        \\    4: push_byte 2
        \\    6: mul
        \\    7: add
        \\    8: push_byte 3
        \\    10: sub
        \\    11: ret_main
        \\
        \\
    ).equal_fmt(res);
}

test "let compiler" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let x = 1;
        \\ x + 1;
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try snap.Snap.init(@src(),
        \\function (14 bytes)
        \\    0: push_byte 1
        \\    2: set_global 0 0 0 0
        \\    7: pop
        \\    8: get_global_small 0
        \\    10: push_byte 1
        \\    12: add
        \\    13: ret_main
        \\
        \\
    ).equal_fmt(res);
}

test "condition compiler" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let x = true;
        \\ if (x) 1000 else 2;
    , allocator);
    const prog = try p.parse();
    const res = try compile(prog, allocator);
    try snap.Snap.init(@src(),
        \\function (27 bytes)
        \\    0: true
        \\    1: set_global 0 0 0 0
        \\    6: pop
        \\    7: get_global_small 0
        \\    9: branch 0 0 0 21
        \\    14: push_byte 2
        \\    16: jump 0 0 0 26
        \\    21: push 0 0 3 232
        \\    26: ret_main
        \\
        \\
    ).equal_fmt(res);
}

test "object compiler" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
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
    try snap.Snap.init(@src(),
        \\function (48 bytes)
        \\    0: nil
        \\    1: push_byte 1
        \\    3: string 0 0 0 2
        \\    8: object 0 0 0 3
        \\    13: set_global 0 0 0 0
        \\    18: pop
        \\    19: get_global_small 0
        \\    21: get_field 0 0 0 0
        \\    26: push_byte 1
        \\    28: add
        \\    29: print 0 0 0 1
        \\    34: pop
        \\    35: get_global_small 0
        \\    37: get_field 0 0 0 1
        \\    42: print 0 0 0 1
        \\    47: ret_main
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
        \\class: 0 1
        \\
    ).equal_fmt(res);
}

test "object 2 compiler" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
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
    try snap.Snap.init(@src(),
        \\function (74 bytes)
        \\    0: nil
        \\    1: push_byte 1
        \\    3: string 0 0 0 2
        \\    8: object 0 0 0 3
        \\    13: set_global 0 0 0 0
        \\    18: pop
        \\    19: get_global_small 0
        \\    21: get_field 0 0 0 0
        \\    26: push_byte 1
        \\    28: add
        \\    29: print 0 0 0 1
        \\    34: pop
        \\    35: get_global_small 0
        \\    37: get_field 0 0 0 1
        \\    42: print 0 0 0 1
        \\    47: pop
        \\    48: push_byte 2
        \\    50: get_global_small 0
        \\    52: set_field 0 0 0 0
        \\    57: pop
        \\    58: get_global_small 0
        \\    60: get_field 0 0 0 0
        \\    65: push_byte 1
        \\    67: add
        \\    68: print 0 0 0 1
        \\    73: ret_main
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
        \\class: 0 1
        \\
    ).equal_fmt(res);
}

test "object 3 compiler" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
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
    try snap.Snap.init(@src(),
        \\function (71 bytes)
        \\    0: nil
        \\    1: push_byte 1
        \\    3: string 0 0 0 2
        \\    8: closure 0 0 0 1 0 0 0 0
        \\    17: object 0 0 0 4
        \\    22: set_global 0 0 0 0
        \\    27: pop
        \\    28: push_byte 1
        \\    30: get_global_small 0
        \\    32: methodcall 0 0 0 3
        \\    37: pop
        \\    38: get_global_small 0
        \\    40: get_field 0 0 0 1
        \\    45: print 0 0 0 1
        \\    50: pop
        \\    51: push_byte 2
        \\    53: get_global_small 0
        \\    55: set_field 0 0 0 0
        \\    60: pop
        \\    61: push_byte 2
        \\    63: get_global_small 0
        \\    65: methodcall 0 0 0 3
        \\    70: ret_main
        \\
        \\function (16 bytes)
        \\    0: get_small 0
        \\    2: get_field 0 0 0 0
        \\    7: get_small 1
        \\    9: add
        \\    10: print 0 0 0 1
        \\    15: ret
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
        \\class (17 bytes)
        \\class: 0 1 3
        \\
    ).equal_fmt(res);
}

test "linked list" {
    const Parser = @import("parser.zig").Parser;
    const snap = @import("snap.zig");
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
    try snap.Snap.init(@src(),
        \\function (167 bytes)
        \\    0: closure 0 0 0 1 0 0 0 0
        \\    9: set_global 0 0 0 0
        \\    14: pop
        \\    15: closure 0 0 0 6 0 0 0 0
        \\    24: set_global 0 0 0 1
        \\    29: pop
        \\    30: get_global_small 1
        \\    32: call
        \\    33: set_global 0 0 0 2
        \\    38: pop
        \\    39: push_byte 0
        \\    41: set_global 0 0 0 3
        \\    46: pop
        \\    47: nil
        \\    48: get_global_small 3
        \\    50: push_byte 100
        \\    52: lt
        \\    53: branch 0 0 0 63
        \\    58: jump 0 0 0 158
        \\    63: pop
        \\    64: push_byte 1
        \\    66: get_global_small 2
        \\    68: methodcall 0 0 0 4
        \\    73: pop
        \\    74: push_byte 2
        \\    76: get_global_small 2
        \\    78: methodcall 0 0 0 4
        \\    83: pop
        \\    84: push_byte 3
        \\    86: get_global_small 2
        \\    88: methodcall 0 0 0 4
        \\    93: pop
        \\    94: push_byte 42
        \\    96: get_global_small 2
        \\    98: methodcall 0 0 0 5
        \\    103: pop
        \\    104: get_global_small 2
        \\    106: methodcall 0 0 0 6
        \\    111: print 0 0 0 1
        \\    116: pop
        \\    117: get_global_small 2
        \\    119: methodcall 0 0 0 6
        \\    124: print 0 0 0 1
        \\    129: pop
        \\    130: get_global_small 2
        \\    132: methodcall 0 0 0 6
        \\    137: print 0 0 0 1
        \\    142: pop
        \\    143: get_global_small 3
        \\    145: push_byte 1
        \\    147: add
        \\    148: set_global 0 0 0 3
        \\    153: jump 0 0 0 48
        \\    158: pop
        \\    159: get_global_small 2
        \\    161: methodcall 0 0 0 7
        \\    166: ret_main
        \\
        \\function (10 bytes)
        \\    0: nil
        \\    1: get_small 0
        \\    3: nil
        \\    4: object 0 0 0 2
        \\    9: ret
        \\
        \\function (96 bytes)
        \\    0: get_small 0
        \\    2: get_field 0 0 0 3
        \\    7: nil
        \\    8: eq
        \\    9: branch 0 0 0 83
        \\    14: get_small 0
        \\    16: get_field 0 0 0 3
        \\    21: set 0 0 0 2
        \\    26: pop
        \\    27: nil
        \\    28: get_small 2
        \\    30: get_field 0 0 0 1
        \\    35: nil
        \\    36: ne
        \\    37: branch 0 0 0 47
        \\    42: jump 0 0 0 65
        \\    47: pop
        \\    48: get_small 2
        \\    50: get_field 0 0 0 1
        \\    55: set 0 0 0 2
        \\    60: jump 0 0 0 28
        \\    65: pop
        \\    66: get_small 1
        \\    68: get_global_small 0
        \\    70: call
        \\    71: get_small 2
        \\    73: set_field 0 0 0 1
        \\    78: jump 0 0 0 95
        \\    83: get_small 1
        \\    85: get_global_small 0
        \\    87: call
        \\    88: get_small 0
        \\    90: set_field 0 0 0 3
        \\    95: ret
        \\
        \\function (41 bytes)
        \\    0: get_small 0
        \\    2: get_field 0 0 0 3
        \\    7: set 0 0 0 2
        \\    12: pop
        \\    13: get_small 1
        \\    15: get_global_small 0
        \\    17: call
        \\    18: get_small 0
        \\    20: set_field 0 0 0 3
        \\    25: pop
        \\    26: get_small 2
        \\    28: get_small 0
        \\    30: get_field 0 0 0 3
        \\    35: set_field 0 0 0 1
        \\    40: ret
        \\
        \\function (156 bytes)
        \\    0: get_small 0
        \\    2: get_field 0 0 0 3
        \\    7: nil
        \\    8: eq
        \\    9: branch 0 0 0 154
        \\    14: get_small 0
        \\    16: get_field 0 0 0 3
        \\    21: get_field 0 0 0 1
        \\    26: nil
        \\    27: eq
        \\    28: branch 0 0 0 119
        \\    33: get_small 0
        \\    35: get_field 0 0 0 3
        \\    40: set 0 0 0 1
        \\    45: pop
        \\    46: nil
        \\    47: get_small 1
        \\    49: get_field 0 0 0 1
        \\    54: get_field 0 0 0 1
        \\    59: nil
        \\    60: ne
        \\    61: branch 0 0 0 71
        \\    66: jump 0 0 0 89
        \\    71: pop
        \\    72: get_small 1
        \\    74: get_field 0 0 0 1
        \\    79: set 0 0 0 1
        \\    84: jump 0 0 0 47
        \\    89: pop
        \\    90: get_small 1
        \\    92: get_field 0 0 0 0
        \\    97: set 0 0 0 2
        \\    102: pop
        \\    103: nil
        \\    104: get_small 1
        \\    106: set_field 0 0 0 1
        \\    111: pop
        \\    112: get_small 2
        \\    114: jump 0 0 0 149
        \\    119: get_small 0
        \\    121: get_field 0 0 0 3
        \\    126: set 0 0 0 3
        \\    131: pop
        \\    132: get_small 0
        \\    134: get_field 0 0 0 3
        \\    139: nil
        \\    140: eq
        \\    141: pop
        \\    142: get_small 1
        \\    144: get_field 0 0 0 0
        \\    149: jump 0 0 0 155
        \\    154: nil
        \\    155: ret
        \\
        \\function (60 bytes)
        \\    0: get_small 0
        \\    2: get_field 0 0 0 3
        \\    7: set 0 0 0 1
        \\    12: pop
        \\    13: nil
        \\    14: get_small 1
        \\    16: nil
        \\    17: ne
        \\    18: branch 0 0 0 28
        \\    23: jump 0 0 0 59
        \\    28: pop
        \\    29: get_small 1
        \\    31: get_field 0 0 0 0
        \\    36: print 0 0 0 1
        \\    41: pop
        \\    42: get_small 1
        \\    44: get_field 0 0 0 1
        \\    49: set 0 0 0 1
        \\    54: jump 0 0 0 14
        \\    59: ret
        \\
        \\function (44 bytes)
        \\    0: nil
        \\    1: nil
        \\    2: closure 0 0 0 2 0 0 0 0
        \\    11: closure 0 0 0 3 0 0 0 0
        \\    20: closure 0 0 0 4 0 0 0 0
        \\    29: closure 0 0 0 5 0 0 0 0
        \\    38: object 0 0 0 8
        \\    43: ret
        \\
        \\string (8 bytes)
        \\string: val
        \\
        \\string (9 bytes)
        \\string: next
        \\
        \\class (13 bytes)
        \\class: 0 1
        \\string (9 bytes)
        \\string: head
        \\
        \\string (11 bytes)
        \\string: append
        \\
        \\string (12 bytes)
        \\string: prepend
        \\
        \\string (8 bytes)
        \\string: pop
        \\
        \\string (10 bytes)
        \\string: debug
        \\
        \\class (25 bytes)
        \\class: 3 4 5 6 7
        \\
    ).equal_fmt(res);
}
