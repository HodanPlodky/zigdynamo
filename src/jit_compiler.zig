const std = @import("std");
const bytecode = @import("bytecode.zig");
const bc_interpret = @import("bc_interpreter.zig");
const runtime = @import("runtime.zig");

const BREAKPOINT: bool = false;
const DGB: bool = false;

pub const JitError = error{
    CanOnlyCompileFn,
    UnsupportedBcInstruction,
    OutOfMem,
};

pub const JitFunction = struct {
    code: [*]u8,

    pub fn run(self: *const JitFunction, state: *const JitState) void {
        const f: *const fn (*const JitState) callconv(.C) void = @alignCast(@ptrCast(self.code));
        f(state);
    }
};

/// struct representing state of interpreter
/// for jit compiled code this is necessary because
/// normal structs in zig do not gurantee order of
/// fields and jit code needs different set of fields
/// then the bytecode interpreter
pub const JitState = extern struct {
    intepreter: *const bc_interpret.Interpreter,
    stack: *const bc_interpret.Stack,
    env: *const bc_interpret.Environment,
    gc: *const bc_interpret.GC,

    // basic
    alloc_stack: *const fn (*bc_interpret.Stack, usize) callconv(.C) void,

    // objects handle
    create_closure: *const fn (noalias *bc_interpret.Interpreter, u64, u64) callconv(.C) void,
    create_object: *const fn (noalias *bc_interpret.Interpreter, bytecode.ConstantIndex) callconv(.C) void,
    get_field: *const fn (noalias *bc_interpret.Interpreter, bytecode.ConstantIndex) callconv(.C) void,
    set_field: *const fn (noalias *bc_interpret.Interpreter, bytecode.ConstantIndex) callconv(.C) void,

    // calls
    call: *const fn (noalias *bc_interpret.Interpreter, noalias *const JitState) callconv(.C) void,
    method_call: *const fn (noalias *bc_interpret.Interpreter, noalias *const JitState, bytecode.ConstantIndex) callconv(.C) void,
    print: *const fn (noalias *bc_interpret.Interpreter, arg_count: u64) callconv(.C) void,

    // debug
    dbg: *const fn (runtime.Value) callconv(.C) void,
    dbg_raw: *const fn (u64) callconv(.C) void,
    dbg_inst: *const fn (u64) callconv(.C) void,

    // panics
    binop_panic: *const fn (runtime.Value, runtime.Value) callconv(.C) void,
    if_condition_panic: *const fn () callconv(.C) void,
    string_panic: *const fn () callconv(.C) void,

    pub fn get_offset(comptime field_name: []const u8) u32 {
        return @offsetOf(JitState, field_name);
    }
};

const GPR64 = enum(u4) {
    rax = 0,
    rbx = 3,
    rcx = 1,
    rdx = 2,
    rsi = 6,
    rdi = 7,
    rsp = 4,
    rbp = 5,

    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
};

const Scale = enum(u2) {
    scale1 = 0b00,
    scale2 = 0b01,
    scale4 = 0b10,
    scale8 = 0b11,

    pub fn from_size(size: usize) Scale {
        return switch (size) {
            0 => Scale.scale1,
            2 => Scale.scale2,
            4 => Scale.scale4,
            8 => Scale.scale8,
            else => @panic("invalid scale"),
        };
    }
};

const PanicTable = struct {
    binop_panic: usize = 0,
    if_condition_panic: usize = 0,
    string_panic: usize = 0,
};

pub const JitCompiler = struct {
    const stack_addr = GPR64.r15;
    const env_addr = GPR64.r14;
    const intepret_addr = GPR64.r13;
    const state_addr = GPR64.rbx;

    code_slice: []u8,
    code_ptr: usize,
    pc: usize,
    bytecode: []const u8,
    panic_table: PanicTable,
    scratch_arena: std.heap.ArenaAllocator,

    pub fn init(code_buffer_size: usize) JitCompiler {
        const addr = std.os.linux.mmap(
            null,
            code_buffer_size,
            std.os.linux.PROT.READ | std.os.linux.PROT.WRITE,
            std.os.linux.MAP{
                .ANONYMOUS = true,
                .TYPE = std.os.linux.MAP_TYPE.PRIVATE,
            },
            -1,
            0,
        );

        const code: [*]u8 = @ptrFromInt(addr);
        const code_slice = code[0..code_buffer_size];

        var res = JitCompiler{
            .code_slice = code_slice,
            .code_ptr = 0,
            .pc = undefined,
            .bytecode = undefined,
            .panic_table = .{},
            .scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
        res.init_panic_handlers() catch unreachable;
        return res;
    }

    /// emit panic calls for and populate panic call table
    fn init_panic_handlers(self: *JitCompiler) !void {
        try self.emit_panic_handler_with_prolog("binop_panic", JitCompiler.bin_op_panic_prolog);
        try self.emit_panic_handler("if_condition_panic");
        try self.emit_panic_handler("string_panic");
    }

    fn bin_op_panic_prolog(self: *JitCompiler) !void {
        try self.mov_reg_reg(GPR64.rdi, GPR64.r8);
        try self.mov_reg_reg(GPR64.rsi, GPR64.r9);
    }

    fn emit_panic_handler(self: *JitCompiler, comptime panic_name: []const u8) !void {
        @field(self.panic_table, panic_name) = self.code_ptr;
        try self.call(panic_name);
    }

    fn emit_panic_handler_with_prolog(self: *JitCompiler, comptime panic_name: []const u8, comptime prolog: fn (*JitCompiler) JitError!void) !void {
        @field(self.panic_table, panic_name) = self.code_ptr;
        try prolog(self);
        try self.call(panic_name);
    }

    /// creates jump depending on current flags such that if the test
    /// return false then in jumps to panic
    fn emit_panic(self: *JitCompiler, comptime panic_name: []const u8) !void {
        const panic_offset = @field(self.panic_table, panic_name);
        std.debug.assert(self.code_ptr > panic_offset);
        var diff: usize = self.code_ptr + 2 - panic_offset;

        // we got signed so thats why size - 1
        if (diff < (1 << 7)) {
            const diff_u8: u8 = @intCast(diff);
            const jmp_val: u8 = ~diff_u8 + 1;
            const jmp_slice: [2]u8 = .{ 0x75, jmp_val };
            try self.emit_slice(jmp_slice[0..]);
            return;
        }
        diff += 4;
        if (diff < (1 << 31)) {
            // jne 401000 <tmp>
            // 0f 85 7e 8c ed ff
            const diff_u32: u32 = @intCast(diff);
            const jmp_val: u32 = ~diff_u32 + 1;
            const jmp_slice: [6]u8 = .{
                0x0f,
                0x85,
                @intCast(jmp_val & 0xff),
                @intCast((jmp_val >> 8) & 0xff),
                @intCast((jmp_val >> 16) & 0xff),
                @intCast((jmp_val >> 24) & 0xff),
            };

            try self.emit_slice(jmp_slice[0..]);
            return;
        }
        @panic("cannot do panic jump");
    }

    pub fn compile_fn(self: *JitCompiler, function: bytecode.Constant) JitError!JitFunction {
        switch (function.get_type()) {
            bytecode.ConstantType.function => self.pc = 0,
            else => return JitError.CanOnlyCompileFn,
        }

        const jit_offset = bytecode.Constant.function_header_size - 4;
        const jit_state: u32 = function.get_u32(@intCast(jit_offset));
        if (jit_state != 0) {
            return JitFunction{ .code = @ptrCast(&self.code_slice[jit_state]) };
        }

        _ = std.os.linux.mprotect(self.code_slice.ptr, self.code_slice.len, std.os.linux.PROT.WRITE | std.os.linux.PROT.READ);

        var offsets = std.ArrayList(u32).initCapacity(self.scratch_arena.allocator(), function.get_size() + 4) catch unreachable;
        var jumps = std.ArrayList(u32).initCapacity(self.scratch_arena.allocator(), function.get_size() + 4) catch unreachable;

        // append dummy data for header
        offsets.appendNTimesAssumeCapacity(0xfefefefe, bytecode.Constant.function_header_size);

        // align
        self.code_ptr = (self.code_ptr + 15) & 0xffffffff_fffffff0;

        const start = self.code_ptr;

        // ignore the header of function
        self.bytecode = function.get_slice()[(bytecode.Constant.function_header_size)..];

        if (BREAKPOINT) {
            try self.emit_break();
        }
        try self.emit_prolog();

        while (self.pc < self.bytecode.len) {
            try self.compile_bytecode_inst(&offsets, &jumps);
        }

        self.fix_jumps(jumps.items, offsets.items);

        const ok = self.scratch_arena.reset(std.heap.ArenaAllocator.ResetMode.retain_capacity);
        std.debug.assert(ok);

        // set jit offset
        function.data[jit_offset] = @intCast((start >> 24) & 0xff);
        function.data[jit_offset + 1] = @intCast((start >> 16) & 0xff);
        function.data[jit_offset + 2] = @intCast((start >> 8) & 0xff);
        function.data[jit_offset + 3] = @intCast(start & 0xff);

        _ = std.os.linux.mprotect(self.code_slice.ptr, self.code_slice.len, std.os.linux.PROT.EXEC | std.os.linux.PROT.READ);
        return JitFunction{ .code = @ptrCast(&self.code_slice[start]) };
    }

    fn compile_bytecode_inst(self: *JitCompiler, offsets: *std.ArrayList(u32), jumps: *std.ArrayList(u32)) !void {
        const inst = self.read_inst();
        const offset: u32 = @intCast(self.code_ptr);
        offsets.appendNTimesAssumeCapacity(offset, inst.get_extrabytes() + 1);

        if (DGB) {
            const inst_raw: u64 = @intFromEnum(inst);
            try self.set_reg_64(GPR64.rdi, inst_raw);
            try self.call("dbg_inst");
        }
        switch (inst) {
            bytecode.Instruction.pop => {
                try self.stack_pop();
            },
            bytecode.Instruction.ret => {
                try self.env_pop_locals();
                try self.emit_epilog();

                // return
                try self.emit_byte(0xc3);
            },

            bytecode.Instruction.push_byte => {
                const number: u32 = @intCast(self.bytecode[self.pc]);
                const value = runtime.Value.new_num(number);

                try self.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);

                self.pc += 1;
            },
            bytecode.Instruction.push => {
                const number = self.read_u32();

                const value = runtime.Value.new_num(number);

                try self.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.nil => {
                const value = runtime.Value.new_nil();
                try self.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.true => {
                const value = runtime.Value.new_true();
                try self.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.false => {
                const value = runtime.Value.new_false();
                try self.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.add => {
                try self.handle_binop_simple(0x1);
            },
            bytecode.Instruction.sub => {
                try self.handle_binop_simple(0x29);
            },
            bytecode.Instruction.mul => {
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler) !void {
                        // shr r9, 0x20
                        const shift_slice: [4]u8 = .{ 0x49, 0xc1, 0xe9, 0x20 };
                        try comp.emit_slice(shift_slice[0..]);
                        // imul r8, r9
                        const imul_slice: [4]u8 = .{ 0x4d, 0x0f, 0xaf, 0xc1 };
                        try comp.emit_slice(imul_slice[0..]);
                    }
                }.f);
            },
            bytecode.Instruction.div => {
                unreachable;
            },
            bytecode.Instruction.gt => {
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler) !void {
                        // xor rdi, rdi (opcode 0x31)
                        try comp.emit_basic_reg(0x31, GPR64.rdi, GPR64.rdi);

                        // cmp r9, r8
                        // opcode 0x39, cmp r/m64 reg
                        try comp.emit_basic_reg(0x39, GPR64.r8, GPR64.r9);

                        // adc rdi,0x8
                        // add with carry
                        // 48 83 d7 05
                        const adc_slice: [4]u8 = .{ 0x48, 0x83, 0xd7, 0x08 };
                        try comp.emit_slice(adc_slice[0..]);

                        try comp.mov_reg_reg(GPR64.r8, GPR64.rdi);
                    }
                }.f);
            },
            bytecode.Instruction.lt => {
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler) !void {
                        // xor rdi, rdi (opcode 0x31)
                        try comp.emit_basic_reg(0x31, GPR64.rdi, GPR64.rdi);

                        // here is change from gt!
                        // cmp r8, r8
                        // opcode 0x39, cmp r/m64 reg
                        try comp.emit_basic_reg(0x39, GPR64.r9, GPR64.r8);

                        // adc rdi,0x8
                        // add with carry
                        // 48 83 d7 05
                        const adc_slice: [4]u8 = .{ 0x48, 0x83, 0xd7, 0x08 };
                        try comp.emit_slice(adc_slice[0..]);

                        try comp.mov_reg_reg(GPR64.r8, GPR64.rdi);
                    }
                }.f);
            },
            bytecode.Instruction.eq => {
                try self.get_stack_to_reg(GPR64.r8, 0);
                try self.get_stack_to_reg(GPR64.r9, 1);

                // xor rsi, rsi
                // technically this xor is not opcode r/m64 reg
                // but opcode reg r/m64. However both are the same
                try self.emit_basic_reg(0x33, GPR64.rsi, GPR64.rsi);
                // cmp r8, r9
                // same issue as xor but this time we only care
                // about the equilaty so yeah does not matter
                try self.emit_basic_reg(0x3b, GPR64.r9, GPR64.r8);
                // sete sil (lower bytes of rsi)
                // 40 0f 94 c6
                const sete_slice: [4]u8 = .{ 0x40, 0x0f, 0x94, 0xc6 };
                try self.emit_slice(sete_slice[0..]);
                // add rsi, (false value - 8)
                // 48 83 c6 08
                const add_slice: [4]u8 = .{ 0x48, 0x83, 0xc6, @intFromEnum(runtime.ValueType.false) };
                try self.emit_slice(add_slice[0..]);

                try self.stack_pop();
                try self.stack_set_top(GPR64.rsi);
            },
            bytecode.Instruction.ne => {
                try self.get_stack_to_reg(GPR64.r8, 0);
                try self.get_stack_to_reg(GPR64.r9, 1);

                // xor rsi, rsi
                // technically this xor is not opcode r/m64 reg
                // but opcode reg r/m64. However both are the same
                try self.emit_basic_reg(0x33, GPR64.rsi, GPR64.rsi);
                // cmp r8, r9
                // same issue as xor but this time we only care
                // about the equilaty so yeah does not matter
                try self.emit_basic_reg(0x3b, GPR64.r9, GPR64.r8);
                // setne sil (lower bytes of rsi)
                // 40 0f 95 c6
                const sete_slice: [4]u8 = .{ 0x40, 0x0f, 0x95, 0xc6 };
                try self.emit_slice(sete_slice[0..]);
                // add rsi, (false value - 8)
                // 48 83 c6 08
                const add_slice: [4]u8 = .{ 0x48, 0x83, 0xc6, @intFromEnum(runtime.ValueType.false) };
                try self.emit_slice(add_slice[0..]);

                try self.stack_pop();
                try self.stack_set_top(GPR64.rsi);
            },
            bytecode.Instruction.jump => {
                // opcode for jmp rel32
                try self.emit_byte(0xe9);
                // jumps will point to number it self
                const jump_offset: u32 = @intCast(self.code_ptr);
                jumps.appendAssumeCapacity(jump_offset);

                // store original value of jump for fix up after
                const jump_pc = self.bytecode[self.pc..(self.pc + 4)];
                try self.emit_slice(jump_pc);
                self.pc += 4;
            },
            bytecode.Instruction.branch => {
                try self.get_stack_to_reg(GPR64.rax, 0);
                try self.stack_pop();

                const true_byte: u8 = @intFromEnum(runtime.ValueType.true);

                // tmp store
                try self.mov_reg_reg(GPR64.r8, GPR64.rax);

                // check if the value is even bool
                // and al, 0x8 => 0x8 highest bit of tag only used by booleans
                // 24 ib
                // cmp al, 0x8
                // 3c ib
                const and_slice: [2]u8 = .{ 0x24, 0x8 };
                try self.emit_slice(and_slice[0..]);
                const cmp_slice: [2]u8 = .{ 0x3c, 0x8 };
                try self.emit_slice(cmp_slice[0..]);
                try self.emit_panic("if_condition_panic");

                // tmp restore
                try self.mov_reg_reg(GPR64.rax, GPR64.r8);

                // cmp eax, <true_byte>
                try self.emit_byte(0x3d);
                const true_slice: [4]u8 = .{ true_byte, 0, 0, 0 };
                try self.emit_slice(true_slice[0..]);

                // opcode for jmp rel32
                const jump_slice: [2]u8 = .{ 0x0f, 0x84 };
                try self.emit_slice(jump_slice[0..]);
                const jump_offset: u32 = @intCast(self.code_ptr);
                jumps.appendAssumeCapacity(jump_offset);
                // store original value of jump for fix up after
                const jump_pc = self.bytecode[self.pc..(self.pc + 4)];
                try self.emit_slice(jump_pc);
                self.pc += 4;
            },
            bytecode.Instruction.get_small => {
                const index: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load current ptr into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                // load val from index
                try self.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rcx, GPR64.rax, index * 8);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.get => {
                const index: u32 = self.read_u32();

                // load current ptr into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                // load val from index
                try self.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rcx, GPR64.rax, index * 8);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.set_small => {
                const number: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load top of stack
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load current ptr into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                try self.set_to_index64(Scale.scale8, GPR64.rcx, GPR64.rax, number * 8, GPR64.rbp);
            },
            bytecode.Instruction.set => {
                const number = self.read_u32();

                // load top of stack
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load current ptr into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                try self.set_to_index64(Scale.scale8, GPR64.rcx, GPR64.rax, number * 8, GPR64.rbp);
            },
            bytecode.Instruction.get_global_small => {
                //try self.emit_break();
                const index: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load buffer into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.set_reg_64(GPR64.rcx, @intCast(index));
                // load val from index
                try self.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.get_global => {
                const index: u32 = self.read_u32();

                // load buffer into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.set_reg_64(GPR64.rcx, @intCast(index));
                // load val from index
                try self.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.set_global => {
                const index: u32 = self.read_u32();

                // load value
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load buffer into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.set_reg_64(GPR64.rcx, @intCast(index));

                try self.set_to_index64(Scale.scale8, GPR64.rax, GPR64.rcx, 0, GPR64.rbp);
            },
            bytecode.Instruction.set_global_small => {
                const index: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load value
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load buffer into the rax
                try self.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.set_reg_64(GPR64.rcx, @intCast(index));

                try self.set_to_index64(Scale.scale8, GPR64.rax, GPR64.rcx, 0, GPR64.rbp);
            },
            bytecode.Instruction.call => {
                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.mov_reg_reg(GPR64.rsi, state_addr);
                try self.vzeroupper();
                try self.call("call");
            },
            bytecode.Instruction.print => {
                const arg_count = self.read_u32();

                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.set_reg_64(GPR64.rsi, @intCast(arg_count));

                try self.call("print");
            },
            bytecode.Instruction.string => {
                const const_idx = self.read_u32();
                const string = runtime.Value.new_string(const_idx);
                try self.set_reg_64(GPR64.rbp, string.data);

                // load bytecode.constants.ptr
                try self.mov_from_struct_64(
                    GPR64.rax,
                    intepret_addr,
                    @offsetOf(bc_interpret.Interpreter, "bytecode") + @offsetOf(bytecode.Bytecode, "constants"),
                );

                // load constant
                try self.set_reg_64(GPR64.rcx, @intCast(const_idx));
                try self.mov_index_access64(GPR64.rax, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                // cmp BYTE PTR [rax+0x4], <string_type> (0x2)
                // 80 78 04 02
                const cmp_slice: [4]u8 = .{
                    0x80,
                    0x78,
                    0x04,
                    @intFromEnum(bytecode.ConstantType.string),
                };
                try self.emit_slice(cmp_slice[0..]);

                try self.emit_panic("string_panic");

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.closure => {
                const constant_idx: u64 = self.read_u32();
                const unbound_count: u64 = self.read_u32();
                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.set_reg_64(GPR64.rsi, constant_idx);
                try self.set_reg_64(GPR64.rdx, unbound_count);

                try self.call("create_closure");
            },
            bytecode.Instruction.object => {
                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.set_reg_from_u32_bc(GPR64.rsi);

                try self.call("create_object");
            },
            bytecode.Instruction.get_field => {
                const index: u64 = @intCast(self.read_u32());
                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.set_reg_64(GPR64.rsi, index);
                try self.call("get_field");
            },
            bytecode.Instruction.set_field => {
                const index: u64 = @intCast(self.read_u32());
                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.set_reg_64(GPR64.rsi, index);

                try self.call("set_field");
            },
            bytecode.Instruction.methodcall => {
                const index: u64 = @intCast(self.read_u32());
                try self.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.mov_reg_reg(GPR64.rsi, state_addr);
                try self.set_reg_64(GPR64.rdx, index);

                try self.call("method_call");
            },
            bytecode.Instruction.dup => {
                try self.get_stack_to_reg(GPR64.rbp, 0);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.ret_main => {
                // only support jiting normal
                // function but ret_main should only
                // occur in main function
                return JitError.UnsupportedBcInstruction;
            },
        }
    }

    fn handle_binop_simple(self: *JitCompiler, comptime opcode: u8) !void {
        try self.handle_binop(struct {
            fn f(comp: *JitCompiler) !void {
                try comp.emit_basic_reg(opcode, GPR64.r9, GPR64.r8);
            }
        }.f);
    }

    fn handle_binop(self: *JitCompiler, comptime oper: fn (*JitCompiler) JitError!void) !void {
        try self.get_stack_to_reg(GPR64.r9, 0);
        try self.get_stack_to_reg(GPR64.r8, 1);

        // check lower bits
        try self.mov_reg_reg(GPR64.rdi, GPR64.r8);
        // or rdi, r9
        // or r/m64 reg
        try self.emit_basic_reg(0x09, GPR64.r9, GPR64.rdi);
        // test dil,0x7 (dil lowest 8 bit of rdi)
        // 40 f6 c7 07
        const test_slice: [4]u8 = .{ 0x40, 0xf6, 0xc7, 0x07 };
        try self.emit_slice(test_slice[0..]);

        // handle cond
        try self.emit_panic("binop_panic");

        try oper(self);

        try self.stack_pop();
        try self.stack_set_top(GPR64.r8);
    }

    fn read_inst(self: *JitCompiler) bytecode.Instruction {
        const res: bytecode.Instruction = @enumFromInt(self.bytecode[self.pc]);
        self.pc += 1;
        return res;
    }

    fn fix_jumps(self: *JitCompiler, jumps: []const u32, offsets: []const u32) void {
        for (jumps) |jump| {
            // read pc val
            const jump_idx: usize = @intCast(jump);
            var orig_pc: usize = 0;

            // warning in bc I have different endianess
            // then in the asm because fu future me
            orig_pc |= self.code_slice[jump_idx];
            orig_pc <<= 8;
            orig_pc |= self.code_slice[jump_idx + 1];
            orig_pc <<= 8;
            orig_pc |= self.code_slice[jump_idx + 2];
            orig_pc <<= 8;
            orig_pc |= self.code_slice[jump_idx + 3];
            // look up in offsets
            const addr = offsets[orig_pc];
            // compute rel
            const rel: u32 = if (addr > jump)
                addr - (jump + 4)
            else
                ~((jump + 4) - addr) + 1;
            // set rel jump
            self.code_slice[jump_idx] = @intCast(rel & 0xff);
            self.code_slice[jump_idx + 1] = @intCast((rel >> 8) & 0xff);
            self.code_slice[jump_idx + 2] = @intCast((rel >> 16) & 0xff);
            self.code_slice[jump_idx + 3] = @intCast((rel >> 24) & 0xff);
        }
    }

    //
    // State helper
    //

    /// load value from JIT state based on field name of the value
    fn mov_from_jit_state(self: *JitCompiler, dst: GPR64, comptime field_name: []const u8) !void {
        const offset = comptime JitState.get_offset(field_name);
        try self.mov_from_jit_state_offset(dst, offset);
    }

    /// emits instruction for moving the 64bit value from JIT state into
    /// 64bit register, this assumes that the pointer to state is stored in rbx
    fn mov_from_jit_state_offset(self: *JitCompiler, to_reg: GPR64, offset: u32) !void {
        try self.mov_from_struct_64(to_reg, state_addr, offset);
    }

    //
    // Calls
    //

    fn call(self: *JitCompiler, comptime function_name: []const u8) !void {
        try self.mov_from_jit_state(GPR64.rax, function_name);
        try self.call_from_rax();
    }

    fn call_from_rax(self: *JitCompiler) !void {
        // call rax
        const slice: [2]u8 = .{ 0xff, 0xd0 };
        try self.emit_slice(slice[0..]);
    }

    //
    // Stack helpers
    //

    /// clobers rax and rcx
    fn get_stack_to_reg(self: *JitCompiler, dst: GPR64, offset: u8) !void {
        // load len
        try self.mov_from_struct_64(GPR64.rcx, stack_addr, 8);

        // load stack ptr
        try self.deref_ptr(GPR64.rax, stack_addr);

        // load value from top of stack with offset
        // mov reg, [rax + rcx*8 - offset]
        // offset is calculated with two's complement
        const scale = Scale.from_size(@sizeOf(runtime.Value));
        try self.mov_index_access64(dst, scale, GPR64.rax, GPR64.rcx, @intCast((~((offset + 1) * @sizeOf(runtime.Value))) + 1));
    }

    /// clobers rax
    fn stack_pop(self: *JitCompiler) !void {
        const stack_addr_val: u8 = @intFromEnum(stack_addr);

        const len_offset = 8;
        // dec QWORD PTR [stack_addr+0x60]
        // 49 ff 4f 60
        // 49 = REX.W | B-bit = high stack_addr
        // ff = opcode ????
        // 4f = modrm = 01_001_[lower_stack_addr]
        // 60 = offset
        const rex = 0x48 | ((stack_addr_val & 0x8) >> 3);
        const modrm = 0b01_001_000 | (stack_addr_val & 0x7);
        const dec_slice: [4]u8 = .{ rex, 0xff, modrm, len_offset };
        try self.emit_slice(dec_slice[0..]);
    }

    fn stack_push(self: *JitCompiler, src: GPR64) !void {
        std.debug.assert(src != GPR64.rax);
        std.debug.assert(src != GPR64.rcx);
        std.debug.assert(src != GPR64.rdi);
        std.debug.assert(src != GPR64.rsi);
        try self.vzeroupper();

        try self.mov_reg_reg(GPR64.rdi, stack_addr);
        // load len
        try self.mov_from_struct_64(GPR64.rsi, stack_addr, 0x8);

        // inc rsi
        // 48 ff c6 why the fuck it is also 0xff ????
        const inc_slice: [3]u8 = .{ 0x48, 0xff, 0xc6 };
        try self.emit_slice(inc_slice[0..]);

        // store new len
        // mov [r15 + 0x8], rsi (r15 is stack addr)
        // 49 89 77 08
        const len_store_slice: [4]u8 = .{ 0x49, 0x89, 0x77, 0x08 };
        try self.emit_slice(len_store_slice[0..]);

        // check capacity and jump over call if
        // it is not necessary

        // load cap
        const cap_offset = @offsetOf(std.ArrayList(runtime.Value), "capacity");
        try self.mov_from_struct_64(GPR64.rcx, stack_addr, cap_offset);

        // cmp rsi, rcx
        // 48 39 ce
        const cmp_slice: [3]u8 = .{ 0x48, 0x39, 0xce };
        try self.emit_slice(cmp_slice[0..]);

        // jle <after_alloc_stack>
        // 7e 09
        const jump_slice: [2]u8 = .{ 0x7e, 0x09 };
        try self.emit_slice(jump_slice[0..]);

        try self.call("alloc_stack");

        try self.stack_set_top(src);

        // do dbg
        if (DGB) {
            try self.get_stack_to_reg(GPR64.rdi, 0);
            try self.call("dbg");
        }
    }

    /// clobers rax, rcx
    fn stack_set_top(self: *JitCompiler, src: GPR64) !void {
        std.debug.assert(src != GPR64.rax);
        std.debug.assert(src != GPR64.rcx);
        const reg_val: u8 = @intFromEnum(src);

        // load len
        try self.mov_from_struct_64(GPR64.rcx, stack_addr, 8);

        // load stack ptr
        try self.deref_ptr(GPR64.rax, stack_addr);

        // mov QWORD PTR [rax+rcx*8-0x8],r8
        // 4c 89 44 c8 f8
        // 4c = REX => set W R B
        // 89 = opcode
        // 44 = modrm = 01_000_100
        // c8 = sib = 11_001_000 = scale8 | rcx | rax
        // f8 = -0x8 = size of Value
        const rex = 0x48 | ((reg_val & 0x8) >> 1);
        const modrm = 0x40 | ((reg_val & 0x7) << 3) | 0b100;
        const scale = Scale.from_size(@sizeOf(runtime.Value));
        const scale_bits: u8 = @intFromEnum(scale);
        const sib = (scale_bits << 6) | 0b1000;
        const size: u8 = @sizeOf(runtime.Value);
        const offset = ~size + 1;

        const move_slice: [5]u8 = .{ rex, 0x89, modrm, sib, offset };
        try self.emit_slice(move_slice[0..]);
    }

    //
    // env helpers
    //

    fn env_pop_locals(self: *JitCompiler) !void {
        // load local buffer ptr
        try self.mov_from_struct_64(
            GPR64.rax,
            env_addr,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"),
        );

        // load local buffer len
        try self.mov_from_struct_64(
            GPR64.rcx,
            env_addr,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer") + 8,
        );

        // this is done with 32bit so I dont
        // have the machinery to do that
        // mov eax,DWORD PTR [rax+rcx*8-<2 * sizeof(Value)>]
        // 8b 44 c8 f0
        const load_old_fp_slice: [4]u8 = .{ 0x8b, 0x44, 0xc8, 0xf0 };
        try self.emit_slice(load_old_fp_slice[0..]);

        // load current fp again 32bit
        // mov ecx,DWORD PTR [r14+<current_fp_offset>]
        // 41 8b 4e 48
        const load_current_fp_slice: [4]u8 = .{
            0x41,
            0x8b,
            0x4e,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"),
        };
        try self.emit_slice(load_current_fp_slice[0..]);

        // store current ptr to len of buffer
        // mov QWORD PTR [r14+<len offset>],rcx
        // 49 89 4e 18
        const store_len_slice: [4]u8 = .{
            0x49,
            0x89,
            0x4e,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer") + 8,
        };
        try self.emit_slice(store_len_slice[0..]);

        // mov DWORD PTR [r14+<current_ptr>],eax
        // 41 89 46 48
        const store_curent_ptr_slice: [4]u8 = .{
            0x41,
            0x89,
            0x46,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"),
        };
        try self.emit_slice(store_curent_ptr_slice[0..]);
    }

    //
    // Struct access
    //

    fn mov_from_struct_64(self: *JitCompiler, dst: GPR64, base: GPR64, offset: u32) !void {
        // REX.W + 8B /r
        const dst_val: u8 = @intFromEnum(dst);
        const base_val: u8 = @intFromEnum(base);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we set the self
        // ptr reg as rbx and that has highes bit num 0
        try self.emit_byte(0x48 | ((base_val & 0x8) >> 3) | ((dst_val & 0x8) >> 1));

        // opcode
        try self.emit_byte(0x8b);

        // ModRM
        // | mode | reg | r/m |
        // mode will be less then 0b11 => setting it to 0b10 (thats what I saw in wild)
        // only exception would be if the offset would be 0
        // 10 | bottom 3 bits of to_reg | rbx bottom 3 bit (0x2)
        //
        // The reason for the check against 0b101 is that for
        // some ungodly fucking manifested reason the x86 has fucking
        // exeption only for this case in the modrm byte which
        // says it had different purpouse for this value. The
        // reson is different on 64 and 32 bit no less so just check
        // the fucking table
        //
        // I cound have handle it also by emitting one byte but you
        // know what fuck that right now just adding TODO for future fucker
        const mod: u8 = if (offset != 0 or base_val & 0x7 == 0b101)
            0x80
        else
            0x00;

        const to_reg_lower = dst_val & 0x7;
        try self.emit_byte(mod | (to_reg_lower << 3) | (base_val & 0x7));

        // if offset is not zero we have to
        // add it at the end of the instruction
        if (offset != 0 or base_val & 0x7 == 0b101) {
            const offset_bytes: [4]u8 = .{
                @intCast(offset & 0xff),
                @intCast((offset >> 8) & 0xff),
                @intCast((offset >> 16) & 0xff),
                @intCast((offset >> 24) & 0xff),
            };
            try self.emit_slice(offset_bytes[0..]);
        }
    }

    fn mov_index_access64(self: *JitCompiler, dst: GPR64, scale: Scale, base: GPR64, index: GPR64, offset: u32) !void {
        // example:
        //      48 8b 74 d1 f8
        //      REX.W opcode 01_110_100
        //      rcx = 1
        //      rdx = 2
        //      rsi = 6
        //      modrm = 01_11_100
        //      01 = indirect
        //      110 = 6 = rsi
        //      100 = 4 => its sib
        //      sib = 11_010_001
        //      scale = 11 => 8
        //      index = 010 => rdx
        //      base = 001 => rcx
        //      f8 = 11111000 = -8
        // mov rsi,QWORD PTR [rcx+rdx*8-0x8]

        const dst_val: u8 = @intFromEnum(dst);
        var index_value: u8 = @intFromEnum(index);
        var base_value: u8 = @intFromEnum(base);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The R contains highest bit of reg number
        // The X contains highest bit of index number
        // The B contains highest bit of base number
        try self.emit_byte(0x48 | ((dst_val & 0x8) >> 1) | ((index_value & 0x8) >> 2) | ((base_value & 0x8) >> 3));

        // after this I dont need higher bits for index and base
        index_value &= 0x7;
        base_value &= 0x7;

        // opcode
        try self.emit_byte(0x8b);

        const modrm = create_modrm_sib(dst, offset);
        try self.emit_byte(modrm);

        const sib = create_sib(scale, base, index);
        try self.emit_byte(sib);

        try self.emit_offset(offset);
    }

    fn set_to_index64(self: *JitCompiler, scale: Scale, base: GPR64, index: GPR64, offset: u32, src: GPR64) !void {
        // mov QWORD PTR [rax+rcx*8],r9 (mov r/m64, reg)
        // 4c 89 0c c8
        // 4c = REX.W | W, R (from r9)
        // 89 = opcode
        // 0c = modrm 00_001_100
        // c8 = sib 11 001 000 = scale 8 | rcx | rax

        const src_val: u8 = @intFromEnum(src);
        const base_val: u8 = @intFromEnum(base);
        const index_val: u8 = @intFromEnum(index);

        const rex = 0x48 | ((src_val & 0x8) >> 1) | ((index_val & 0x8) >> 2) | ((base_val & 0x8) >> 3);
        try self.emit_byte(rex);

        // opcode
        try self.emit_byte(0x89);

        const modrm = create_modrm_sib(src, offset);
        try self.emit_byte(modrm);

        const sib = create_sib(scale, base, index);
        try self.emit_byte(sib);

        try self.emit_offset(offset);
    }

    fn emit_offset(self: *JitCompiler, offset: u32) !void {
        // if offset is zero it is captured above
        if (offset != 0) {
            if (offset < 256) {
                try self.emit_byte(@intCast(offset));
            } else {
                const offset_bytes: [4]u8 = .{
                    @intCast(offset & 0xff),
                    @intCast((offset >> 8) & 0xff),
                    @intCast((offset >> 16) & 0xff),
                    @intCast((offset >> 24) & 0xff),
                };
                try self.emit_slice(offset_bytes[0..]);
            }
        }
    }

    //
    // Regs movement
    //

    fn mov_reg_reg(self: *JitCompiler, dst: GPR64, src: GPR64) !void {
        // mov rbx, rdi
        // 48 89 fb
        // 48 = REX
        // 89 = opcode (mod r/m64, r)
        // fb = modrm 11_111_011 => mod | src | dst

        // REX | W | R | X | B
        // reg => R, r/m64 => B
        //const rex = 0x48 | ((src_val & 0x8) >> 1) | ((dst_val & 0x8) >> 3);
        const rex = JitCompiler.create_rex(src, dst);
        try self.emit_byte(rex);

        // opcode
        try self.emit_byte(0x89);

        // MODrm
        // mod = 0b11 | lower bits src | lower bits dest
        const modrm = JitCompiler.create_modrm_regs(src, dst);
        try self.emit_byte(modrm);
    }

    fn set_reg_64(self: *JitCompiler, reg: GPR64, value: u64) !void {
        const reg_val: u8 = @intFromEnum(reg);
        // REX.W
        // | 4-bit | W | R | X | B |
        // The R could contain highest bit of reg number
        // The B will never be set since we move immediate
        // and there is no other reg
        try self.emit_byte(0x48 | ((reg_val & 0x8) >> 3));

        // opcode has in it self the lower 3 bits of reg
        // index (0xb8 is base and you add those)
        const base_opcode: u8 = 0xb8;
        const opcode = base_opcode + (reg_val & 0x7);
        try self.emit_byte(opcode);

        const value_bytes: [8]u8 = .{
            @intCast(value & 0xff),
            @intCast((value >> 8) & 0xff),
            @intCast((value >> 16) & 0xff),
            @intCast((value >> 24) & 0xff),
            @intCast((value >> 32) & 0xff),
            @intCast((value >> 40) & 0xff),
            @intCast((value >> 48) & 0xff),
            @intCast((value >> 56) & 0xff),
        };

        try self.emit_slice(value_bytes[0..]);
    }

    fn deref_ptr(self: *JitCompiler, dst: GPR64, src: GPR64) !void {
        // mov rcx,QWORD PTR [rbx]
        // 48 8b 0b
        // 48 = REX
        // 8b = opcode
        // 0b = modrm

        const dest_value: u8 = @intFromEnum(dst);
        const src_value: u8 = @intFromEnum(src);

        // REX
        const rex = 0x48 | ((dest_value & 0x8) >> 1) | ((src_value & 0x8) >> 3);
        try self.emit_byte(rex);

        // opcode
        try self.emit_byte(0x8b);

        const modrm = ((dest_value & 0x7) << 3) | (src_value & 0x7);
        try self.emit_byte(modrm);
    }

    //
    // Prolog and epilog
    //

    fn emit_prolog(self: *JitCompiler) !void {
        // push rax
        try self.emit_byte(0x50);
        // push rbx
        try self.emit_byte(0x53);
        // push r8
        try self.emit_byte(0x41);
        try self.emit_byte(0x50);
        // push r9
        try self.emit_byte(0x41);
        try self.emit_byte(0x51);
        // push r13
        try self.emit_byte(0x41);
        try self.emit_byte(0x55);
        // push r14
        try self.emit_byte(0x41);
        try self.emit_byte(0x56);
        // push r15
        try self.emit_byte(0x41);
        try self.emit_byte(0x57);

        // mov rbx, rdi
        // rbx will store address to state
        try self.mov_reg_reg(state_addr, GPR64.rdi);

        // mov r15, [rbx + <stack offset>]
        // r15 will store stack addr
        try self.mov_from_jit_state(stack_addr, "stack");

        // mov r14, [rbx + <stack offset>]
        // r14 will store env addr
        try self.mov_from_jit_state(env_addr, "env");

        // mov r13, [rbx + <stack offset>]
        // r14 will store env addr
        try self.mov_from_jit_state(intepret_addr, "intepreter");
    }

    fn emit_epilog(self: *JitCompiler) !void {
        // pop r15
        try self.emit_byte(0x41);
        try self.emit_byte(0x5f);
        // pop r14
        try self.emit_byte(0x41);
        try self.emit_byte(0x5e);
        // pop r13
        try self.emit_byte(0x41);
        try self.emit_byte(0x5d);
        // pop r9
        try self.emit_byte(0x41);
        try self.emit_byte(0x59);
        // pop r8
        try self.emit_byte(0x41);
        try self.emit_byte(0x58);
        // pop rbx
        try self.emit_byte(0x5b);
        // pop rax
        try self.emit_byte(0x58);
    }

    //
    // Basic emits
    //

    fn emit_slice(self: *JitCompiler, inst: []const u8) !void {
        if (inst.len > (self.code_slice.len - self.code_ptr)) {
            return JitError.OutOfMem;
        }
        @memcpy(self.code_slice[self.code_ptr..(self.code_ptr + inst.len)], inst);
        self.code_ptr += inst.len;
    }

    fn emit_byte(self: *JitCompiler, byte: u8) !void {
        const slice: [1]u8 = .{byte};
        try self.emit_slice(slice[0..]);
    }

    //
    // Other helpers
    //

    /// creates basic REX.W assuming only regs
    fn create_rex(reg: GPR64, rm64: GPR64) u8 {
        const reg_val: u8 = @intFromEnum(reg);
        const rm64_val: u8 = @intFromEnum(rm64);

        // REX.W
        // | 4-bit | W | R | X | B |
        // The W is set
        // The R contains highest bit of reg number
        // The B contains highest bit of rm64 number
        return 0x48 | ((reg_val & 0x8) >> 1) | ((rm64_val & 0x8) >> 3);
    }

    /// creates basic MODrm assuming only regs
    fn create_modrm_regs(reg: GPR64, rm64: GPR64) u8 {
        const reg_val: u8 = @intFromEnum(reg);
        const rm64_val: u8 = @intFromEnum(rm64);

        // MODrm
        // mod = 0b11 = direct | lower bits reg | lower bits rm64
        return 0b11_000_000 | ((reg_val & 0x7) << 3) | (rm64_val & 0x7);
    }

    /// creates basic MODrm assuming only regs
    fn create_modrm_sib(reg: GPR64, offset: u32) u8 {
        const reg_val: u8 = @intFromEnum(reg);

        // ModRM
        // | mode | reg | r/m |
        // mode = 00/01/10 =>
        //      this depends on size of the
        //      offset 0 => 00, 1-255 => 01
        //      otherwise 10
        // reg = bottom 3 bits of to_reg_val
        // rm = 100 => the sib follows
        const mod: u8 = if (offset >= 256)
            0b1000_0000
        else if (offset != 0)
            0b0100_0000
        else
            0;

        const to_reg_lower: u8 = (reg_val & 0x7) << 3;
        const rm_sib: u8 = 0b100;
        return mod | to_reg_lower | rm_sib;
    }

    fn create_sib(scale: Scale, base: GPR64, index: GPR64) u8 {
        const base_val = @intFromEnum(base);
        var index_val = @intFromEnum(index);

        // SIB
        // | scale : 2b | index : 3b | base : 3b |
        var scale_val: u8 = @intFromEnum(scale);
        scale_val <<= 6;
        index_val <<= 3;
        return scale_val | index_val | base_val;
    }

    /// emits most basic inst like add with only regs
    /// check if you can use this before going all in
    /// expects format : opcode r/m64 reg
    fn emit_basic_reg(self: *JitCompiler, opcode: u8, reg: GPR64, rm64: GPR64) !void {
        const inst_slice: [3]u8 = .{
            JitCompiler.create_rex(reg, rm64),
            opcode,
            JitCompiler.create_modrm_regs(reg, rm64),
        };

        try self.emit_slice(inst_slice[0..]);
    }

    fn emit_break(self: *JitCompiler) !void {
        try self.emit_byte(0xcc);
    }

    fn set_reg_from_u32_bc(self: *JitCompiler, dst: GPR64) !void {
        const num: u64 = @intCast(self.read_u32());
        try self.set_reg_64(dst, num);
    }

    /// instruction to reset flags
    /// this can be used to better
    /// asm emit
    fn vzeroupper(self: *JitCompiler) !void {
        const vzeroupper_slice: [3]u8 = .{ 0xc5, 0xf8, 0x77 };
        try self.emit_slice(vzeroupper_slice[0..]);
    }

    fn read_u32(self: *JitCompiler) u32 {
        var num: u32 = 0;
        num |= self.bytecode[self.pc];
        num <<= 8;
        num |= self.bytecode[self.pc + 1];
        num <<= 8;
        num |= self.bytecode[self.pc + 2];
        num <<= 8;
        num |= self.bytecode[self.pc + 3];
        self.pc += 4;
        return num;
    }
};
