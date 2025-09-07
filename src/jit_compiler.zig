const std = @import("std");
const bytecode = @import("bytecode.zig");
const bc_interpret = @import("bc_interpreter.zig");
const runtime = @import("runtime.zig");
const jit_utils = @import("jit_utils.zig");

const JitState = jit_utils.JitState;
const JitFunction = jit_utils.JitFunction;
const GPR64 = jit_utils.GPR64;
const Scale = jit_utils.Scale;

const BREAKPOINT: bool = false;
const DGB: bool = false;

pub const JitCompiler = struct {
    const stack_addr = GPR64.r15;
    const env_addr = GPR64.r14;
    const intepret_addr = GPR64.r13;

    base: jit_utils.JitCompilerBase,
    pc: usize,
    bytecode: []const u8,

    pub fn init(code_buffer_size: usize, heuristic: jit_utils.Heuristic) JitCompiler {
        const base = jit_utils.JitCompilerBase.init(code_buffer_size, heuristic);
        return JitCompiler{
            .base = base,
            .pc = undefined,
            .bytecode = undefined,
        };
    }

    pub fn compile_fn(
        self: *JitCompiler,
        function: *const bytecode.Function,
        metadata: *runtime.FunctionMetadata,
    ) jit_utils.JitError!JitFunction {
        self.pc = 0;

        if (metadata.jit_state != 0) {
            return JitFunction{ .code = @ptrCast(&self.base.code_slice[metadata.jit_state]) };
        }

        // this heuristic is purely chosen by vibe
        if (metadata.call_counter < self.base.heuristic.call_count) {
            metadata.call_counter += 1;
            return jit_utils.JitError.HeuristicNotMet;
        }

        self.base.start_compilation(function.code.count + 4);

        const start = self.base.code_ptr;

        // ignore the header of function
        self.bytecode = function.code.get_slice_const();

        if (BREAKPOINT) {
            try self.emit_break();
        }
        try self.emit_prolog();

        while (self.pc < self.bytecode.len) {
            try self.compile_bytecode_inst();
        }


        metadata.jit_state = @intCast(start);

        self.base.end_compilation();
        return JitFunction{ .code = @ptrCast(&self.base.code_slice[start]) };
    }

    fn compile_bytecode_inst(self: *JitCompiler) !void {
        const inst = self.read_inst();
        const offset: u32 = @intCast(self.base.code_ptr);
        self.base.append_offsets(offset, inst.get_extrabytes() + 1);

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
                try self.base.emit_byte(0xc3);
            },

            bytecode.Instruction.push_byte => {
                const number: u32 = @intCast(self.bytecode[self.pc]);
                const value = runtime.Value.new_num(number);

                try self.base.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);

                self.pc += 1;
            },
            bytecode.Instruction.push => {
                const number = self.read_u32();

                const value = runtime.Value.new_num(number);

                try self.base.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.nil => {
                const value = runtime.Value.new_nil();
                try self.base.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.true => {
                const value = runtime.Value.new_true();
                try self.base.set_reg_64(GPR64.rbp, value.data);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.false => {
                const value = runtime.Value.new_false();
                try self.base.set_reg_64(GPR64.rbp, value.data);
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
                        try comp.base.emit_slice(shift_slice[0..]);
                        // imul r8, r9
                        const imul_slice: [4]u8 = .{ 0x4d, 0x0f, 0xaf, 0xc1 };
                        try comp.base.emit_slice(imul_slice[0..]);
                    }
                }.f);
            },
            bytecode.Instruction.div => {
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler) !void {
                        // shr r8, 0x20
                        const shr_r8_slice: [4]u8 = .{ 0x49, 0xc1, 0xe8, 0x20 };
                        try comp.base.emit_slice(shr_r8_slice[0..]);
                        // shr r9, 0x20
                        const shr_r9_slice: [4]u8 = .{ 0x49, 0xc1, 0xe9, 0x20 };
                        try comp.base.emit_slice(shr_r9_slice[0..]);

                        try comp.base.mov_reg_reg(GPR64.rax, GPR64.r8);

                        // xor rdx, rdx
                        try comp.emit_basic_reg(0x31, GPR64.rdx, GPR64.rdx);

                        // div r9
                        const div_slice: [3]u8 = .{ 0x49, 0xf7, 0xf1 };
                        try comp.base.emit_slice(div_slice[0..]);

                        // shl rax, 0x20
                        const shl_slice: [4]u8 = .{ 0x48, 0xc1, 0xe0, 0x20 };
                        try comp.base.emit_slice(shl_slice[0..]);

                        try comp.base.mov_reg_reg(GPR64.r8, GPR64.rax);
                    }
                }.f);
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
                        try comp.base.emit_slice(adc_slice[0..]);

                        try comp.base.mov_reg_reg(GPR64.r8, GPR64.rdi);
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
                        try comp.base.emit_slice(adc_slice[0..]);

                        try comp.base.mov_reg_reg(GPR64.r8, GPR64.rdi);
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
                try self.base.emit_slice(sete_slice[0..]);
                // add rsi, (false value - 8)
                // 48 83 c6 08
                const add_slice: [4]u8 = .{ 0x48, 0x83, 0xc6, @intFromEnum(runtime.ValueType.false) };
                try self.base.emit_slice(add_slice[0..]);

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
                try self.base.emit_slice(sete_slice[0..]);
                // add rsi, (false value - 8)
                // 48 83 c6 08
                const add_slice: [4]u8 = .{ 0x48, 0x83, 0xc6, @intFromEnum(runtime.ValueType.false) };
                try self.base.emit_slice(add_slice[0..]);

                try self.stack_pop();
                try self.stack_set_top(GPR64.rsi);
            },
            bytecode.Instruction.jump => {
                // opcode for jmp rel32
                try self.base.emit_byte(0xe9);
                // jumps will point to number it self
                const jump_offset: u32 = @intCast(self.base.code_ptr);
                self.base.append_jump(jump_offset);

                // store original value of jump for fix up after
                const jump_pc = self.bytecode[self.pc..(self.pc + 4)];
                try self.base.emit_slice(jump_pc);
                self.pc += 4;
            },
            bytecode.Instruction.branch => {
                try self.get_stack_to_reg(GPR64.rax, 0);
                try self.stack_pop();

                const true_byte: u8 = @intFromEnum(runtime.ValueType.true);

                // tmp store
                try self.base.mov_reg_reg(GPR64.r8, GPR64.rax);

                // check if the value is even bool
                // and al, 0x8 => 0x8 highest bit of tag only used by booleans
                // 24 ib
                // cmp al, 0x8
                // 3c ib
                const and_slice: [2]u8 = .{ 0x24, 0x8 };
                try self.base.emit_slice(and_slice[0..]);
                const cmp_slice: [2]u8 = .{ 0x3c, 0x8 };
                try self.base.emit_slice(cmp_slice[0..]);
                try self.base.emit_panic("if_condition_panic");

                // tmp restore
                try self.base.mov_reg_reg(GPR64.rax, GPR64.r8);

                // cmp eax, <true_byte>
                try self.base.emit_byte(0x3d);
                const true_slice: [4]u8 = .{ true_byte, 0, 0, 0 };
                try self.base.emit_slice(true_slice[0..]);

                // opcode for jmp rel32
                const jump_slice: [2]u8 = .{ 0x0f, 0x84 };
                try self.base.emit_slice(jump_slice[0..]);
                const jump_offset: u32 = @intCast(self.base.code_ptr);
                self.base.append_jump(jump_offset);
                // store original value of jump for fix up after
                const jump_pc = self.bytecode[self.pc..(self.pc + 4)];
                try self.base.emit_slice(jump_pc);
                self.pc += 4;
            },
            bytecode.Instruction.get_small => {
                const index: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load current ptr into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.base.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                // load val from index
                try self.base.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rcx, GPR64.rax, index * 8);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.get => {
                const index: u32 = self.read_u32();

                // load current ptr into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.base.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                // load val from index
                try self.base.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rcx, GPR64.rax, index * 8);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.set_small => {
                const number: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load top of stack
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load current ptr into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.base.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                try self.base.set_to_index64(Scale.scale8, GPR64.rcx, GPR64.rax, number * 8, GPR64.rbp);
            },
            bytecode.Instruction.set => {
                const number = self.read_u32();

                // load top of stack
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load current ptr into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.base.mov_from_struct_64(GPR64.rcx, env_addr, @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"));

                try self.base.set_to_index64(Scale.scale8, GPR64.rcx, GPR64.rax, number * 8, GPR64.rbp);
            },
            bytecode.Instruction.get_global_small => {
                //try self.emit_break();
                const index: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load buffer into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.base.set_reg_64(GPR64.rcx, @intCast(index));
                // load val from index
                try self.base.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.get_global => {
                const index: u32 = self.read_u32();

                // load buffer into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.base.set_reg_64(GPR64.rcx, @intCast(index));
                // load val from index
                try self.base.mov_index_access64(GPR64.rbp, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.set_global => {
                const index: u32 = self.read_u32();

                // load value
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load buffer into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.base.set_reg_64(GPR64.rcx, @intCast(index));

                try self.base.set_to_index64(Scale.scale8, GPR64.rax, GPR64.rcx, 0, GPR64.rbp);
            },
            bytecode.Instruction.set_global_small => {
                const index: u32 = @intCast(self.bytecode[self.pc]);
                self.pc += 1;

                // load value
                try self.get_stack_to_reg(GPR64.rbp, 0);

                // load buffer into the rax
                try self.base.mov_from_struct_64(GPR64.rax, env_addr, @offsetOf(bc_interpret.Environment, "global"));
                try self.base.set_reg_64(GPR64.rcx, @intCast(index));

                try self.base.set_to_index64(Scale.scale8, GPR64.rax, GPR64.rcx, 0, GPR64.rbp);
            },
            bytecode.Instruction.call => {
                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.base.mov_reg_reg(GPR64.rsi, jit_utils.JitCompilerBase.state_addr);
                try self.vzeroupper();
                try self.base.call("call");
            },
            bytecode.Instruction.print => {
                const arg_count = self.read_u32();

                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.base.set_reg_64(GPR64.rsi, @intCast(arg_count));

                try self.base.call("print");
            },
            bytecode.Instruction.string => {
                const const_idx = self.read_u32();
                const string = runtime.Value.new_string(const_idx);
                try self.base.set_reg_64(GPR64.rbp, string.data);

                // load bytecode.constants.ptr
                try self.base.mov_from_struct_64(
                    GPR64.rax,
                    intepret_addr,
                    @offsetOf(bc_interpret.JitInterpreter, "bytecode") + @offsetOf(bytecode.Bytecode, "constants"),
                );

                // load constant
                try self.base.set_reg_64(GPR64.rcx, @intCast(const_idx));
                try self.base.mov_index_access64(GPR64.rax, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                // cmp BYTE PTR [rax+0x4], <string_type> (0x2)
                // 80 78 04 02
                const cmp_slice: [4]u8 = .{
                    0x80,
                    0x78,
                    0x04,
                    @intFromEnum(bytecode.ConstantType.string),
                };
                try self.base.emit_slice(cmp_slice[0..]);

                try self.base.emit_panic("string_panic");

                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.closure => {
                const constant_idx: u64 = self.read_u32();
                const unbound_count: u64 = self.read_u32();
                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.base.set_reg_64(GPR64.rsi, constant_idx);
                try self.base.set_reg_64(GPR64.rdx, unbound_count);

                try self.base.call("create_closure");
            },
            bytecode.Instruction.object => {
                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.set_reg_from_u32_bc(GPR64.rsi);

                try self.base.call("create_object");
            },
            bytecode.Instruction.get_field => {
                const index: u64 = @intCast(self.read_u32());
                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.base.set_reg_64(GPR64.rsi, index);
                try self.base.call("get_field");
            },
            bytecode.Instruction.set_field => {
                const index: u64 = @intCast(self.read_u32());
                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.base.set_reg_64(GPR64.rsi, index);

                try self.base.call("set_field");
            },
            bytecode.Instruction.methodcall => {
                const index: u64 = @intCast(self.read_u32());
                try self.base.mov_reg_reg(GPR64.rdi, intepret_addr);
                try self.base.mov_reg_reg(GPR64.rsi, jit_utils.JitCompilerBase.state_addr);
                try self.base.set_reg_64(GPR64.rdx, index);

                try self.base.call("method_call");
            },
            bytecode.Instruction.dup => {
                try self.get_stack_to_reg(GPR64.rbp, 0);
                try self.stack_push(GPR64.rbp);
            },
            bytecode.Instruction.ret_main => {
                // only support jiting normal
                // function but ret_main should only
                // occur in main function
                return jit_utils.JitError.UnsupportedBcInstruction;
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

    fn handle_binop(self: *JitCompiler, comptime oper: fn (*JitCompiler) jit_utils.JitError!void) !void {
        try self.get_stack_to_reg(GPR64.r9, 0);
        try self.get_stack_to_reg(GPR64.r8, 1);

        // check lower bits
        try self.base.mov_reg_reg(GPR64.rdi, GPR64.r8);
        // or rdi, r9
        // or r/m64 reg
        try self.emit_basic_reg(0x09, GPR64.r9, GPR64.rdi);
        // test dil,0x7 (dil lowest 8 bit of rdi)
        // 40 f6 c7 07
        const test_slice: [4]u8 = .{ 0x40, 0xf6, 0xc7, 0x07 };
        try self.base.emit_slice(test_slice[0..]);

        // handle cond
        try self.base.emit_panic("binop_panic");

        try oper(self);

        try self.stack_pop();
        try self.stack_set_top(GPR64.r8);
    }

    fn read_inst(self: *JitCompiler) bytecode.Instruction {
        const res: bytecode.Instruction = @enumFromInt(self.bytecode[self.pc]);
        self.pc += 1;
        return res;
    }


    //
    // Stack helpers
    //

    /// clobers rax and rcx
    fn get_stack_to_reg(self: *JitCompiler, dst: GPR64, offset: u8) !void {
        // load len
        try self.base.mov_from_struct_64(GPR64.rcx, stack_addr, 8);

        // load stack ptr
        try self.base.deref_ptr(GPR64.rax, stack_addr);

        // load value from top of stack with offset
        // mov reg, [rax + rcx*8 - offset]
        // offset is calculated with two's complement
        const scale = Scale.from_size(@sizeOf(runtime.Value));
        try self.base.mov_index_access64(dst, scale, GPR64.rax, GPR64.rcx, @intCast((~((offset + 1) * @sizeOf(runtime.Value))) + 1));
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
        try self.base.emit_slice(dec_slice[0..]);
    }

    fn stack_push(self: *JitCompiler, src: GPR64) !void {
        std.debug.assert(src != GPR64.rax);
        std.debug.assert(src != GPR64.rcx);
        std.debug.assert(src != GPR64.rdi);
        std.debug.assert(src != GPR64.rsi);
        try self.vzeroupper();

        try self.base.mov_reg_reg(GPR64.rdi, stack_addr);
        // load len
        try self.base.mov_from_struct_64(GPR64.rsi, stack_addr, 0x8);

        // inc rsi
        // 48 ff c6 why the fuck it is also 0xff ????
        const inc_slice: [3]u8 = .{ 0x48, 0xff, 0xc6 };
        try self.base.emit_slice(inc_slice[0..]);

        // store new len
        // mov [r15 + 0x8], rsi (r15 is stack addr)
        // 49 89 77 08
        const len_store_slice: [4]u8 = .{ 0x49, 0x89, 0x77, 0x08 };
        try self.base.emit_slice(len_store_slice[0..]);

        // check capacity and jump over call if
        // it is not necessary

        // load cap
        const cap_offset = @offsetOf(std.ArrayList(runtime.Value), "capacity");
        try self.base.mov_from_struct_64(GPR64.rcx, stack_addr, cap_offset);

        // cmp rsi, rcx
        // 48 39 ce
        const cmp_slice: [3]u8 = .{ 0x48, 0x39, 0xce };
        try self.base.emit_slice(cmp_slice[0..]);

        // jle <after_alloc_stack>
        // 7e 09
        const jump_slice: [2]u8 = .{ 0x7e, 0x09 };
        try self.base.emit_slice(jump_slice[0..]);

        try self.base.call("alloc_stack");

        try self.stack_set_top(src);

        // do dbg
        if (DGB) {
            try self.get_stack_to_reg(GPR64.rdi, 0);
            try self.base.call("dbg");
        }
    }

    /// clobers rax, rcx
    fn stack_set_top(self: *JitCompiler, src: GPR64) !void {
        std.debug.assert(src != GPR64.rax);
        std.debug.assert(src != GPR64.rcx);
        const reg_val: u8 = @intFromEnum(src);

        // load len
        try self.base.mov_from_struct_64(GPR64.rcx, stack_addr, 8);

        // load stack ptr
        try self.base.deref_ptr(GPR64.rax, stack_addr);

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
        try self.base.emit_slice(move_slice[0..]);
    }

    //
    // env helpers
    //

    fn env_pop_locals(self: *JitCompiler) !void {
        // load local buffer ptr
        try self.base.mov_from_struct_64(
            GPR64.rax,
            env_addr,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer"),
        );

        // load local buffer len
        try self.base.mov_from_struct_64(
            GPR64.rcx,
            env_addr,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer") + 8,
        );

        // this is done with 32bit so I dont
        // have the machinery to do that
        // mov eax,DWORD PTR [rax+rcx*8-<2 * sizeof(Value)>]
        // 8b 44 c8 f0
        const load_old_fp_slice: [4]u8 = .{ 0x8b, 0x44, 0xc8, 0xf0 };
        try self.base.emit_slice(load_old_fp_slice[0..]);

        // load current fp again 32bit
        // mov ecx,DWORD PTR [r14+<current_fp_offset>]
        // 41 8b 4e 48
        const load_current_fp_slice: [4]u8 = .{
            0x41,
            0x8b,
            0x4e,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"),
        };
        try self.base.emit_slice(load_current_fp_slice[0..]);

        // store current ptr to len of buffer
        // mov QWORD PTR [r14+<len offset>],rcx
        // 49 89 4e 18
        const store_len_slice: [4]u8 = .{
            0x49,
            0x89,
            0x4e,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "buffer") + 8,
        };
        try self.base.emit_slice(store_len_slice[0..]);

        // mov DWORD PTR [r14+<current_ptr>],eax
        // 41 89 46 48
        const store_curent_ptr_slice: [4]u8 = .{
            0x41,
            0x89,
            0x46,
            @offsetOf(bc_interpret.Environment, "local") + @offsetOf(bc_interpret.LocalEnv, "current_ptr"),
        };
        try self.base.emit_slice(store_curent_ptr_slice[0..]);
    }


    //
    // Prolog and epilog
    //

    fn emit_prolog(self: *JitCompiler) !void {
        // push rbx
        try self.base.emit_byte(0x53);
        // push rbp
        try self.base.emit_byte(0x55);
        // push r8
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x50);
        // push r9
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x51);
        // push r13
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x55);
        // push r14
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x56);
        // push r15
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x57);

        // mov rbx, rdi
        // rbx will store address to state
        try self.base.mov_reg_reg(jit_utils.JitCompilerBase.state_addr, GPR64.rdi);

        // mov r15, [rbx + <stack offset>]
        // r15 will store stack addr
        try self.base.mov_from_jit_state(stack_addr, "stack");

        // mov r14, [rbx + <stack offset>]
        // r14 will store env addr
        try self.base.mov_from_jit_state(env_addr, "env");

        // mov r13, [rbx + <stack offset>]
        // r14 will store env addr
        try self.base.mov_from_jit_state(intepret_addr, "intepreter");
    }

    fn emit_epilog(self: *JitCompiler) !void {
        // pop r15
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x5f);
        // pop r14
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x5e);
        // pop r13
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x5d);
        // pop r9
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x59);
        // pop r8
        try self.base.emit_byte(0x41);
        try self.base.emit_byte(0x58);
        // pop rbp
        try self.base.emit_byte(0x5d);
        // pop rbx
        try self.base.emit_byte(0x5b);
    }

    /// emits most basic inst like add with only regs
    /// check if you can use this before going all in
    /// expects format : opcode r/m64 reg
    fn emit_basic_reg(self: *JitCompiler, opcode: u8, reg: GPR64, rm64: GPR64) !void {
        const inst_slice: [3]u8 = .{
            jit_utils.create_rex(reg, rm64),
            opcode,
            jit_utils.create_modrm_regs(reg, rm64),
        };

        try self.base.emit_slice(inst_slice[0..]);
    }

    fn set_reg_from_u32_bc(self: *JitCompiler, dst: GPR64) !void {
        const num: u64 = @intCast(self.read_u32());
        try self.base.set_reg_64(dst, num);
    }

    /// instruction to reset flags
    /// this can be used to better
    /// asm emit
    fn vzeroupper(self: *JitCompiler) !void {
        const vzeroupper_slice: [3]u8 = .{ 0xc5, 0xf8, 0x77 };
        try self.base.emit_slice(vzeroupper_slice[0..]);
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
