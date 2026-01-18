const std = @import("std");
const bytecode = @import("../bytecode.zig");
const ast = @import("../ast.zig");
const jit_utils = @import("../jit_utils.zig");
const GPR64 = jit_utils.GPR64;
const Scale = jit_utils.Scale;
const JitFunction = jit_utils.JitFunction;
const runtime = @import("../runtime.zig");
const RegAllocAnalysis = @import("analysis/reg_alloc.zig").RegAllocAnalysis;
const CompiledResult = @import("compile.zig").CompiledResult;
const Compiler = @import("compile.zig").Compiler;
const ir = @import("ir.zig");
const AnalysisBase = @import("analysis/analysis_base.zig").AnalysisBase;
const SharedData = @import("analysis/analysis_base.zig").SharedData;
const run_passes = @import("compile.zig").run_passes;
const outofssa = @import("compile.zig").outofssa;
const OptJitInterpreter = @import("../bc_interpreter.zig").OptJitInterpreter;
const Environment = @import("../bc_interpreter.zig").Environment;
const LocalEnv = @import("../bc_interpreter.zig").LocalEnv;
const rev = @import("../utils.zig").ReversedSlice;

const ValuePlace = RegAllocAnalysis.ValuePlace;

const JitState = jit_utils.JitState(OptJitInterpreter);

const DBG: bool = false;

pub const JitCompiler = struct {
    // register usage:
    //   rbx: jit state address
    //   rax, rcx, rdi, rsi: temporary values in instructions
    //   r12, r13, r4, r15: free regs used for ir values (callee save so you have to handle it)
    //   rest unused

    const Base = jit_utils.JitCompilerBase(JitState);
    const BitSet = std.DynamicBitSetUnmanaged;

    base: Base,
    ir_compiler: *const Compiler,
    register_alloc: RegAllocAnalysis,
    globals: [][]const u8,

    bb_emited: BitSet,

    pub fn init(code_buffer_size: usize, heuristic: jit_utils.Heuristic) JitCompiler {
        const base = Base.init(code_buffer_size, heuristic);
        return JitCompiler{
            .base = base,
            .ir_compiler = undefined,
            .register_alloc = undefined,
            .bb_emited = undefined,
            .globals = undefined,
        };
    }

    pub fn compile_fn(
        self: *JitCompiler,
        bcdata: *const bytecode.Function,
        function: *const ast.Function,
        metadata: *runtime.FunctionMetadata,
        globals: [][]const u8,
    ) !JitFunction {
        // bytecode is not used;
        _ = bcdata;

        defer _ = self.base.scratch_arena.reset(.retain_capacity);

        if (metadata.jit_state != 0) {
            return JitFunction{ .code = @ptrCast(&self.base.code_slice[metadata.jit_state]) };
        }

        // this heuristic is purely chosen by vibe
        if (metadata.call_counter < self.base.heuristic.call_count) {
            metadata.call_counter += 1;
            return jit_utils.JitError.HeuristicNotMet;
        }

        self.globals = globals;
        const scratch = self.base.scratch_arena.allocator();

        var compiler = try Compiler.init(self.globals, scratch, scratch);
        try compiler.compile(function, metadata);
        const shared_data = try SharedData.init(&compiler, scratch);
        try run_passes(&compiler, scratch, shared_data);
        try outofssa(&compiler, scratch, scratch, shared_data);

        self.base.start_compilation(compiler.stores.get_max_idx(ir.Instruction).get_usize());
        const start = self.base.code_ptr;
        const analysis_base = AnalysisBase{
            .compiler = &compiler,
            .alloc = scratch,
            .shared_data = shared_data,
        };

        var free_regs: [4]GPR64 = .{
            GPR64.r12,
            GPR64.r13,
            GPR64.r14,
            GPR64.r15,
        };
        self.register_alloc = try RegAllocAnalysis.init(analysis_base, &free_regs);
        try self.register_alloc.analyze();

        self.ir_compiler = &compiler;

        const bb_count = self.ir_compiler.stores.get_max_idx(ir.BasicBlock).get_usize();

        for (0..bb_count) |_| {
            self.base.append_offsets(std.math.maxInt(u32), 1);
        }

        self.bb_emited = try BitSet.initEmpty(scratch, bb_count);

        try self.compile_ir_function(self.ir_compiler.entry_fn, true);

        metadata.jit_state = @intCast(start);

        self.base.end_compilation();
        return JitFunction{ .code = @ptrCast(&self.base.code_slice[start]) };
    }

    fn compile_ir_function(self: *JitCompiler, function_idx: ir.FunctionIdx, top_level: bool) !void {
        const function = self.ir_compiler.stores.get(ir.Function, function_idx);
        if (DBG) {
            try self.base.emit_break();
        }
        try self.emit_prolog();
        try self.compile_ir_basicblock(function.entry, top_level);
    }

    fn compile_ir_basicblock(self: *JitCompiler, bb_idx: ir.BasicBlockIdx, top_level: bool) !void {
        if (self.bb_emited.isSet(bb_idx.get_usize())) {
            return;
        }
        self.bb_emited.set(bb_idx.get_usize());
        self.base.offsets.items[bb_idx.get_usize()] = @intCast(self.base.code_ptr);
        const bb = self.ir_compiler.stores.get(ir.BasicBlock, bb_idx);
        for (bb.instructions.items) |inst_idx| {
            try self.compile_ir_instruction(inst_idx, top_level);
        }

        var succ_iter = self.ir_compiler.get_succesors(bb_idx);
        while (succ_iter.next()) |succ| {
            try self.compile_ir_basicblock(succ, top_level);
        }
    }

    fn compile_ir_instruction(self: *JitCompiler, inst_idx: ir.InstructionIdx, top_level: bool) !void {
        const inst = self.ir_compiler.stores.get(ir.Instruction, inst_idx);
        const ir_reg = self.ir_compiler.get_canonical_output(inst_idx);
        //if (self.ir_compiler.stores.get_type(inst) != .Void) {
        //const place = self.get_place(ir_reg);
        //std.debug.print("{} -> {} -> {}\n", .{ inst_idx.get_usize(), ir_reg.get_usize(), place });
        //}

        switch (inst) {
            .ldi, .string => |_| {
                // nop
                // it only introduces the values
                // and those are already set in reg alloc
                // where this register
                // is set as value reg
            },
            .nil, .true, .false => {
                // same as ldi
            },
            .nop => {},
            .mov, .regify => |reg| {
                const src = self.get_place(reg);
                const dst = self.get_place(ir_reg);
                try self.mov_places(src, dst);
            },

            .load_global => |index| {
                const EnvType = @import("../bc_interpreter.zig").Environment;
                // load env addr
                try self.base.mov_from_jit_state(GPR64.rdi, "env");

                // load global
                try self.base.mov_from_struct_64(GPR64.rax, GPR64.rdi, @offsetOf(EnvType, "global"));
                try self.base.set_reg_64(GPR64.rcx, @intCast(index));
                try self.base.mov_index_access64(GPR64.rsi, Scale.scale8, GPR64.rax, GPR64.rcx, 0);

                const outplace = self.get_place(ir_reg);
                try self.mov_places(.{ .reg = GPR64.rsi }, outplace);
            },
            .store_global => |store_idx| {
                const EnvType = @import("../bc_interpreter.zig").Environment;
                const store = self.ir_compiler.get(ir.StoreData, store_idx);

                // load val
                const src_place = self.get_place(store.value);
                try self.mov_place_to_reg(src_place, GPR64.rsi);

                // load env addr
                try self.base.mov_from_jit_state(GPR64.rdi, "env");
                // load buffer into the rax
                try self.base.mov_from_struct_64(GPR64.rax, GPR64.rdi, @offsetOf(EnvType, "global"));
                try self.base.set_reg_64(GPR64.rcx, @intCast(store.idx));

                // store it
                try self.base.set_to_index64(Scale.scale8, GPR64.rax, GPR64.rcx, 0, GPR64.rsi);
            },
            .load_env => unreachable,
            .store_env => unreachable,
            .add => |binop_idx| try self.handle_binop_simple(0x1, ir_reg, binop_idx),
            .sub => |binop_idx| try self.handle_binop_simple(0x29, ir_reg, binop_idx),
            .mul => |binop_idx| {
                const binop = self.ir_compiler.get(ir.BinOpData, binop_idx);
                const left = self.get_place(binop.left);
                const right = self.get_place(binop.right);
                const out_place = self.get_place(ir_reg);
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler, output: ValuePlace) !void {
                        // shr rsi, 0x20
                        try comp.base.emit_slice(&.{ 0x48, 0xc1, 0xee, 0x20 });
                        // imul rdi, rsi
                        const imul_slice: [4]u8 = .{ 0x48, 0x0f, 0xaf, 0xfe };
                        try comp.base.emit_slice(imul_slice[0..]);
                        try comp.mov_places(.{ .reg = GPR64.rdi }, output);
                    }
                }.f, out_place, left, right);
            },
            .div => unreachable,
            .gt => |binop_idx| {
                const binop = self.ir_compiler.get(ir.BinOpData, binop_idx);
                const left = self.get_place(binop.left);
                const right = self.get_place(binop.right);
                const out_place = self.get_place(ir_reg);
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler, output: ValuePlace) !void {
                        // xor rax, rax
                        try comp.emit_basic_reg(0x31, GPR64.rax, GPR64.rax);

                        // cmp rdi, rsi
                        try comp.emit_basic_reg(0x39, GPR64.rdi, GPR64.rsi);

                        // adc rax,0x8
                        // add with carry
                        // 48 83 d0 05
                        const adc_slice: [4]u8 = .{ 0x48, 0x83, 0xd0, 0x08 };
                        try comp.base.emit_slice(adc_slice[0..]);
                        try comp.mov_places(.{ .reg = GPR64.rax }, output);
                    }
                }.f, out_place, left, right);
            },
            .lt => |binop_idx| {
                const binop = self.ir_compiler.get(ir.BinOpData, binop_idx);
                const left = self.get_place(binop.left);
                const right = self.get_place(binop.right);
                const out_place = self.get_place(ir_reg);
                try self.handle_binop(struct {
                    fn f(comp: *JitCompiler, output: ValuePlace) !void {
                        // xor rax, rax
                        try comp.emit_basic_reg(0x31, GPR64.rax, GPR64.rax);

                        // this is a change
                        // cmp rsi, rdi
                        try comp.emit_basic_reg(0x39, GPR64.rsi, GPR64.rdi);

                        // adc rax,0x8
                        // add with carry
                        // 48 83 d0 05
                        const adc_slice: [4]u8 = .{ 0x48, 0x83, 0xd0, 0x08 };
                        try comp.base.emit_slice(adc_slice[0..]);
                        try comp.mov_places(.{ .reg = GPR64.rax }, output);
                    }
                }.f, out_place, left, right);
            },
            .ret => |reg| {
                const src = self.get_place(reg);

                if (top_level) {
                    try self.stack_push(src);
                    try self.emit_epilog();
                    try self.base.emit_byte(0xc3);
                } else {
                    //ret
                    try self.mov_place_to_reg(src, GPR64.rax);
                    try self.emit_epilog();
                    try self.base.emit_byte(0xc3);
                }
            },
            .branch => |branch_idx| {
                const branch = self.ir_compiler.get(ir.BranchData, branch_idx);
                const cond_place = self.get_place(branch.cond);
                try self.mov_place_to_reg(cond_place, GPR64.rax);

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

                try self.mov_place_to_reg(cond_place, GPR64.rax);

                const true_byte: u8 = @intFromEnum(runtime.ValueType.true);
                // cmp eax, <true_byte>
                try self.base.emit_byte(0x3d);
                const true_slice: [4]u8 = .{ true_byte, 0, 0, 0 };
                try self.base.emit_slice(true_slice[0..]);

                // jne <false_idx>
                // we are jumping to false and put true case
                // right after
                // the false_idx will be overwritten after
                try self.base.emit_slice(&.{ 0x0f, 0x85 });
                // jumps will point to number it self
                const jump_offset: u32 = @intCast(self.base.code_ptr);
                self.base.append_jump(jump_offset);
                try self.base.emit_u32(branch.false_branch.index);
            },
            .jmp => |target_idx| {
                // opcode for jmp rel32
                try self.base.emit_byte(0xe9);
                // jumps will point to number it self
                const jump_offset: u32 = @intCast(self.base.code_ptr);
                self.base.append_jump(jump_offset);

                try self.base.emit_u32(target_idx.index);
            },
            .arg => |index| {
                try self.base.mov_from_jit_state(GPR64.rdi, "env");

                // load current ptr into the rax
                try self.base.mov_from_struct_64(GPR64.rsi, GPR64.rdi, @offsetOf(Environment, "local") + @offsetOf(LocalEnv, "current_ptr"));

                // load local.buffer ptr
                try self.base.mov_from_struct_64(GPR64.rcx, GPR64.rdi, @offsetOf(Environment, "local") + @offsetOf(LocalEnv, "buffer"));

                // load val from index
                try self.base.mov_index_access64(GPR64.rdi, Scale.scale8, GPR64.rcx, GPR64.rsi, index * 8);

                const out = self.get_place(ir_reg);
                try self.mov_places(.{ .reg = GPR64.rdi }, out);
            },
            .call => |call_idx| {
                const call = self.ir_compiler.get(ir.CallData, call_idx);

                // push args to stack
                var args_iter = rev(ir.Reg).init(call.args);
                while (args_iter.next()) |arg| {
                    const arg_place = self.get_place(arg);
                    try self.stack_push(arg_place);
                }

                // push target
                const target_place = self.get_place(call.target);
                try self.stack_push(target_place);

                // do call it self
                try self.base.mov_from_jit_state(GPR64.rdi, "intepreter");
                try self.base.mov_reg_reg(GPR64.rsi, GPR64.rbx);
                try self.base.call("call");

                const outplace = self.get_place(ir_reg);
                try self.stack_get_top(outplace, 0);
                try self.stack_pop();
            },
            .print => |print_idx| {
                const print = self.ir_compiler.get(ir.PrintData, print_idx);

                // push args to stack
                for (print.args) |arg| {
                    const arg_place = self.get_place(arg);
                    try self.stack_push(arg_place);
                }
                //
                // do call it self
                try self.base.mov_from_jit_state(GPR64.rdi, "intepreter");
                try self.base.set_reg_64(GPR64.rsi, print.args.len);
                try self.base.call("print");
                try self.stack_pop();
            },
            .copy => |copy_idx| {
                const copy = self.ir_compiler.get(ir.CopyData, copy_idx);
                const src = self.get_place(copy.src);
                const dst = self.get_place(copy.dst);
                try self.mov_places(src, dst);
            },

            // should not be in code when generating
            // machine code
            .get_local, .set_local => unreachable,
            .parallel_copy => unreachable,
            .phony => unreachable, // at this point the code should be out of ssa
        }
    }

    fn handle_binop_simple(self: *JitCompiler, comptime opcode: u8, reg: ir.Reg, binop_idx: ir.BinOpIdx) !void {
        const binop = self.ir_compiler.get(ir.BinOpData, binop_idx);
        const left = self.get_place(binop.left);
        const right = self.get_place(binop.right);
        const out_place = self.get_place(reg);
        try self.handle_binop(struct {
            fn f(comp: *JitCompiler, output: ValuePlace) !void {
                try comp.emit_basic_reg(opcode, GPR64.rsi, GPR64.rdi);
                try comp.mov_places(.{ .reg = GPR64.rdi }, output);
            }
        }.f, out_place, left, right);
    }

    fn handle_binop(self: *JitCompiler, comptime oper: fn (*JitCompiler, ValuePlace) jit_utils.JitError!void, output: ValuePlace, left: ValuePlace, right: ValuePlace) !void {
        // check lower bits
        try self.mov_place_to_reg(left, GPR64.rdi);
        try self.mov_place_to_reg(right, GPR64.rsi);

        // or rdi, rsi
        // or r/m64 reg
        try self.emit_basic_reg(0x09, GPR64.rsi, GPR64.rdi);
        // test dil,0x7 (dil lowest 8 bit of rdi)
        // 40 f6 c7 07
        const test_slice: [4]u8 = .{ 0x40, 0xf6, 0xc7, 0x07 };
        try self.base.emit_slice(test_slice[0..]);

        // handle cond
        try self.base.emit_panic("binop_panic");

        // the rdi was rewritten
        try self.mov_place_to_reg(left, GPR64.rdi);

        try oper(self, output);
    }

    fn emit_basic_reg(self: *JitCompiler, opcode: u8, reg: GPR64, rm64: GPR64) !void {
        const inst_slice: [3]u8 = .{
            jit_utils.create_rex(reg, rm64),
            opcode,
            jit_utils.create_modrm_regs(reg, rm64),
        };

        try self.base.emit_slice(inst_slice[0..]);
    }

    fn get_place(self: *const JitCompiler, inst_idx: ir.InstructionIdx) ValuePlace {
        return self.register_alloc.translates[inst_idx.get_usize()];
    }

    fn mov_places(self: *JitCompiler, src: ValuePlace, dst: ValuePlace) !void {
        switch (dst) {
            .reg => |reg| try self.mov_place_to_reg(src, reg),
            .memory => |offset| try self.mov_place_to_mem(src, offset),

            // value or none cannot be destination
            .value, .none => unreachable,
        }
    }

    fn mov_place_to_reg(self: *JitCompiler, src: ValuePlace, dst: GPR64) !void {
        switch (src) {
            .reg => |reg| try self.base.mov_reg_reg(dst, reg),
            .memory => |offset| if (offset == 0)
                try self.base.mov_from_offset(GPR64.rsp, 0, dst)
            else
                try self.base.mov_from_offset(GPR64.rsp, @intCast(offset), dst),
            .value => |value| try self.base.set_reg_64(dst, value.data),
            .none => unreachable,
        }
    }

    fn mov_place_to_mem(self: *JitCompiler, src: ValuePlace, dst_offset: usize) !void {
        switch (src) {
            .reg => |reg| if (dst_offset == 0)
                try self.base.mov_to_offset(GPR64.rsp, 0, reg)
            else
                try self.base.mov_to_offset(GPR64.rsp, @intCast(dst_offset), reg),
            .memory, .none, .value => unreachable,
        }
    }

    fn stack_push(self: *JitCompiler, src: ValuePlace) !void {
        // load stack addr
        try self.base.mov_from_jit_state(GPR64.rdi, "stack");

        // load len
        try self.base.mov_from_struct_64(GPR64.rsi, GPR64.rdi, 0x8);

        // inc rsi
        // 48 ff c6 why the fuck it is also 0xff ????
        try self.base.emit_slice(&.{ 0x48, 0xff, 0xc6 });

        // store new len
        try self.base.mov_to_offset(GPR64.rdi, 0x8, GPR64.rsi);

        // check capacity and jump over call if
        // it is not necessary

        // load cap
        const cap_offset = @offsetOf(std.ArrayList(runtime.Value), "capacity");
        try self.base.mov_from_struct_64(GPR64.rcx, GPR64.rdi, cap_offset);

        // cmp rsi, rcx
        // 48 39 ce
        try self.base.emit_slice(&.{ 0x48, 0x39, 0xce });

        // jle <after_alloc_stack>
        // 7e 09
        try self.base.emit_slice(&.{ 0x7e, 0x09 });

        try self.base.call("alloc_stack");

        try self.stack_set_top(src);
    }

    fn stack_set_top(self: *JitCompiler, src: ValuePlace) !void {
        try self.mov_place_to_reg(src, GPR64.rdi);

        // load stack addr
        try self.base.mov_from_jit_state(GPR64.rax, "stack");

        // load len
        try self.base.mov_from_struct_64(GPR64.rcx, GPR64.rax, 0x8);

        // load stack ptr
        try self.base.deref_ptr(GPR64.rax, GPR64.rax);

        // mov QWORD PTR [rax+rcx*8-0x8],rdi
        // 48 89 7c c8 f8
        try self.base.emit_slice(&.{ 0x48, 0x89, 0x7c, 0xc8, 0xf8 });
    }

    fn stack_get_top(self: *JitCompiler, dst: ValuePlace, offset: u8) !void {
        try self.base.mov_from_jit_state(GPR64.rdi, "stack");
        // load len
        try self.base.mov_from_struct_64(GPR64.rcx, GPR64.rdi, 8);

        // load stack ptr
        try self.base.deref_ptr(GPR64.rax, GPR64.rdi);

        // load value from top of stack with offset
        // mov reg, [rax + rcx*8 - offset]
        // offset is calculated with two's complement
        const scale = Scale.from_size(@sizeOf(runtime.Value));
        try self.base.mov_index_access64(GPR64.rdi, scale, GPR64.rax, GPR64.rcx, @intCast((~((offset + 1) * @sizeOf(runtime.Value))) + 1));
        try self.mov_places(.{ .reg = GPR64.rdi }, dst);
    }

    fn stack_pop(self: *JitCompiler) !void {
        try self.base.mov_from_jit_state(GPR64.rdi, "stack");
        const stack_addr_val: u8 = @intFromEnum(GPR64.rdi);

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

    fn store_regs(self: *JitCompiler) !void {
        // push r12
        try self.base.emit_slice(&.{ 0x41, 0x54 });
        // push r13
        try self.base.emit_slice(&.{ 0x41, 0x55 });
        // push r14
        try self.base.emit_slice(&.{ 0x41, 0x56 });
        // push r15
        try self.base.emit_slice(&.{ 0x41, 0x57 });
    }

    fn restore_regs(self: *JitCompiler) !void {
        // pop r15
        try self.base.emit_slice(&.{ 0x41, 0x5f });
        // pop r14
        try self.base.emit_slice(&.{ 0x41, 0x5e });
        // pop r13
        try self.base.emit_slice(&.{ 0x41, 0x5d });
        // pop r12
        try self.base.emit_slice(&.{ 0x41, 0x5c });
    }

    fn emit_prolog(self: *JitCompiler) !void {
        // push rbx
        try self.base.emit_byte(0x53);
        try self.store_regs();

        // sub rsp, <stacksize>https://www.youtube.com/watch?v=xKH3Hj4lqLs
        // 48 83 ec 7f = sub rsp, 0x7f
        if (self.register_alloc.curr_max_mem <= 0x7f) {
            const stack_size: u8 = @intCast(self.register_alloc.curr_max_mem);
            try self.base.emit_slice(&.{ 0x48, 0x83, 0xec, stack_size });
        } else {
            // future fucker got it
            unreachable;
        }

        // mov rbx, rdi
        // rbx will store address to state
        try self.base.mov_reg_reg(Base.state_addr, GPR64.rdi);
    }

    fn emit_epilog(self: *JitCompiler) !void {
        // add rsp, <stacksize>
        // 48 83 c4 7f = add rsp, 0x7f
        if (self.register_alloc.curr_max_mem <= 0x7f) {
            const stack_size: u8 = @intCast(self.register_alloc.curr_max_mem);
            try self.base.emit_slice(&.{ 0x48, 0x83, 0xc4, stack_size });
        } else {
            // future fucker got it
            unreachable;
        }

        try self.restore_regs();
        // pop rbx
        try self.base.emit_byte(0x5b);
    }
};
