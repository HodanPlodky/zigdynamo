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

const ValuePlace = RegAllocAnalysis.ValuePlace;

pub const JitCompiler = struct {
    base: jit_utils.JitCompilerBase,
    ir_compiler: *const Compiler,
    register_alloc: RegAllocAnalysis,
    globals: [][]const u8,

    pub fn init(code_buffer_size: usize, heuristic: jit_utils.Heuristic) JitCompiler {
        const base = jit_utils.JitCompilerBase.init(code_buffer_size, heuristic);
        return JitCompiler{
            .base = base,
            .ir_compiler = undefined,
            .register_alloc = undefined,
        };
    }

    pub fn compile_fn(
        self: *JitCompiler,
        bcdata: *const bytecode.Function,
        function: *const ast.Function,
        metadata: *runtime.FunctionMetadata,
    ) !JitFunction {
        // vytecode is not used;
        _ = bcdata;

        defer self.base.scratch_arena.reset();

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

        const scratch = self.base.scratch_arena.allocator();

        var compiler = try Compiler.init(self.globals, scratch, scratch);
        try compiler.compile(function, metadata);
        const shared_data = try SharedData.init(&compiler, scratch);
        try run_passes(&compiler, scratch);

        const analysis_base = AnalysisBase{
            .compiler = compiler,
            .alloc = scratch,
            .shared_data = shared_data,
        };

        self.register_alloc = RegAllocAnalysis.init(analysis_base, &.{
            GPR64.rbp,
            GPR64.r8,
            GPR64.r9,
            GPR64.r10,
        });
        self.register_alloc.analyze();

        self.ir_compiler = &compiler;

        self.compile_ir_function(self.ir_compiler.entry_fn);

        metadata.jit_state = @intCast(start);

        self.base.end_compilation();
        return JitFunction{ .code = @ptrCast(&self.base.code_slice[start]) };
    }

    fn compile_ir_function(self: *JitCompiler, function_idx: ir.FunctionIdx, top_level: bool) !void {
        const function = self.ir_compiler.stores.get(ir.Function, function_idx);
        self.compile_ir_basicblock(function.entry, top_level);
    }

    fn compile_ir_basicblock(self: *JitCompiler, bb_idx: ir.BasicBlockIdx, top_level: bool) !void {
        const bb = self.ir_compiler.stores.get(ir.BasicBlock, bb_idx);
        for (bb.instructions) |inst_idx| {
            self.compile_ir_instruction(inst_idx, top_level);
        }

        var succ_iter = self.ir_compiler.get_succesors(bb_idx);
        while (succ_iter.next()) |succ| {
            try self.compile_ir_basicblock(succ);
        }
    }

    fn compile_ir_instruction(self: *JitCompiler, inst_idx: ir.InstructionIdx, top_level: bool) !void {
        const inst = self.ir_compiler.stores.get(ir.Instruction, inst_idx);
        switch (inst) {
            .ldi => |_| {
                // nop
                // it only introduces the values
                // and those are already set in reg alloc
                // where this register
                // is set as value reg
            },
            .mov => |reg| {
                const src = self.get_place(reg);
                const dst = self.get_place(inst_idx);
                try self.mov_places(src, dst);
            },
            .nil => unreachable,
            .true => unreachable,
            .false => unreachable,
            .load_global => unreachable,
            .store_global => unreachable,
            .load_env => unreachable,
            .store_env => unreachable,
            .add => unreachable,
            .sub => unreachable,
            .mul => unreachable,
            .div => unreachable,
            .lt => unreachable,
            .gt => unreachable,
            .ret => |reg| {
                const src = self.get_place(reg);
                try self.mov_place_to_reg(src, GPR64.rax);

                if (top_level) {
                    try self.stack_push(src);
                } else {
                    //ret
                    try self.base.emit_byte(0xc3);
                }
            },
            .branch => unreachable,
            .jmp => unreachable,
            .arg => unreachable,
            .nop => unreachable,
            .phony => unreachable,
            .call => unreachable,
            .get_local => unreachable,
            .set_local => unreachable,
        }
    }

    fn get_place(self: *const JitCompiler, inst_idx: ir.InstructionIdx) ValuePlace {
        return self.register_alloc.translates[inst_idx.get_usize()];
    }

    fn mov_places(self: *JitCompiler, src: ValuePlace, dst: ValuePlace) !void {
        switch (dst) {
            .reg => |reg| try self.mov_place_to_reg(src, reg),
            .memory => |offset| try self.mov_place_to_mem(src, offset),

            // value cannot be destination
            .value => unreachable,
        }
    }

    fn mov_place_to_reg(self: *JitCompiler, src: ValuePlace, dst: GPR64) !void {
        switch (src) {
            .reg => |reg| try self.base.mov_reg_reg(dst, reg),
            .memory => |offset| if (offset == 0)
                try self.base.mov_from_offset(GPR64.rsp, 0, dst)
            else
                try self.base.mov_from_offset(GPR64.rsp, (~offset + 1), dst),
            .value => |value| try self.base.set_reg_64(dst, value.data),
        }
    }

    fn mov_place_to_mem(self: *JitCompiler, src: ValuePlace, dst_offset: usize) !void {
        switch (src) {
            .reg => |reg| if (dst_offset == 0)
                try self.base.mov_to_offset(GPR64.rsp, 0, reg)
            else
                try self.base.mov_to_offset(GPR64.rsp, (~dst_offset + 1), reg),
            .memory => unreachable,
            .value => unreachable,
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
};
