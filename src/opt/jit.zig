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
    ir_result: CompiledResult,
    register_alloc: RegAllocAnalysis,
    globals: [][]const u8,

    pub fn init(code_buffer_size: usize, heuristic: jit_utils.Heuristic) JitCompiler {
        const base = jit_utils.JitCompilerBase.init(code_buffer_size, heuristic);
        return JitCompiler{
            .base = base,
            .ir_result = undefined,
            .register_alloc = undefined,
        };
    }

    pub fn compile_fn(
        self: *JitCompiler,
        function: *const ast.Function,
        metadata: *runtime.FunctionMetadata,
    ) !JitFunction {
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
            GPR64.rcx,
            GPR64.rdx,
            GPR64.r9,
        });
        self.register_alloc.analyze();

        self.ir_result = compiler.create_result();

        self.compile_ir_function(self.ir_result.entry_fn);

        metadata.jit_state = @intCast(start);

        self.base.end_compilation();
        return JitFunction{ .code = @ptrCast(&self.base.code_slice[start]) };
    }

    fn compile_ir_function(self: *JitCompiler, function_idx: ir.FunctionIdx) !void {
        const function = self.ir_result.stores.get(ir.Function, function_idx);
        self.compile_ir_basicblock(function.entry);
    }

    fn compile_ir_basicblock(self: *JitCompiler, bb_idx: ir.BasicBlockIdx) !void {
        const bb = self.ir_result.stores.get(ir.BasicBlock, bb_idx);
        for (bb.instructions) |inst_idx| {
            self.compile_ir_instruction(inst_idx);
        }
    }

    fn compile_ir_instruction(self: *JitCompiler, inst_idx: ir.InstructionIdx) !void {
        const inst = self.ir_result.stores.get(ir.Instruction, inst_idx);
        switch (inst) {
            .ldi => {},
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
                //ret
                try self.base.emit_byte(0xc3);
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
};
