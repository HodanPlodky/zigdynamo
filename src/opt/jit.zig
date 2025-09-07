const ast = @import("../ast.zig");
const jit_utils = @import("../jit_utils.zig");
const GPR64 = jit_utils.GPR64;
const JitFunction = jit_utils.JitFunction;
const runtime = @import("../runtime.zig");
const RegAllocAnalysis = @import("analysis/reg_alloc.zig").RegAllocAnalysis;
const CompiledResult = @import("compile.zig").CompiledResult;

pub const JitCompiler = struct {
    base: jit_utils.JitCompilerBase,
    ir_result: CompiledResult,
    register_alloc: RegAllocAnalysis,

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
    ) jit_utils.JitError!JitFunction {
        _ = self; // autofix
        _ = function; // autofix
        _ = metadata; // autofix
    }
};
