#!/bin/bash

MINRUNS=10

hyperfine --warmup 3 --min-runs ${MINRUNS} -L kind optjit,jit,bc 'zig-out/bin/dynamo --{kind} benchmarks/whilefib.dyn' 2> /dev/null
hyperfine --warmup 3 --min-runs ${MINRUNS} -L kind optjit,jit,bc 'zig-out/bin/dynamo --{kind} benchmarks/fib.dyn' 2> /dev/null


