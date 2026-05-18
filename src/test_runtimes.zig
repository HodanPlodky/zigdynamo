const std = @import("std");
const Parser = @import("parser.zig").Parser;
const compile = @import("compiler.zig").compile;
const BcInterpreter = @import("bc_interpreter.zig").BcInterpreter;
const JitInterpreter = @import("bc_interpreter.zig").JitInterpreter;
const OptJitInterpreter = @import("bc_interpreter.zig").OptJitInterpreter;
const Bytecode = @import("bytecode.zig").Bytecode;
const runtime = @import("runtime.zig");
const snap = @import("snap.zig");

const TestResult = struct {
    result: u64,
    output: std.ArrayList(u8),

    fn new(result: u64, output: std.ArrayList(u8)) !TestResult {
        return TestResult{
            .result = result,
            .output = output,
        };
    }

    fn deinit(self: *TestResult) void {
        self.output.deinit(std.testing.allocator);
    }

    pub fn format(
        self: *const TestResult,
        writer: *std.Io.Writer,
    ) !void {
        try writer.print("result: {x} ({})\n{s}\n", .{ self.result, self.result >> 32, self.output.items });
    }
};

fn run_with(comptime Interpret: type, bytecode: Bytecode, allocator: std.mem.Allocator, writer: *std.Io.Writer) !runtime.Value {
    var runtime_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer runtime_arena.deinit();
    const alloc = runtime_arena.allocator();
    var interpret = Interpret.init(
        alloc,
        bytecode,
        try allocator.allocWithOptions(u8, 1024 + 512, std.mem.Alignment.@"16", null),
        writer,
        .{ .call_count = 0 },
    );
    const val = interpret.run();
    return val;
}

fn test_helper(code: []const u8) !TestResult {
    return test_helper_inner(code, &.{ JitInterpreter, OptJitInterpreter });
}

fn test_helper_inner(code: []const u8, comptime jits: []const type) !TestResult {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(code, allocator);
    const prog = try p.parse();
    const bytecode = try compile(prog, allocator);

    var bc_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer bc_writer.deinit();
    const bc_val = try run_with(
        BcInterpreter,
        bytecode,
        allocator,
        &bc_writer.writer,
    );

    const bc_data = bc_writer.toArrayList();

    inline for (jits) |jit_type| {
        var jit_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer jit_writer.deinit();
        const jit_val = try run_with(
            jit_type,
            bytecode,
            allocator,
            &jit_writer.writer,
        );
        try std.testing.expectEqual(bc_val.data, jit_val.data);

        var jit_data = jit_writer.toArrayList();
        defer jit_data.deinit(std.testing.allocator);

        try std.testing.expectEqualStrings(bc_data.items, jit_data.items);
    }

    return try TestResult.new(bc_val.data, bc_data);
}

test "basic" {
    const code =
        \\ let f = fn() = 1;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 100000000 (1)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "fib" {
    const code =
        \\ let fib = fn(n) = 
        \\     if (n < 2)
        \\         n
        \\     else
        \\         fib(n - 1) + fib(n - 2);
        \\ 
        \\ fib(10);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 3700000000 (55)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "factorial" {
    const code =
        \\ let factorial = fn(n) = 
        \\     if (n < 1)
        \\         1
        \\     else
        \\         n * factorial(n - 1);
        \\ 
        \\ print(factorial(5));
        \\ print(factorial(0));
        \\ print(factorial(10));
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\120 
        \\1 
        \\3628800 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "factorial_loop" {
    const code =
        \\ let factorial = fn(n) = {
        \\     let result = 1;
        \\     while (n > 1) {
        \\         result = result * n;
        \\         n = n - 1;
        \\     };
        \\     result;
        \\ };
        \\ 
        \\ print(factorial(5));
        \\ print(factorial(0));
        \\ print(factorial(10));
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\120 
        \\1 
        \\3628800 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "assign" {
    const code =
        \\ let a = 5;
        \\ print(a);
        \\ a = 10;
        \\ print(a);
        \\ a = 11;
        \\ a;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: b00000000 (11)
        \\5 
        \\10 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "basic arith" {
    const code =
        \\  1 + 2;
        \\ 1 +   2 * 2 - 3;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 200000000 (2)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "basic_closure" {
    const code =
        \\ let inc = fn(n) = fn(x) = n + x;
        \\ let inc1 = inc(1);
        \\ inc1(2);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 300000000 (3)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "basic_function" {
    const code =
        \\ let f = fn(n) = n + 1;
        \\ f(1);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 200000000 (2)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "basic_method_call" {
    const code =
        \\ let o = object {
        \\     a: 1,
        \\     val: "lalal",
        \\     f: fn(x) = {
        \\         print(this.a + x);
        \\     },
        \\ };
        \\ o.f(1);
        \\ print(o.val);
        \\ o.a = 2;
        \\ o.f(2);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\2 
        \\lalal 
        \\4 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "basic_object" {
    const code =
        \\ let o = object {
        \\     a: 1,
        \\     val: "ahoj",
        \\ };
        \\ 
        \\ print(o.a + 1);
        \\ print(o.val);
        \\ o.a = 2;
        \\ print(o.a + 1);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\2 
        \\ahoj 
        \\3 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "closure_test" {
    const code =
        \\ let f = fn() = n;
        \\ let n = 1;
        \\ let f2 = fn() = {
        \\     let n = 2;
        \\     print(f());
        \\ };
        \\ 
        \\ f2();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\1 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "function" {
    const code =
        \\ let inc = fn(incremeter) = fn(n) = n + incremeter;
        \\ let inc1 = inc(1);
        \\ 
        \\ let twice = fn(f, val) = f(f(val));
        \\ 
        \\ print(twice(inc(5), 1));
        \\ 
        \\ let print_hello = fn() = print("hello");
        \\ 
        \\ print_hello();
        \\ 
        \\ inc(2)(inc1(1) + inc(1)(2));
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 700000000 (7)
        \\11 
        \\hello 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "if" {
    const code =
        \\ let x = true;
        \\ if (x) 1 else 2;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 100000000 (1)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

//test "incorrect_add" {
//const code =
//\\ let f = fn() = "a" + 1;
//\\ f();
//;
//try test_helper(code[0..]);
//}

test "let" {
    const code =
        \\ let x = 1 + 2;
        \\ x + 2;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 500000000 (5)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "linkedlist" {
    const code =
        \\ let createnode = fn(val) = object {
        \\     val : val,
        \\     next: nil,
        \\ };
        \\ 
        \\ let createlist = fn() = object {
        \\     head: nil,
        \\ 
        \\     append: fn(val) = {
        \\         print("append");
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
        \\         print("prepend");
        \\         let tmp = this.head;
        \\         this.head = createnode(val);
        \\         this.head.next = tmp;
        \\     },
        \\ 
        \\     pop: fn() = {
        \\         print("pop");
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
        \\ while (i < 3) {
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
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\append 
        \\append 
        \\append 
        \\prepend 
        \\pop 
        \\2 
        \\pop 
        \\1 
        \\pop 
        \\42 
        \\append 
        \\append 
        \\append 
        \\prepend 
        \\pop 
        \\2 
        \\pop 
        \\1 
        \\pop 
        \\42 
        \\append 
        \\append 
        \\append 
        \\prepend 
        \\pop 
        \\2 
        \\pop 
        \\1 
        \\pop 
        \\42 
        \\42 
        \\42 
        \\42 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "number" {
    const code =
        \\ 1;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 100000000 (1)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "object" {
    const code =
        \\ let pos = object {
        \\     x: 1,
        \\     y: 2,
        \\ };
        \\ 
        \\ let person = object {
        \\     name: "Adam",
        \\     age: 25,
        \\     ageup: fn() = {
        \\         this.age = this.age + 1;
        \\     },
        \\ };
        \\ 
        \\ let ask = fn (person) = {
        \\     let age = person.age;
        \\     if (age < 18)
        \\         print("cannot drink")
        \\     else
        \\         print("can drink");
        \\ };
        \\ 
        \\ print(person.name, "age", person.age);
        \\ ask(person);
        \\ person.age = 15;
        \\ ask(person);
        \\ person.ageup();
        \\ print(person.age);
        \\ 
        \\ let protoperson = object {
        \\     ending: "!",
        \\ 
        \\     greet: fn() = {
        \\         print(this.prefer_greet(),  this.name, this.ending);
        \\     },
        \\ 
        \\     prefer_greet: fn() = {
        \\         "Hello";
        \\     },
        \\ };
        \\ 
        \\ let adam = object(protoperson) {
        \\     name: "Adam",
        \\ 
        \\     prefer_greet: fn() = {
        \\         "Yo";
        \\     },
        \\ };
        \\ 
        \\ adam.greet();
        \\ 
        \\ pos.x + pos.y;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 300000000 (3)
        \\Adam age 25 
        \\can drink 
        \\cannot drink 
        \\16 
        \\Yo Adam ! 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "ret1" {
    const code =
        \\ let f = fn() = 1;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 100000000 (1)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retadd" {
    const code =
        \\ let f = fn() = 1 + 2 + 3;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 600000000 (6)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retbignum" {
    const code =
        \\ let f = fn() = 12345;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 303900000000 (12345)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retident" {
    const code =
        \\ let ident = fn(x) = x;
        \\ let f = fn(x, y) = x + 2 * y;
        \\ f(ident(2), ident(3));
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 800000000 (8)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retif" {
    const code =
        \\ let f = fn() = if (1 < 2) 1 else 2;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 100000000 (1)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retinnercall" {
    const code =
        \\ let inc = fn(x) = x + 1;
        \\ let double_inc = fn(x) = inc(inc(x));
        \\ double_inc(1);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 300000000 (3)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retmul" {
    const code =
        \\ let f = fn() = 2 * 3 * 4;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1800000000 (24)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retobject" {
    const code =
        \\ let f = fn(n) = object {
        \\     n: n + 1,
        \\     other: "ahoj",
        \\ };
        \\ 
        \\ let x = f(1);
        \\ print(x.other);
        \\ x.n;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 200000000 (2)
        \\ahoj 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retset" {
    const code =
        \\ let f = fn(x) = {
        \\     let y = x + 1;
        \\     y = y + 1;
        \\     y + 1;
        \\ };
        \\ 
        \\ f(1);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 400000000 (4)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retstring" {
    const code =
        \\ let f = fn() = "hello";
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 7 (0)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "retsub" {
    const code =
        \\ let f = fn() = 10 - 1;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 900000000 (9)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "sayhello" {
    const code =
        \\ let say_one = fn() = print("hello");
        \\ 
        \\ let do_it_more = fn() = {
        \\     let n = 0;
        \\     while (n < 10) {
        \\         n = n + 1;
        \\         say_one();
        \\     };
        \\ };
        \\ 
        \\ do_it_more();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\hello 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "get global" {
    const code =
        \\ let f = fn() = n;
        \\ let n = 5;
        \\ f();
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 500000000 (5)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "setglobal" {
    const code =
        \\ let n = 1;
        \\ let f = fn() = {
        \\     n = n + 1;
        \\ };
        \\ 
        \\ print(n);
        \\ f();
        \\ print(n);
        \\ f();
        \\ print(n);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\1 
        \\2 
        \\3 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "while" {
    const code =
        \\ let fib = fn(n) = {
        \\     let a = 0;
        \\     let b = 1;
        \\     while (n > 0) {
        \\         let tmp = a;
        \\         a = b;
        \\         b = tmp + b;
        \\         n = n - 1;
        \\     };
        \\     a;
        \\ };
        \\ 
        \\ fib(40);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 6197ecb00000000 (102334155)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "division" {
    const code =
        \\ let f = fn(x) = x / 3;
        \\ print(f(7) * 3 != 7);
        \\ print(f(7) == 2);
        \\ f(7);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 200000000 (2)
        \\true 
        \\true 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "print multiple" {
    const code =
        \\ let f = fn(x, y) = {
        \\     print(y, x, "hello");
        \\     print("x is", x, "y is", y);
        \\ };
        \\ f("a", 1);
        \\ f(1, "2");
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\1 a hello 
        \\x is a y is 1 
        \\2 1 hello 
        \\x is 1 y is 2 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "test runtime max" {
    const code =
        \\ let max = fn(a, b) = if (a > b) a else b;
        \\ print(max(1, 2));
        \\ print(max(2, 1));
        \\ print(max(2, 2));
        \\ print(max(0, 200));
        \\ max(123, 123);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 7b00000000 (123)
        \\2 
        \\2 
        \\2 
        \\200 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "test runtime isbigger/smaller" {
    const code =
        \\ let isbigger = fn(a, b) = a > b;
        \\ let issmaller = fn(a, b) = a < b;
        \\ print(isbigger(2, 1));
        \\ print(isbigger(1, 2));
        \\ print(issmaller(2, 1));
        \\ print(issmaller(1, 2));
        \\ isbigger(2, 2);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 8 (0)
        \\true 
        \\false 
        \\false 
        \\true 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "field access" {
    const code =
        \\ let f = fn(o) = {
        \\     o.number + 1;
        \\ };
        \\ 
        \\ f(object {number: 41,});
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 2a00000000 (42)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "field assign" {
    const code =
        \\ let f = fn(n) = object {
        \\     number: n * n,
        \\ };
        \\ let g = fn() = {
        \\     let o = f(2);
        \\     print(o.number);
        \\     o.number = 12;
        \\     o;
        \\ };
        \\ 
        \\ let x = g();
        \\ x.number;
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: c00000000 (12)
        \\4 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "bin ops" {
    const code =
        \\ let f = fn(x, y) = {
        \\     print(x + y);
        \\     print(x * y);
        \\     print(x > y);
        \\     print(x < y);
        \\     print(x == y);
        \\     print(x != y);
        \\ };
        \\ 
        \\ f(1, 2);
        \\ f(2, 1);
        \\ f(1, 1);
        \\ f(2, 2);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\3 
        \\2 
        \\false 
        \\true 
        \\false 
        \\true 
        \\3 
        \\2 
        \\true 
        \\false 
        \\false 
        \\true 
        \\2 
        \\1 
        \\false 
        \\false 
        \\true 
        \\false 
        \\4 
        \\4 
        \\false 
        \\false 
        \\true 
        \\false 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "equal and not equals" {
    const code =
        \\ let f = fn(x, y) = {
        \\     print(x == y);
        \\     print(x != y);
        \\     print(y == x);
        \\     print(y != x);
        \\     print(x == x);
        \\     print(y == y);
        \\     print(x != x);
        \\     print(y != y);
        \\ };
        \\ 
        \\ let x = object { number: 1, };
        \\ f(1, 1);
        \\ f(1, 2);
        \\ f(1, nil);
        \\ f(nil, nil);
        \\ f(x, nil);
        \\ f(1, x);
        \\ f(x, x);
    ;
    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\true 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\false 
        \\false 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\true 
        \\false 
        \\false 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\true 
        \\false 
        \\false 
        \\true 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\false 
        \\false 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\true 
        \\false 
        \\false 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\true 
        \\false 
        \\false 
        \\true 
        \\false 
        \\true 
        \\false 
        \\true 
        \\true 
        \\false 
        \\false 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "blocks" {
    const code =
        \\ let f = fn() = { 
        \\     let x = 5;
        \\     {
        \\         let x = x + 1;
        \\         print("printing", x);
        \\     };
        \\     x;
        \\ };
        \\ 
        \\ f();
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 500000000 (5)
        \\printing 6 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "if-else chain" {
    const code =
        \\ let f = fn() = {
        \\     if (false) {
        \\         1;
        \\     } else if (true) {
        \\         2;
        \\     } else {
        \\         3;
        \\     };
        \\ };
        \\ 
        \\ 
        \\ f();
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 200000000 (2)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "allocations" {
    const code =
        \\ let f = fn() = {
        \\     let n = 100;
        \\     while(n > 0) {
        \\         let o = object {
        \\             number: 1,
        \\         };
        \\         print(o.number);
        \\         n = n - 1;
        \\     };
        \\ };
        \\ 
        \\ f();
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 0 (0)
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\1 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "bin tree" {
    const code =
        \\ let createnode = fn(val, left, right) = object {
        \\     val: val,
        \\     left: left,
        \\     right: right,
        \\ };
        \\ 
        \\ let max = fn(a,b) = if (a > b) a else b;
        \\ 
        \\ let size  = fn(node) = if (node == nil) 0 else 1 + size(node.left) + size(node.right);
        \\ let depth = fn(node) = if (node == nil) 0 else 1 + max(depth(node.left), depth(node.right));
        \\ 
        \\ let inOrder = fn(node) = {
        \\     if (node != nil) {
        \\         inOrder(node.left);
        \\         print(node.val);
        \\         inOrder(node.right);
        \\     };
        \\ };
        \\ 
        \\ let insertRec = fn(node, val) = {
        \\     if (node == nil) {
        \\         createnode(val, nil, nil);
        \\     } else {
        \\         if (val < node.val) {
        \\             node.left = insertRec(node.left, val);
        \\         } else {
        \\             node.right = insertRec(node.right, val);
        \\         };
        \\         node;
        \\     };
        \\ };
        \\ 
        \\ let binaryTree = fn() = object {
        \\     root: nil,
        \\ 
        \\     insert: fn(val) = { this.root = insertRec(this.root, val); },
        \\ 
        \\     debug: fn() = {
        \\         print("Size:", size(this.root));
        \\         print("Depth:", depth(this.root));
        \\         print("In-order traversal:");
        \\         inOrder(this.root);
        \\         print("(end)");
        \\     },
        \\ };
        \\ 
        \\ let tree = binaryTree();
        \\ tree.insert(10);
        \\ tree.insert(5);
        \\ tree.insert(15);
        \\ tree.insert(7);
        \\ tree.insert(3);
        \\ 
        \\ tree.debug();
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\Size: 5 
        \\Depth: 3 
        \\In-order traversal: 
        \\3 
        \\5 
        \\7 
        \\10 
        \\15 
        \\(end) 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "order of args" {
    const code =
        \\ let f = fn(a,b,c) = object {
        \\     a: a,
        \\     b: b,
        \\     c: c,
        \\ };
        \\ 
        \\ let g = fn(a, b, c) = {
        \\     f(a, b, c);
        \\ };
        \\ 
        \\ let o = g("a", "b", "c");
        \\ print(o.a);
        \\ print(o.b);
        \\ print(o.c);
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\a 
        \\b 
        \\c 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "char_to_int builtin" {
    const code =
        \\ let f = fn(c) = {
        \\     char_to_int(c);
        \\ };
        \\ 
        \\ print(f("a"));
        \\ print(f("b"));
        \\ print(f("c"));
        \\ print(f("z"));
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1 (0)
        \\97 
        \\98 
        \\99 
        \\122 
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}

test "reg stress test" {
    const code =
        \\ let add = fn(a, b) = {
        \\     a + b;
        \\ };
        \\ 
        \\ let g = fn() = { 
        \\     let a = 1;
        \\     let b = 1;
        \\     let c = 1;
        \\     let d = 1;
        \\     let e = 1;
        \\     let f = 1;
        \\     let g = 1;
        \\     let h = 1;
        \\     let i = 1;
        \\     let j = 1;
        \\     let k = 1;
        \\     let l = 1;
        \\     let m = 1;
        \\     let n = 1;
        \\     let o = 1;
        \\     let p = 1;
        \\     let q = 1;
        \\     let r = 1;
        \\     let s = 1;
        \\     let t = 1;
        \\     let u = 1;
        \\     let v = 1;
        \\     let w = 1;
        \\     let x = 1;
        \\     let y = 1;
        \\     let z = 1;
        \\ 
        \\     let tmp = add(z, y);
        \\     tmp = add(tmp, x);
        \\     tmp = add(tmp, w);
        \\     tmp = add(tmp, v);
        \\     tmp = add(tmp, u);
        \\     tmp = add(tmp, t);
        \\     tmp = add(tmp, s);
        \\     tmp = add(tmp, r);
        \\     tmp = add(tmp, q);
        \\     tmp = add(tmp, p);
        \\     tmp = add(tmp, o);
        \\     tmp = add(tmp, n);
        \\     tmp = add(tmp, m);
        \\     tmp = add(tmp, l);
        \\     tmp = add(tmp, k);
        \\     tmp = add(tmp, j);
        \\     tmp = add(tmp, i);
        \\     tmp = add(tmp, h);
        \\     tmp = add(tmp, g);
        \\     tmp = add(tmp, f);
        \\     tmp = add(tmp, e);
        \\     tmp = add(tmp, d);
        \\     tmp = add(tmp, c);
        \\     tmp = add(tmp, b);
        \\     tmp = add(tmp, a);
        \\ 
        \\     tmp;
        \\ };
        \\ 
        \\ g();
    ;

    var res = try test_helper(code[0..]);
    try snap.Snap.init(@src(),
        \\result: 1a00000000 (26)
        \\
        \\
    ).equal_fmt(res);
    res.deinit();
}
