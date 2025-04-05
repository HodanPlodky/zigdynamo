const std = @import("std");
const Parser = @import("parser.zig").Parser;
const compile = @import("compiler.zig").compile;
const BcInterpreter = @import("bc_interpreter.zig").BcInterpreter;
const JitInterpreter = @import("bc_interpreter.zig").JitInterpreter;
const Bytecode = @import("bytecode.zig").Bytecode;
const runtime = @import("runtime.zig");

fn run_with(comptime Interpret: type, bytecode: Bytecode, allocator: std.mem.Allocator, writer: std.io.AnyWriter) !runtime.Value {
    var runtime_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer runtime_arena.deinit();
    const alloc = runtime_arena.allocator();
    var interpret = Interpret.init(
        alloc,
        bytecode,
        try allocator.allocWithOptions(u8, 1024 * 10, 16, null),
        writer,
        .{ .call_count = 0 },
    );
    const val = interpret.run();
    return val;
}

fn test_helper(code: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(code, allocator);
    const prog = try p.parse();
    const bytecode = try compile(prog, allocator);

    var bc_writer = std.ArrayList(u8).init(std.testing.allocator);
    defer bc_writer.deinit();
    const bc_val = try run_with(
        BcInterpreter,
        bytecode,
        allocator,
        bc_writer.writer().any(),
    );
    var jit_writer = std.ArrayList(u8).init(std.testing.allocator);
    defer jit_writer.deinit();
    const jit_val = try run_with(
        JitInterpreter,
        bytecode,
        allocator,
        jit_writer.writer().any(),
    );
    try std.testing.expectEqual(bc_val.data, jit_val.data);
    try std.testing.expectEqualStrings(bc_writer.items, jit_writer.items);
}

test "basic" {
    const code =
        \\ let f = fn() = 1;
        \\ f();
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
}

test "basic arith" {
    const code =
        \\  1 + 2;
        \\ 1 +   2 * 2 - 3;
    ;
    try test_helper(code[0..]);
}

test "basic_closure" {
    const code =
        \\ let inc = fn(n) = fn(x) = n + x;
        \\ let inc1 = inc(1);
        \\ inc1(2);
    ;
    try test_helper(code[0..]);
}

test "basic_function" {
    const code =
        \\ let f = fn(n) = n + 1;
        \\ f(1);
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
}

test "if" {
    const code =
        \\ let x = true;
        \\ if (x) 1 else 2;
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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

    try test_helper(code[0..]);
}

test "number" {
    const code =
        \\ 1;
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
}

test "ret1" {
    const code =
        \\ let f = fn() = 1;
        \\ f();
    ;
    try test_helper(code[0..]);
}

test "retadd" {
    const code =
        \\ let f = fn() = 1 + 2 + 3;
        \\ f();
    ;
    try test_helper(code[0..]);
}

test "retbignum" {
    const code =
        \\ let f = fn() = 12345;
        \\ f();
    ;
    try test_helper(code[0..]);
}

test "retident" {
    const code =
        \\ let ident = fn(x) = x;
        \\ let f = fn(x, y) = x + 2 * y;
        \\ f(ident(2), ident(3));
    ;
    try test_helper(code[0..]);
}

test "retif" {
    const code =
        \\ let f = fn() = if (1 < 2) 1 else 2;
        \\ f();
    ;
    try test_helper(code[0..]);
}

test "retinnercall" {
    const code =
        \\ let inc = fn(x) = x + 1;
        \\ let double_inc = fn(x) = inc(inc(x));
        \\ double_inc(1);
    ;
    try test_helper(code[0..]);
}

test "retmul" {
    const code =
        \\ let f = fn() = 2 * 3 * 4;
        \\ f();
    ;
    try test_helper(code[0..]);
}

test "retobject" {
    const code =
        \\ let f = fn(n) = object {
        \\     n: n + 1,
        \\ };
        \\ 
        \\ f(1).n;
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
}

test "retstring" {
    const code =
        \\ let f = fn() = "hello";
        \\ f();
    ;
    try test_helper(code[0..]);
}

test "retsub" {
    const code =
        \\ let f = fn() = 10 - 1;
        \\ f();
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
}

test "division" {
    const code =
        \\ let f = fn(x) = x / 3;
        \\ print(f(7) * 3 != 7);
        \\ print(f(7) == 2);
        \\ f(7);
    ;
    try test_helper(code[0..]);
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
    try test_helper(code[0..]);
}
