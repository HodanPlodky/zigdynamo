const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const std = @import("std");

pub const ParserError = error{
    UnexpectedToken,
    Unimplemented,
};

pub const Parser = struct {
    lexer: lexer.Lexer,
    alloc: std.mem.Allocator,
    curr: lexer.Token,

    pub fn new(input: []const u8, alloc: std.mem.Allocator) Parser {
        var res = Parser{ .lexer = lexer.Lexer.new(input), .alloc = alloc, .curr = lexer.Token.eof };
        res.next();
        return res;
    }

    fn next(self: *Parser) void {
        self.curr = self.lexer.get_token();
        //std.debug.print("{}\n", .{self.curr});
    }

    fn pop(self: *Parser) lexer.Token {
        const tmp = self.curr;
        self.next();
        return tmp;
    }

    /// check if the tag of lexer is expected
    /// if yes then go to next token
    fn compare(self: *Parser, expected: lexer.Token) !void {
        if (std.meta.activeTag(self.curr) == std.meta.activeTag(expected)) {
            self.next();
        } else {
            return ParserError.UnexpectedToken;
        }
    }

    fn curr_is(self: *Parser, expected: lexer.Token) bool {
        return std.meta.activeTag(self.curr) == std.meta.activeTag(expected);
    }

    pub fn eof(self: *Parser) bool {
        return self.curr == lexer.Token.eof;
    }

    pub fn parse(self: *Parser) !ast.Program {
        var res = std.ArrayList(ast.Ast).init(self.alloc);
        while (!self.eof()) {
            try res.append(try self.expr());
            try self.compare(lexer.Token.semicol);
        }

        return ast.Program{ .data = res.items };
    }

    fn expr_ptr(self: *Parser) !*ast.Ast {
        const res = try self.alloc.create(ast.Ast);
        res.* = try self.expr();
        return res;
    }

    fn expr(self: *Parser) (std.mem.Allocator.Error || ParserError)!ast.Ast {
        return self.e_3();
    }

    fn e_3(self: *Parser) !ast.Ast {
        const left = try self.e_2();
        if (!self.curr_is(lexer.Token.assign)) {
            return left;
        }
        try self.compare(lexer.Token.assign);
        const right = try self.alloc.create(ast.Ast);
        right.* = try self.e_2();

        return switch (left) {
            ast.Ast.ident => |ident| ast.Ast{ .assign = ast.Assign{
                .target = ident,
                .value = right,
            } },
            ast.Ast.field_access => |field_access| ast.Ast{ .field_assign = ast.FieldAssign{
                .object = field_access.target,
                .field = field_access.field,
                .value = right,
            } },
            else => @panic("cannot assign into"),
        };
    }

    fn e_2(self: *Parser) !ast.Ast {
        var res = try self.e_1();
        while (true) {
            switch (self.curr) {
                lexer.Token.lt => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_1();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = '<',
                        .right = right,
                    } };
                },
                lexer.Token.gt => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_1();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = '>',
                        .right = right,
                    } };
                },
                lexer.Token.eq => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_1();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = 'e',
                        .right = right,
                    } };
                },
                lexer.Token.ne => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_1();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = 'n',
                        .right = right,
                    } };
                },
                else => break,
            }
        }
        return res;
    }

    fn e_1(self: *Parser) !ast.Ast {
        var res = try self.e_0();
        while (true) {
            switch (self.curr) {
                lexer.Token.add => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_0();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = '+',
                        .right = right,
                    } };
                },
                lexer.Token.sub => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_0();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = '-',
                        .right = right,
                    } };
                },
                else => break,
            }
        }
        return res;
    }

    fn e_0(self: *Parser) !ast.Ast {
        var res = try self.e_post();
        while (true) {
            switch (self.curr) {
                lexer.Token.mul => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_post();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = '*',
                        .right = right,
                    } };
                },
                lexer.Token.div => {
                    self.next();
                    const left = try self.alloc.create(ast.Ast);
                    left.* = res;
                    const right = try self.alloc.create(ast.Ast);
                    right.* = try self.e_post();
                    res = ast.Ast{ .binop = ast.BinOp{
                        .left = left,
                        .op = '/',
                        .right = right,
                    } };
                },
                else => break,
            }
        }
        return res;
    }

    fn e_post(self: *Parser) !ast.Ast {
        var result = try self.factor();
        while (true) {
            switch (self.curr) {
                lexer.Token.lparent => {
                    self.next();
                    var args = std.ArrayList(ast.Ast).init(self.alloc);
                    if (self.curr != lexer.Token.rparent) {
                        try args.append(try self.expr());

                        while (self.curr != lexer.Token.rparent) {
                            try self.compare(lexer.Token.comma);
                            try args.append(try self.expr());
                        }
                    }
                    try self.compare(lexer.Token.rparent);
                    const tmp = try self.alloc.create(ast.Ast);
                    tmp.* = result;
                    switch (tmp.*) {
                        ast.Ast.field_access => |field| {
                            result = ast.Ast{ .field_call = ast.FieldCall{
                                .target = field.target,
                                .field = field.field,
                                .args = args.items,
                            } };
                        },
                        else => {
                            result = ast.Ast{ .call = ast.Call{
                                .target = tmp,
                                .args = args.items,
                            } };
                        },
                    }
                },
                lexer.Token.dot => {
                    self.next();
                    const field = try self.parse_ident();
                    const tmp = try self.alloc.create(ast.Ast);
                    tmp.* = result;
                    result = ast.Ast{ .field_access = ast.FieldAccess{
                        .target = tmp,
                        .field = field,
                    } };
                },
                else => break,
            }
        }
        return result;
    }

    fn factor(self: *Parser) !ast.Ast {
        switch (self.pop()) {
            lexer.Token.number => |num| return ast.Ast{ .number = num },
            lexer.Token.ident => |ident| return ast.Ast{ .ident = ident },
            lexer.Token.kwlet => return self.parse_let(),
            lexer.Token.kwobject => return self.parse_object(),
            lexer.Token.kwnil => return ast.Ast.nil,
            lexer.Token.kwfn => return self.parse_function(),
            lexer.Token.lcurly => return self.parse_block(),
            lexer.Token.kwif => return self.parse_if(),
            lexer.Token.kwwhile => return self.parse_while(),
            lexer.Token.kwtrue => return ast.Ast{ .bool = true },
            lexer.Token.kwfalse => return ast.Ast{ .bool = false },
            lexer.Token.string => |value| return ast.Ast{ .string = value },
            lexer.Token.kwprint => return ast.Ast.print_fn,
            else => return ParserError.UnexpectedToken,
        }
    }

    fn parse_let(self: *Parser) !ast.Ast {
        switch (self.pop()) {
            lexer.Token.ident => |ident| {
                try self.compare(lexer.Token.assign);
                const val = try self.expr_ptr();
                return ast.Ast{ .let = ast.Let{
                    .target = ident,
                    .value = val,
                } };
            },
            else => return ParserError.UnexpectedToken,
        }
        return ParserError.UnexpectedToken;
    }

    fn parse_object(self: *Parser) !ast.Ast {
        var prototype: ?*ast.Ast = null;
        if (self.curr_is(lexer.Token.lparent)) {
            self.next();
            prototype = try self.expr_ptr();
            try self.compare(lexer.Token.rparent);
        }
        try self.compare(lexer.Token.lcurly);

        var fields = std.ArrayList(ast.Field).init(self.alloc);
        while (!self.curr_is(lexer.Token.rcurly)) {
            const name = try self.parse_ident();
            try self.compare(lexer.Token.colon);
            const value = try self.expr_ptr();
            try self.compare(lexer.Token.comma);
            try fields.append(ast.Field{
                .name = name,
                .value = value,
            });
        }
        try self.compare(lexer.Token.rcurly);
        return ast.Ast{ .object = ast.Object{
            .prototype = prototype,
            .fields = fields.items,
        } };
    }

    fn parse_ident(self: *Parser) ![]const u8 {
        switch (self.pop()) {
            lexer.Token.ident => |ident| return ident,
            else => return ParserError.UnexpectedToken,
        }
        return ParserError.UnexpectedToken;
    }

    fn parse_function(self: *Parser) !ast.Ast {
        try self.compare(lexer.Token.lparent);

        var args = std.ArrayList([]const u8).init(self.alloc);
        if (!self.curr_is(lexer.Token.rparent)) {
            while (true) {
                try args.append(try self.parse_ident());
                if (!self.curr_is(lexer.Token.comma)) {
                    break;
                }
                try self.compare(lexer.Token.comma);
            }
        }
        try self.compare(lexer.Token.rparent);
        try self.compare(lexer.Token.assign);
        const body = try self.expr_ptr();
        return ast.Ast{ .function = ast.Function{
            .body = body,
            .params = args.items,
        } };
    }

    pub fn parse_block(self: *Parser) !ast.Ast {
        var exprs = std.ArrayList(ast.Ast).init(self.alloc);
        while (!self.curr_is(lexer.Token.rcurly)) {
            try exprs.append(try self.expr());
            try self.compare(lexer.Token.semicol);
        }
        try self.compare(lexer.Token.rcurly);
        return ast.Ast{ .block = exprs.items };
    }

    pub fn parse_if(self: *Parser) !ast.Ast {
        try self.compare(lexer.Token.lparent);
        const cond = try self.expr_ptr();
        try self.compare(lexer.Token.rparent);
        const then_block = try self.expr_ptr();
        if (!self.curr_is(lexer.Token.kwelse)) {
            return ast.Ast{ .condition = ast.Condition{
                .cond = cond,
                .then_block = then_block,
                .else_block = null,
            } };
        }
        try self.compare(lexer.Token.kwelse);
        const else_block = try self.expr_ptr();
        return ast.Ast{ .condition = ast.Condition{
            .cond = cond,
            .then_block = then_block,
            .else_block = else_block,
        } };
    }

    pub fn parse_while(self: *Parser) !ast.Ast {
        try self.compare(lexer.Token.lparent);
        const cond = try self.expr_ptr();
        try self.compare(lexer.Token.rparent);
        const body = try self.expr_ptr();
        return ast.Ast{ .loop = ast.Loop{
            .cond = cond,
            .body = body,
        } };
    }
};

const ohsnap = @import("ohsnap");

test "parser test basic" {
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new("1 +   2 * 2 - 3;", allocator);
    const res = try p.parse();
    try oh.snap(
        @src(),
        \\ast.Program
        \\  .data: []ast.Ast
        \\    [0]: ast.Ast
        \\      .binop: ast.BinOp
        \\        .op: u8 = 45
        \\        .left: *ast.Ast
        \\          .binop: ast.BinOp
        \\            .op: u8 = 43
        \\            .left: *ast.Ast
        \\              .number: u32 = 1
        \\            .right: *ast.Ast
        \\              .binop: ast.BinOp
        \\                .op: u8 = 42
        \\                .left: *ast.Ast
        \\                  .number: u32 = 2
        \\                .right: *ast.Ast
        \\                  .number: u32 = 2
        \\        .right: *ast.Ast
        \\          .number: u32 = 3
        ,
    ).expectEqual(res);
}

test "test let" {
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let x = 1;
        \\ x + 1;
    , allocator);
    const res = try p.parse();
    try oh.snap(
        @src(),
        \\ast.Program
        \\  .data: []ast.Ast
        \\    [0]: ast.Ast
        \\      .let: ast.Let
        \\        .target: []const u8
        \\          "x"
        \\        .value: *ast.Ast
        \\          .number: u32 = 1
        \\    [1]: ast.Ast
        \\      .binop: ast.BinOp
        \\        .op: u8 = 43
        \\        .left: *ast.Ast
        \\          .ident: []const u8
        \\            "x"
        \\        .right: *ast.Ast
        \\          .number: u32 = 1
        ,
    ).expectEqual(res);
}

test "test function" {
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let f = fn(n) = 1 * f(n-1) + n;
        \\ f(f(10) + 1);
    , allocator);
    const res = try p.parse();
    try oh.snap(
        @src(),
        \\ast.Program
        \\  .data: []ast.Ast
        \\    [0]: ast.Ast
        \\      .let: ast.Let
        \\        .target: []const u8
        \\          "f"
        \\        .value: *ast.Ast
        \\          .function: ast.Function
        \\            .params: [][]const u8
        \\              [0]: []const u8
        \\                "n"
        \\            .body: *ast.Ast
        \\              .binop: ast.BinOp
        \\                .op: u8 = 43
        \\                .left: *ast.Ast
        \\                  .binop: ast.BinOp
        \\                    .op: u8 = 42
        \\                    .left: *ast.Ast
        \\                      .number: u32 = 1
        \\                    .right: *ast.Ast
        \\                      .call: ast.Call
        \\                        .target: *ast.Ast
        \\                          .ident: []const u8
        \\                            "f"
        \\                        .args: []ast.Ast
        \\                          [0]: ast.Ast
        \\                            .binop: ast.BinOp
        \\                              .op: u8 = 45
        \\                              .left: *ast.Ast
        \\                                .ident: []const u8
        \\                                  "n"
        \\                              .right: *ast.Ast
        \\                                .number: u32 = 1
        \\                .right: *ast.Ast
        \\                  .ident: []const u8
        \\                    "n"
        \\    [1]: ast.Ast
        \\      .call: ast.Call
        \\        .target: *ast.Ast
        \\          .ident: []const u8
        \\            "f"
        \\        .args: []ast.Ast
        \\          [0]: ast.Ast
        \\            .binop: ast.BinOp
        \\              .op: u8 = 43
        \\              .left: *ast.Ast
        \\                .call: ast.Call
        \\                  .target: *ast.Ast
        \\                    .ident: []const u8
        \\                      "f"
        \\                  .args: []ast.Ast
        \\                    [0]: ast.Ast
        \\                      .number: u32 = 10
        \\              .right: *ast.Ast
        \\                .number: u32 = 1
        ,
    ).expectEqual(res);
}

test "test if" {
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ if (true) 1 else 2;
    , allocator);
    const res = try p.parse();
    try oh.snap(@src(),
        \\ast.Program
        \\  .data: []ast.Ast
        \\    [0]: ast.Ast
        \\      .condition: ast.Condition
        \\        .cond: *ast.Ast
        \\          .bool: bool = true
        \\        .then_block: *ast.Ast
        \\          .number: u32 = 1
        \\        .else_block: ?*ast.Ast
        \\          .number: u32 = 2
    ).expectEqual(res);
}

test "test while" {
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let cond = true;
        \\ let x = 1;
        \\ while (cond) {
        \\      x = x + 1;
        \\      print("x", x);
        \\ };
    , allocator);
    const res = try p.parse();
    try oh.snap(@src(),
        \\ast.Program
        \\  .data: []ast.Ast
        \\    [0]: ast.Ast
        \\      .let: ast.Let
        \\        .target: []const u8
        \\          "cond"
        \\        .value: *ast.Ast
        \\          .bool: bool = true
        \\    [1]: ast.Ast
        \\      .let: ast.Let
        \\        .target: []const u8
        \\          "x"
        \\        .value: *ast.Ast
        \\          .number: u32 = 1
        \\    [2]: ast.Ast
        \\      .loop: ast.Loop
        \\        .cond: *ast.Ast
        \\          .ident: []const u8
        \\            "cond"
        \\        .body: *ast.Ast
        \\          .block: []ast.Ast
        \\            [0]: ast.Ast
        \\              .assign: ast.Assign
        \\                .target: []const u8
        \\                  "x"
        \\                .value: *ast.Ast
        \\                  .binop: ast.BinOp
        \\                    .op: u8 = 43
        \\                    .left: *ast.Ast
        \\                      .ident: []const u8
        \\                        "x"
        \\                    .right: *ast.Ast
        \\                      .number: u32 = 1
        \\            [1]: ast.Ast
        \\              .call: ast.Call
        \\                .target: *ast.Ast
        \\                  .print_fn: void = void
        \\                .args: []ast.Ast
        \\                  [0]: ast.Ast
        \\                    .string: []const u8
        \\                      "x"
        \\                  [1]: ast.Ast
        \\                    .ident: []const u8
        \\                      "x"
    ).expectEqual(res);
}

test "test objects" {
    const oh = ohsnap{};
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var p = Parser.new(
        \\ let x = object {
        \\      a: 1,
        \\      f: fn() = {
        \\          this.a + 1;
        \\      },
        \\ };
        \\ let y = object(x) {
        \\      b: 2,
        \\      g: fn() = {
        \\          this.f() + this.b;
        \\      },
        \\ };
    , allocator);
    const res = try p.parse();
    try oh.snap(@src(),
        \\ast.Program
        \\  .data: []ast.Ast
        \\    [0]: ast.Ast
        \\      .let: ast.Let
        \\        .target: []const u8
        \\          "x"
        \\        .value: *ast.Ast
        \\          .object: ast.Object
        \\            .prototype: ?*ast.Ast
        \\              null
        \\            .fields: []ast.Field
        \\              [0]: ast.Field
        \\                .name: []const u8
        \\                  "a"
        \\                .value: *ast.Ast
        \\                  .number: u32 = 1
        \\              [1]: ast.Field
        \\                .name: []const u8
        \\                  "f"
        \\                .value: *ast.Ast
        \\                  .function: ast.Function
        \\                    .params: [][]const u8
        \\                      (empty)
        \\                    .body: *ast.Ast
        \\                      .block: []ast.Ast
        \\                        [0]: ast.Ast
        \\                          .binop: ast.BinOp
        \\                            .op: u8 = 43
        \\                            .left: *ast.Ast
        \\                              .field_access: ast.FieldAccess
        \\                                .target: *ast.Ast
        \\                                  .ident: []const u8
        \\                                    "this"
        \\                                .field: []const u8
        \\                                  "a"
        \\                            .right: *ast.Ast
        \\                              .number: u32 = 1
        \\    [1]: ast.Ast
        \\      .let: ast.Let
        \\        .target: []const u8
        \\          "y"
        \\        .value: *ast.Ast
        \\          .object: ast.Object
        \\            .prototype: ?*ast.Ast
        \\              .ident: []const u8
        \\                "x"
        \\            .fields: []ast.Field
        \\              [0]: ast.Field
        \\                .name: []const u8
        \\                  "b"
        \\                .value: *ast.Ast
        \\                  .number: u32 = 2
        \\              [1]: ast.Field
        \\                .name: []const u8
        \\                  "g"
        \\                .value: *ast.Ast
        \\                  .function: ast.Function
        \\                    .params: [][]const u8
        \\                      (empty)
        \\                    .body: *ast.Ast
        \\                      .block: []ast.Ast
        \\                        [0]: ast.Ast
        \\                          .binop: ast.BinOp
        \\                            .op: u8 = 43
        \\                            .left: *ast.Ast
        \\                              .field_call: ast.FieldCall
        \\                                .target: *ast.Ast
        \\                                  .ident: []const u8
        \\                                    "this"
        \\                                .field: []const u8
        \\                                  "f"
        \\                                .args: []ast.Ast
        \\                                  (empty)
        \\                            .right: *ast.Ast
        \\                              .field_access: ast.FieldAccess
        \\                                .target: *ast.Ast
        \\                                  .ident: []const u8
        \\                                    "this"
        \\                                .field: []const u8
        \\                                  "b"
    ).expectEqual(res);
}
