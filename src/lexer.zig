const std = @import("std");

pub const Token = union(enum) {
    // values
    number: u32,
    string: []const u8,

    // Parents
    lparent: void,
    rparent,
    lsquare,
    rsquare,
    lcurly,
    rcurly,

    // Operators
    add,
    sub,
    mul,
    div,
    eq,
    ne,
    assign,
    lt,
    gt,

    // Ident
    ident: []const u8,
    // Keywords
    kwfn,
    kwif,
    kwelse,
    kwlet,
    kwobject,
    kwnil,
    kwtrue,
    kwfalse,
    kwprint,
    kwwhile,

    // other
    comma,
    colon,
    semicol,
    dot,

    // Meta
    eof,
    wrongtok,
};

pub const Loc = struct {
    row: usize = 1,
    col: usize = 0,

    fn new_line(self: *Loc) void {
        self.row += 1;
        self.col = 0;
    }

    fn next(self: *Loc) void {
        self.col += 1;
    }
};

pub const Lexer = struct {
    loc: Loc,
    position: usize,
    input: []const u8,

    pub fn new(input: []const u8) Lexer {
        return Lexer{ .loc = .{}, .position = 0, .input = input };
    }

    pub fn position(self: *const Lexer) usize {
        return self.position;
    }

    fn peek(self: *const Lexer) u8 {
        return self.input[self.position];
    }

    fn eof(self: *const Lexer) bool {
        return self.position >= self.input.len;
    }

    fn next(self: *Lexer) void {
        self.position += 1;
        self.loc.next();
    }

    pub fn get_token(self: *Lexer) Token {
        if (self.eof()) {
            return Token.eof;
        }

        while (!self.eof()) {
            switch (self.peek()) {
                '0' => return {
                    self.next();
                    if ('0' <= self.peek() and self.peek() <= '9') {
                        return Token.wrongtok;
                    }
                    return Token{ .number = 0 };
                },
                '1'...'9' => return Token{ .number = self.number() },
                '(' => {
                    self.next();
                    return Token{ .lparent = {} };
                },
                ')' => {
                    self.next();
                    return Token{ .rparent = {} };
                },
                '[' => {
                    self.next();
                    return Token{ .lsquare = {} };
                },
                ']' => {
                    self.next();
                    return Token{ .rsquare = {} };
                },
                '{' => {
                    self.next();
                    return Token{ .lcurly = {} };
                },
                '}' => {
                    self.next();
                    return Token{ .rcurly = {} };
                },

                '+' => {
                    self.next();
                    return Token{ .add = {} };
                },
                '-' => {
                    self.next();
                    return Token{ .sub = {} };
                },
                '*' => {
                    self.next();
                    return Token{ .mul = {} };
                },
                '<' => {
                    self.next();
                    return Token{ .lt = {} };
                },
                '>' => {
                    self.next();
                    return Token{ .gt = {} };
                },
                '/' => {
                    self.next();
                    return Token{ .div = {} };
                },
                ',' => {
                    self.next();
                    return Token{ .comma = {} };
                },
                ':' => {
                    self.next();
                    return Token{ .colon = {} };
                },
                ';' => {
                    self.next();
                    return Token{ .semicol = {} };
                },
                '.' => {
                    self.next();
                    return Token{ .dot = {} };
                },
                '=' => {
                    self.next();
                    if (self.peek() == '=') {
                        self.next();
                        return Token{ .eq = {} };
                    } else {
                        return Token{ .assign = {} };
                    }
                },
                '!' => {
                    self.next();
                    if (self.peek() != '=') {
                        return Token.wrongtok;
                    }
                    self.next();
                    return Token.ne;
                },

                'a'...'z', 'A'...'Z', '_' => {
                    const identfier = self.ident();
                    return Lexer.check_kw(identfier);
                },

                '\"' => {
                    self.next();
                    return Token{ .string = self.string() };
                },

                // white space
                '\n' => {
                    self.next();
                    self.loc.new_line();
                },
                ' ', '\t', '\r' => self.next(),
                else => return Token{ .wrongtok = {} },
            }
        }
        return Token{ .eof = {} };
    }

    fn number(self: *Lexer) u32 {
        var res: u32 = 0;
        while (!self.eof() and '0' <= self.peek() and self.peek() <= '9') {
            res *= 10;
            res += self.peek() - '0';
            self.next();
        }
        return res;
    }

    fn ident(self: *Lexer) []const u8 {
        const start = self.position;
        var end = start;
        while (!self.eof()) {
            switch (self.peek()) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => end += 1,
                else => break,
            }
            self.next();
        }

        return self.input[start..end];
    }

    fn string(self: *Lexer) []const u8 {
        const start = self.position;
        var end = start;
        while (self.peek() != '\"') {
            self.next();
            end += 1;
        }
        self.next();
        return self.input[start..end];
    }

    fn check_kw(identifier: []const u8) Token {
        if (std.mem.eql(u8, "if", identifier)) {
            return Token{ .kwif = {} };
        } else if (std.mem.eql(u8, "while", identifier)) {
            return Token{ .kwwhile = {} };
        } else if (std.mem.eql(u8, "fn", identifier)) {
            return Token{ .kwfn = {} };
        } else if (std.mem.eql(u8, "let", identifier)) {
            return Token{ .kwlet = {} };
        } else if (std.mem.eql(u8, "object", identifier)) {
            return Token{ .kwobject = {} };
        } else if (std.mem.eql(u8, "nil", identifier)) {
            return Token{ .kwnil = {} };
        } else if (std.mem.eql(u8, "else", identifier)) {
            return Token{ .kwelse = {} };
        } else if (std.mem.eql(u8, "true", identifier)) {
            return Token{ .kwtrue = {} };
        } else if (std.mem.eql(u8, "false", identifier)) {
            return Token{ .kwfalse = {} };
        } else if (std.mem.eql(u8, "print", identifier)) {
            return Token{ .kwprint = {} };
        } else {
            return Token{ .ident = identifier };
        }
    }
};

test "lexer" {
    const ohsnap = @import("ohsnap");
    const oh = ohsnap{};

    var lexer = Lexer.new("1+2");
    var res = std.ArrayList(Token).init(std.testing.allocator);
    defer res.deinit();
    while (true) {
        const tok = lexer.get_token();
        try res.append(tok);
        switch (tok) {
            Token.eof => break,
            Token.wrongtok => break,
            else => {},
        }
    }

    try oh.snap(
        @src(),
        \\[]lexer.Token
        \\  [0]: lexer.Token
        \\    .number: u32 = 1
        \\  [1]: lexer.Token
        \\    .add: void = void
        \\  [2]: lexer.Token
        \\    .number: u32 = 2
        \\  [3]: lexer.Token
        \\    .eof: void = void
        ,
    ).expectEqual(res.items);
}
