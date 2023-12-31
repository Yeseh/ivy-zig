const std = @import("std");

pub const EOF_CHAR: u8 = 0;
pub const ParsePredicate = fn (u8) bool;

pub const Token = struct {
    const Self = @This();

    type: TokenType,
    lex: []const u8,
    line: u32,

    pub fn dump(self: *Self) void {
        std.debug.print("{} '{s}'", .{ @intFromEnum(self.type), self.lex });
    }
};

pub const TokenType = enum(u8) {
    // Single char
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    STAR,
    FSLASH,
    BSLASH,
    SEMICOLON,

    // One or two char
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FN,
    IF,
    NIL,
    OR,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    CONST,
    WHILE,

    ERROR,
    EOF,
};

fn ite(cond: bool, then: TokenType, els: TokenType) TokenType {
    if (cond) {
        return then;
    } else {
        return els;
    }
}

pub const Scanner = struct {
    const Self = @This();

    alloc: std.mem.Allocator,
    start: [:0]const u8,
    current: [:0]const u8,
    line: u32,

    pub fn init(alloc: std.mem.Allocator, source: [:0]const u8) !Self {
        return Self{ .alloc = alloc, .start = source, .current = source, .line = 1 };
    }

    pub fn scan_token(self: *Self) Token {
        self.skip_whitespace();
        if (self.is_end()) {
            return self.make_token(TokenType.EOF);
        }

        self.start = self.current;

        const c = self.adv();
        if (std.ascii.isAlphabetic(c) or c == '_') {
            return self.iden();
        }
        if (std.ascii.isDigit(c)) {
            return self.number();
        }

        const token = switch (c) {
            '(' => self.make_token(TokenType.LPAREN),
            ')' => self.make_token(TokenType.RPAREN),
            '{' => self.make_token(TokenType.LBRACE),
            '}' => self.make_token(TokenType.RBRACE),
            ';' => self.make_token(TokenType.SEMICOLON),
            ',' => self.make_token(TokenType.COMMA),
            '.' => self.make_token(TokenType.DOT),
            '-' => self.make_token(TokenType.MINUS),
            '*' => self.make_token(TokenType.STAR),
            '+' => self.make_token(TokenType.PLUS),
            '/' => self.make_token(TokenType.FSLASH),
            '\\' => self.make_token(TokenType.BSLASH),
            '"' => self.string(),
            '!' => self.make_token(ite(self.match('='), TokenType.BANG_EQUAL, TokenType.BANG)),
            '=' => self.make_token(ite(self.match('='), TokenType.EQUAL_EQUAL, TokenType.EQUAL)),
            '<' => self.make_token(ite(self.match('='), TokenType.LESS_EQUAL, TokenType.LESS)),
            '>' => self.make_token(ite(self.match('='), TokenType.GREATER_EQUAL, TokenType.GREATER)),
            else => bla: {
                std.debug.print("Unexpected character '{c}'.", .{c});
                break :bla self.error_token("Unexpected character");
            },
        };
        return token;
    }

    fn peek(self: *Self) u8 {
        return self.current[0];
    }

    fn peekN(self: *Self, n: u8) []const u8 {
        const end = 1 + n;
        return self.current[1..end];
    }

    /// match multiple chars for complex tokens
    fn matchN(self: *Self, expected: []const u8) bool {
        const cmp = self.current[0..expected.len];
        const eql = std.mem.eql(u8, expected, cmp);
        if (!eql) {
            return false;
        }

        self.current = self.current[expected.len - 1 ..];
        return true;
    }

    fn match(self: *Self, expected: u8) bool {
        const cmp = self.current[0];
        if (expected != cmp) {
            return false;
        }

        self.current = self.current[1..];
        return true;
    }

    pub fn skip_whitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.adv();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.adv();
                },
                '/' => {
                    if (std.mem.eql(u8, self.peekN(1), "/")) {
                        while (self.peek() != '\n' and !self.is_end()) {
                            _ = self.adv();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    // TODO: Support string interpl
    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.is_end()) {
            // TODO: Handle escape chars
            // if (std.mem.eql(u8, self.peekN(2), "\\\"")) {
            //     _ = self.adv();
            // }

            if (self.peek() == '\n') {
                self.line += 1;
            }

            _ = self.adv();
        }

        if (self.is_end()) {
            return self.error_token("Unterminated string.");
        }

        _ = self.adv();
        return self.make_token(TokenType.STRING);
    }

    fn iden(self: *Self) Token {
        while (std.ascii.isAlphabetic(self.peek()) or std.ascii.isDigit(self.peek())) {
            _ = self.adv();
        }

        const tt = switch (self.start[0]) {
            'a' => self.check_keyword(1, 2, "nd", TokenType.AND),
            'c' => blk: {
                if (self.current_diff() > 1) {
                    break :blk switch (self.start[1]) {
                        'o' => self.check_keyword(2, 3, "nst", TokenType.CONST),
                        'l' => self.check_keyword(2, 3, "ass", TokenType.CLASS),
                        else => TokenType.IDENTIFIER,
                    };
                } else {
                    break :blk TokenType.IDENTIFIER;
                }
            },
            'e' => self.check_keyword(1, 3, "lse", TokenType.ELSE),
            'i' => self.check_keyword(1, 1, "f", TokenType.IF),
            'n' => self.check_keyword(1, 2, "il", TokenType.NIL),
            'o' => self.check_keyword(1, 1, "r", TokenType.OR),
            'r' => self.check_keyword(1, 5, "eturn", TokenType.RETURN),
            's' => self.check_keyword(1, 4, "uper", TokenType.SUPER),
            'v' => self.check_keyword(1, 2, "ar", TokenType.VAR),
            'w' => self.check_keyword(1, 4, "hile", TokenType.WHILE),
            'f' => blk: {
                if (self.current_diff() > 1) {
                    break :blk switch (self.start[1]) {
                        'n' => TokenType.FN,
                        'a' => self.check_keyword(2, 3, "lse", TokenType.FALSE),
                        'o' => self.check_keyword(2, 1, "r", TokenType.FOR),
                        else => TokenType.IDENTIFIER,
                    };
                } else {
                    break :blk TokenType.IDENTIFIER;
                }
            },
            't' => blk: {
                if (self.current_diff() > 1) {
                    break :blk switch (self.start[1]) {
                        'h' => self.check_keyword(2, 2, "is", TokenType.THIS),
                        'r' => self.check_keyword(2, 2, "ue", TokenType.TRUE),
                        else => TokenType.IDENTIFIER,
                    };
                } else {
                    break :blk TokenType.IDENTIFIER;
                }
            },
            else => TokenType.IDENTIFIER,
        };

        return self.make_token(tt);
    }

    fn current_diff(self: *Self) usize {
        return @intFromPtr(self.current.ptr) - @intFromPtr(self.start.ptr);
    }

    fn check_keyword(self: *Self, start: u8, length: u8, rest: []const u8, ty: TokenType) TokenType {
        const len = self.current_diff() == start + length;
        const cmp = self.start[start .. start + length];
        const eql = std.mem.eql(u8, cmp, rest);

        if (len and eql) {
            return ty;
        }

        return TokenType.IDENTIFIER;
    }

    fn number(self: *Self) Token {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.adv();
        }

        const pk = self.peek();

        if (pk == '.') {
            _ = self.adv();
            while (!self.is_end() and std.ascii.isDigit(self.peek())) {
                _ = self.adv();
            }
        }

        return self.make_token(TokenType.NUMBER);
    }

    fn error_token(self: *Self, msg: []const u8) Token {
        return Token{
            .type = TokenType.ERROR,
            .lex = msg,
            .line = self.line,
        };
    }

    fn make_token(self: *Self, tt: TokenType) Token {
        const end = @intFromPtr(self.current.ptr) - @intFromPtr(self.start.ptr);
        return Token{
            .type = tt,
            .lex = self.start[0..end],
            .line = self.line,
        };
    }

    fn adv(self: *Self) u8 {
        const c = self.current[0];
        self.current = self.current[1..];
        return c;
    }

    fn is_end(self: *Self) bool {
        return self.current[0] == EOF_CHAR;
    }
};

// TODO: Rewrite these with scanner rewrite
test "Scanner.basic" {
    const alloc = std.testing.allocator;
    {
        var scan = try Scanner.init(alloc, "(){};,.+-*/\\");
        const expected = [_]TokenType{
            .LPAREN, .RPAREN, .LBRACE, .RBRACE, .SEMICOLON, .COMMA, .DOT,
            .PLUS,   .MINUS,  .STAR,   .FSLASH, .BSLASH,    .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            //std.debug.print("Expected: {any}, actual: {any}\n", .{ tt, token.type });
            try std.testing.expect(token.type == tt);
        }
    }
    {
        var scan = try Scanner.init(alloc, "! != = == < <= > >=");
        const expected = [_]TokenType{
            .BANG, .BANG_EQUAL, .EQUAL, .EQUAL_EQUAL, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL, .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            // std.debug.print("Expected: {any}, actual: {any}\n", .{ tt, token.type });
            if (token.type == TokenType.ERROR) {
                std.debug.print(" '{s}'\n", .{token.lex});
            }
            try std.testing.expect(token.type == tt);
        }
    }
    {
        var scan = try Scanner.init(alloc, "9 19 19.8 1111.8888");
        const expected = [_]TokenType{
            .NUMBER, .NUMBER, .NUMBER, .NUMBER, .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            //std.debug.print("Expected: {any}, actual: {any}\n", .{ tt, token.type });
            if (token.type == TokenType.ERROR) {
                std.debug.print(" '{s}'\n", .{token.lex});
            }
            try std.testing.expect(token.type == tt);
        }
    }
    {
        var scan = try Scanner.init(alloc, "and class else false for fun if nil or return super this true var while blabla");
        const expected = [_]TokenType{
            .AND, .CLASS, .ELSE, .FALSE, .FOR, .FN, .IF, .NIL, .OR, .RETURN, .SUPER, .THIS, .TRUE, .VAR, .WHILE, .IDENTIFIER, .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            //std.debug.print("Expected: {any}, actual: {any}\n", .{ tt, token.type });
            if (token.type == TokenType.ERROR) {
                std.debug.print(" '{s}'\n", .{token.lex});
            }
            try std.testing.expect(token.type == tt);
        }
    }
    {
        var scan = try Scanner.init(alloc, "1+2/3*4-5");
        const expected = [_]TokenType{
            .NUMBER, .PLUS, .NUMBER, .FSLASH, .NUMBER, .STAR, .NUMBER, .MINUS, .NUMBER, .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            //std.debug.print("Expected: {any}, actual: {any}\n", .{ tt, token.type });
            if (token.type == TokenType.ERROR) {
                std.debug.print(" '{s}'\n", .{token.lex});
            }
            try std.testing.expect(token.type == tt);
        }
    }
    {
        var scan = try Scanner.init(alloc, "var bla = true;\n");
        const expected = [_]TokenType{
            .VAR, .IDENTIFIER, .EQUAL, .TRUE, .SEMICOLON, .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            std.debug.print("Expected: {any}, actual: {any}\n", .{ tt, token.type });
            if (token.type == TokenType.ERROR) {
                std.debug.print(" '{s}'\n", .{token.lex});
            }
            try std.testing.expect(token.type == tt);
        }
    }
}
