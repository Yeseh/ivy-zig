const std = @import("std");

pub const Token = struct {
    const Self = @This();

    type: TokenType,
    lex: []const u8,
    line: i32,

    pub fn dump(self: *Self) void {
        std.debug.print("{} '{s}'", .{ @enumToInt(self.type), self.lex });
    }
};

pub const TokenType = enum {
    // Single char
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    STAR,
    SLASH,

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
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
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
    line: i32,

    pub fn init(alloc: std.mem.Allocator, source: [:0]const u8) !Self {
        return Self{ .alloc = alloc, .start = source, .current = source, .line = 1 };
    }

    pub fn scan_token(self: *Self) Token {
        std.debug.print("current: {c}\n", .{self.current[0]});

        self.skip_whitespace();
        self.start = self.current;
        if (self.is_end()) {
            return self.make_token(TokenType.EOF);
        }

        const c = self.adv();
        if (std.ascii.isAlphabetic(c) or c == '_') {
            return self.iden();
        }
        if (std.ascii.isDigit(c)) {
            return self.number();
        }

        switch (c) {
            '(' => return self.make_token(TokenType.LPAREN),
            ')' => return self.make_token(TokenType.RPAREN),
            '{' => return self.make_token(TokenType.LBRACE),
            '}' => return self.make_token(TokenType.RBRACE),
            ';' => return self.make_token(TokenType.SEMICOLON),
            ',' => return self.make_token(TokenType.COMMA),
            '.' => return self.make_token(TokenType.DOT),
            '-' => return self.make_token(TokenType.MINUS),
            '*' => return self.make_token(TokenType.STAR),
            '+' => return self.make_token(TokenType.PLUS),
            '/' => return self.make_token(TokenType.SLASH),
            '!' => return self.make_token(ite(self.match('='), TokenType.BANG_EQUAL, TokenType.BANG)),
            '=' => return self.make_token(ite(self.match('='), TokenType.EQUAL_EQUAL, TokenType.EQUAL)),
            '<' => return self.make_token(ite(self.match('='), TokenType.LESS_EQUAL, TokenType.LESS)),
            '>' => return self.make_token(ite(self.match('='), TokenType.GREATER_EQUAL, TokenType.GREATER)),
            else => return self.error_token("Unexpected character."),
        }

        return self.error_token();
    }

    fn peek(self: *Self) u8 {
        return self.current[0];
    }

    fn peekN(self: *Self, n: u8) []const u8 {
        if (self.current.len < n) {
            return "";
        }
        var end = 1 + n;
        return self.current[1..end];
    }

    fn matchN(self: *Self, expected: []const u8) bool {
        if (self.is_end()) {
            return false;
        }

        var cmp = self.current[0..expected.len];
        var eql = std.mem.eql(u8, expected, cmp);
        if (!eql) {
            return false;
        }

        self.current = self.current[expected.len - 1 ..];
        return true;
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.is_end()) {
            return false;
        }

        var cmp = self.current[0];
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

    fn iden(self: *Self) Token {
        var pk = self.peek();
        while (std.ascii.isAlphabetic(pk) or std.ascii.isDigit(pk)) {
            std.debug.print("pk: {c}\n", .{pk});
            _ = self.adv();
            std.debug.print("pk: {c}\n", .{self.peek()});
        }

        var tt = switch (self.start[0]) {
            'a' => self.check_keyword(1, 2, "nd", TokenType.AND),
            'c' => self.check_keyword(1, 4, "lass", TokenType.CLASS),
            'e' => self.check_keyword(1, 3, "lse", TokenType.ELSE),
            'i' => self.check_keyword(1, 1, "f", TokenType.IF),
            'n' => self.check_keyword(1, 2, "il", TokenType.NIL),
            'o' => self.check_keyword(1, 1, "r", TokenType.OR),
            'p' => self.check_keyword(1, 4, "rint", TokenType.PRINT),
            'r' => self.check_keyword(1, 5, "eturn", TokenType.RETURN),
            's' => self.check_keyword(1, 4, "uper", TokenType.SUPER),
            'v' => self.check_keyword(1, 2, "ar", TokenType.VAR),
            'w' => self.check_keyword(1, 4, "hile", TokenType.WHILE),
            'f' => if (self.current_diff() > 1) {
                switch (self.start[1]) {
                    'a' => self.check_keyword(2, 3, "lse", TokenType.FALSE),
                    'o' => self.check_keyword(2, 1, "r", TokenType.FOR),
                    'u' => self.check_keyword(2, 1, "n", TokenType.FUN),
                    else => unreachable,
                }
            },
            't' => if (self.current_diff() > 1) {
                switch (self.start[1]) {
                    'h' => self.check_keyword(2, 2, "is", TokenType.THIS),
                    'r' => self.check_keyword(2, 2, "ue", TokenType.TRUE),
                    else => unreachable,
                }
            },
            else => unreachable,
        };

        return self.make_token(tt);
    }

    fn current_diff(self: *Self) usize {
        return @ptrToInt(self.current.ptr) - @ptrToInt(self.start.ptr);
    }

    fn check_keyword(self: *Self, start: u8, length: u8, rest: []const u8, ty: TokenType) TokenType {
        var bLen = self.current_diff() == start + length;
        var bEq = std.mem.eql(u8, self.start[start..], rest);

        if (bLen and bEq) {
            return ty;
        }

        return TokenType.IDENTIFIER;
    }

    fn number(self: *Self) Token {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.adv();
        }

        var pk = self.peek();
        const next = self.peekN(1);

        if (pk == '.' and next.len > 0 and std.ascii.isDigit(next[0])) {
            _ = self.adv();
            while (std.ascii.isDigit(self.peek())) {
                _ = self.adv();
            }
        }

        return self.make_token(TokenType.NUMBER);
    }

    fn error_token(self: *Self, msg: [:0]const u8) Token {
        return Token{
            .type = TokenType.ERROR,
            .lex = msg,
            .line = self.line,
        };
    }

    fn make_token(self: *Self, tt: TokenType) Token {
        var end = @ptrToInt(self.current.ptr) - @ptrToInt(self.start.ptr);
        return Token{
            .type = tt,
            .lex = self.start[0..end],
            .line = self.line,
        };
    }

    fn adv(self: *Self) u8 {
        var c = self.current[0];
        self.current = self.current[1..];
        return c;
    }

    fn is_end(self: *Self) bool {
        return self.current[0] == 0;
    }
};

test "Scanner.basic" {
    const alloc = std.testing.allocator;
    {
        var scan = try Scanner.init(alloc, "(){};,.+-*/");
        var expected = [_]TokenType{
            .LPAREN, .RPAREN, .LBRACE, .RBRACE, .SEMICOLON, .COMMA, .DOT,
            .PLUS,   .MINUS,  .STAR,   .SLASH,  .EOF,
        };

        var token: Token = undefined;
        for (expected) |tt| {
            token = scan.scan_token();
            // std.debug.print("nExpected: {any}, actual: {any}\n", .{ tt, token.type });
            try std.testing.expect(token.type == tt);
        }
    }
    {
        var scan = try Scanner.init(alloc, "! != = == < <= > >=");
        var expected = [_]TokenType{
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
        var expected = [_]TokenType{
            .NUMBER, .NUMBER, .NUMBER, .NUMBER, .EOF,
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
    {
        var scan = try Scanner.init(alloc, "and class else false for fun if nil or print return super this true var while blabla");
        var expected = [_]TokenType{
            .AND, .CLASS, .ELSE, .FALSE, .FOR, .FUN, .IF, .NIL, .OR, .PRINT, .RETURN, .SUPER, .THIS, .TRUE, .VAR, .WHILE, .IDENTIFIER, .EOF,
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
