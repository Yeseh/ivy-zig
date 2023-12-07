const std = @import("std");
const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const table = @import("table.zig");
const types = @import("types.zig");

const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const debug = @import("debug.zig");

const IvyType = types.IvyType;
const Object = types.Object;
const String = types.String;
const Scanner = scanner.Scanner;
const TokenType = scanner.TokenType;
const Token = scanner.Token;
const OpCode = common.OpCode;
const ParseFn = *const fn (*Compiler, bool) void;
const U8Max = 255;

pub const Precedence = enum {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
};

pub const ParseRule = struct {
    precedence: Precedence,
    prefix: ?ParseFn,
    infix: ?ParseFn,
};

pub const Compiler = struct {
    const TOKEN_COUNT = @typeInfo(TokenType).Enum.fields.len;
    const Self = @This();

    cur: Token,
    prev: Token,
    scan: *Scanner,
    alloc: std.mem.Allocator,
    chunk: *Chunk,
    had_error: bool,
    panic: bool,
    rules: [TOKEN_COUNT]ParseRule,

    pub fn init(alloc: std.mem.Allocator, scan: *Scanner) Compiler {
        return Compiler{
            .cur = undefined,
            .prev = undefined,
            .scan = scan,
            .alloc = alloc,
            .chunk = undefined,
            .had_error = false,
            .panic = false,
            .rules = [TOKEN_COUNT]ParseRule{
                // LPAREN
                ParseRule{ .precedence = .NONE, .prefix = &grouping, .infix = null },
                // RPAREN
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // LBRACE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // RBRACE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // COMMA
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // DOT
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // MINUS
                ParseRule{ .precedence = .TERM, .prefix = &unary, .infix = &binary },
                // PLUS
                ParseRule{ .precedence = .TERM, .prefix = null, .infix = &binary },
                // STAR
                ParseRule{ .precedence = .FACTOR, .prefix = null, .infix = &binary },
                // FSLASH
                ParseRule{ .precedence = .FACTOR, .prefix = null, .infix = &binary },
                // BSLASH
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // SEMICOLON
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },

                // BANG
                ParseRule{ .precedence = .NONE, .prefix = &unary, .infix = null },
                // BANG_EQUAL
                ParseRule{ .precedence = .EQUALITY, .prefix = null, .infix = &binary },
                // EQUAL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // EQUAL_EQUAL
                ParseRule{ .precedence = .EQUALITY, .prefix = null, .infix = &binary },
                // LESS
                ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = &binary },
                // LESS_EQUAL
                ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = &binary },
                // GREATER
                ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = &binary },
                // GREATER_EQUAL
                ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = &binary },

                // IDENTIFIER
                ParseRule{ .precedence = .NONE, .prefix = &variable, .infix = null },
                // STRING
                ParseRule{ .precedence = .NONE, .prefix = &string, .infix = null },
                // NUMBER
                ParseRule{ .precedence = .NONE, .prefix = &number, .infix = null },

                // AND
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // CLASS
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // ELSE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // FALSE
                ParseRule{ .precedence = .NONE, .prefix = &literal, .infix = null },
                // FOR
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // FUN
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // IF
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // NIL
                ParseRule{ .precedence = .NONE, .prefix = &literal, .infix = null },
                // OR
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // PRINT
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // RETURN
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // SUPER
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // THIS
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // TRUE
                ParseRule{ .precedence = .NONE, .prefix = &literal, .infix = null },
                // VAR
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // WHILE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // ERROR
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // EOF
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
            },
        };
    }

    fn get_rule(self: *Self, tt: TokenType) *ParseRule {
        var rule = &self.rules[@intFromEnum(tt)];
        return rule;
    }

    pub fn compile(self: *Self, chunk: *Chunk) !bool {
        self.chunk = chunk;

        self.advance();
        while (!self.match(.EOF)) {
            self.declaration();
        }
        self.end() catch {
            self.err("Out of memory.");
        };

        return !self.had_error;
    }

    fn current_chunk(self: *Self) *Chunk {
        return self.chunk;
    }

    fn check(self: *Self, tt: TokenType) bool {
        return self.cur.type == tt;
    }

    fn match(self: *Self, tt: TokenType) bool {
        if (!self.check(tt)) {
            return false;
        }

        self.advance();
        return true;
    }

    fn eat(self: *Self, tt: TokenType, msg: []const u8) void {
        if (self.cur.type == tt) {
            self.advance();
            return;
        }

        self.err_at_cur(msg);
    }

    fn advance(self: *Self) void {
        self.prev = self.cur;
        while (true) {
            self.cur = self.scan.scan_token();
            if (self.cur.type != TokenType.ERROR) {
                break;
            }

            self.err_at_cur(self.cur.lex);
        }
    }

    fn emit_op(self: *Self, op: OpCode) void {
        self.emit_byte(@intFromEnum(op));
    }

    fn emit_ops(self: *Self, opa: OpCode, opb: OpCode) void {
        self.emit_byte(@intFromEnum(opa));
        if (self.had_error) {
            return;
        }
        self.emit_byte(@intFromEnum(opb));
    }

    fn emit_byte(self: *Self, byte: u8) void {
        self.chunk.write(byte, self.prev.line) catch {
            self.err("Out of memory.");
        };
    }

    fn emit_bytes(self: *Self, byte1: u8, byte2: u8) void {
        self.emit_byte(byte1);
        if (self.had_error) {
            return;
        }
        self.emit_byte(byte2);
    }

    fn emit_return(self: *Self) void {
        self.emit_op(.RETURN);
    }

    fn emit_constant(self: *Self, value: IvyType) void {
        self.emit_bytes(@intFromEnum(OpCode.CONSTANT), self.makeConstant(value));
    }

    fn makeConstant(self: *Self, value: IvyType) u8 {
        var constant = self.chunk.add_constant(value) catch {
            self.err("Out of memory.");
            return 0;
        };
        // TODO: Support for more than 256 constants, see 17.4.1
        if (constant > U8Max) {
            self.err("Too many constants in one chunk.");
            return 0;
        }
        return @as(u8, @intCast(constant));
    }

    fn grouping(self: *Self, canAssign: bool) void {
        _ = canAssign;
        self.expression();
        self.eat(.RPAREN, "Expect ')' after expression.");
    }

    fn literal(self: *Self, canAssign: bool) void {
        _ = canAssign;
        switch (self.prev.type) {
            .FALSE => self.emit_op(.FALSE),
            .NIL => self.emit_op(.NIL),
            .TRUE => self.emit_op(.TRUE),
            else => unreachable,
        }
    }

    fn binary(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var op_type = self.prev.type;
        var rule = self.get_rule(op_type);
        var pres = @intFromEnum(rule.precedence) + 1;
        self.parsePrecedence(@as(Precedence, @enumFromInt(pres)));

        switch (op_type) {
            .PLUS => self.emit_op(.ADD),
            .MINUS => self.emit_op(.SUBTRACT),
            .STAR => self.emit_op(.MULTIPLY),
            .FSLASH => self.emit_op(.DIVIDE),
            .EQUAL_EQUAL => self.emit_op(.EQUAL),
            .BANG_EQUAL => self.emit_ops(.EQUAL, .NOT),
            .GREATER => self.emit_op(.GREATER),
            .LESS => self.emit_op(.LESS),
            .LESS_EQUAL => self.emit_ops(.GREATER, .NOT),
            .GREATER_EQUAL => self.emit_ops(.LESS, .NOT),

            else => unreachable,
        }
    }

    fn unary(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var op_type = self.prev.type;
        self.parsePrecedence(.UNARY);

        switch (op_type) {
            TokenType.MINUS => self.emit_op(.NEGATE),
            TokenType.BANG => self.emit_op(.NOT),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Self, pres: Precedence) void {
        self.advance();

        var prefix = self.get_rule(self.prev.type).prefix orelse {
            self.err("Expect expression.");
            return;
        };

        var canAssign = @intFromEnum(pres) <= @intFromEnum(Precedence.ASSIGNMENT);
        prefix(self, canAssign);

        while (@intFromEnum(pres) <= @intFromEnum(self.get_rule(self.cur.type).precedence)) {
            self.advance();
            var infix = self.get_rule(self.prev.type).infix orelse {
                unreachable;
            };
            infix(self, canAssign);
        }

        if (canAssign and self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Self, msg: []const u8) u8 {
        self.eat(.IDENTIFIER, msg);
        return self.identifierConstant(&self.prev);
    }

    fn defineVariable(self: *Self, global: u8) void {
        self.emit_ops(.DEFINE_GLOBAL, @enumFromInt(global));
    }

    fn identifierConstant(self: *Self, name: *Token) u8 {
        var str = String.copyInterned(self.alloc, name.*.lex, &vm.strings) catch {
            self.err_at_cur("Out of memory.");
            return 0;
        };
        var obj = IvyType.string(str);
        return self.makeConstant(obj);
    }

    fn number(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var value = std.fmt.parseFloat(f64, self.prev.lex) catch {
            self.err_at_cur("Invalid number.");
            return;
        };

        var num = IvyType.number(value);
        self.emit_constant(num);
    }

    fn string(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const chars = self.prev.lex[1 .. self.prev.lex.len - 1];

        const str = String.copyInterned(self.alloc, chars, &vm.strings) catch {
            self.err_at_cur("Out of memory.");
            return;
        };
        const obj = IvyType.string(str);
        self.emit_constant(obj);
    }

    fn variable(self: *Self, canAssign: bool) void {
        self.namedVariable(&self.prev, canAssign);
    }

    fn namedVariable(self: *Self, name: *Token, canAssign: bool) void {
        var arg = self.identifierConstant(name);

        if (canAssign and self.match(.EQUAL)) {
            self.expression();
            self.emit_ops(.SET_GLOBAL, @enumFromInt(arg));
            return;
        }

        self.emit_ops(.GET_GLOBAL, @enumFromInt(arg));
    }

    fn end(self: *Self) !void {
        if (common.DEBUG_PRINT_CODE) {
            if (self.had_error) {
                return;
            }
            // try debug.disassemble_chunk(self.chunk, "code");
        }
        self.emit_return();
    }

    fn declaration(self: *Self) void {
        if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panic) self.synchronize();
    }

    fn varDeclaration(self: *Self) void {
        var global = self.parseVariable("Expect variable name.");

        if (self.match(.EQUAL)) {
            self.expression();
        } else {
            self.emit_op(.NIL);
        }
        self.eat(.SEMICOLON, "Expect ';' after variable declaration.");

        self.defineVariable(global);
    }

    fn synchronize(self: *Self) void {
        self.panic = false;

        while (self.cur.type != .EOF) {
            if (self.prev.type == .SEMICOLON) {
                return;
            }

            switch (self.cur.type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }

            self.advance();
        }
    }

    fn statement(self: *Self) void {
        if (self.match(.PRINT)) {
            self.printStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.eat(.SEMICOLON, "Expect ';' after value.");
        self.emit_op(.POP);
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.eat(.SEMICOLON, "Expect ';' after value.");
        self.emit_op(.PRINT);
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.ASSIGNMENT);
    }

    fn err_at_cur(self: *Self, msg: []const u8) void {
        self.err_at(&self.cur, msg);
    }

    fn err(self: *Self, msg: []const u8) void {
        self.err_at(&self.prev, msg);
    }

    fn err_at(self: *Self, at: *Token, msg: []const u8) void {
        if (self.panic) {
            return;
        }
        self.panic = true;
        std.debug.print("[line: {}] ERR: ", .{at.line});

        switch (at.type) {
            .EOF => std.debug.print(" at end", .{}),
            .ERROR => std.debug.print("", .{}),
            else => std.debug.print(" at '{s}'", .{at.lex}),
        }

        std.debug.print(": {s}\n", .{msg});
    }
};

test "Compiler.strings" {
    const alloc = std.testing.allocator;
    const om = @import("garbage.zig");
    const tst = @import("test/testing.zig");
    {
        var source = "\"hello world\"";
        var cnk: Chunk = try Chunk.init(alloc);
        vm.strings = try table.Table.init(alloc, 8);
        defer cnk.deinit();
        defer om.free(alloc);
        defer vm.strings.deinit();

        var scan = try Scanner.init(alloc, source);
        var comp = Compiler.init(alloc, &scan);
        var compiled = try comp.compile(&cnk);

        var expected = [_]OpCode{ .CONSTANT, .RETURN };

        try std.testing.expect(compiled);
        try debug.disassemble_chunk(&cnk, "Test");
        try tst.assertCompiled(&expected, cnk.code);
    }
}

test "Compiler.basic" {
    const tst = @import("test/testing.zig");

    const alloc = std.testing.allocator;
    {
        var source = "1.2 + 3 - -4";
        var cnk: Chunk = try Chunk.init(alloc);
        defer cnk.deinit();

        var scan = try Scanner.init(alloc, source);
        var comp = Compiler.init(alloc, &scan);
        var compiled = try comp.compile(&cnk);

        var expected = [_]OpCode{ .CONSTANT, .CONSTANT, .ADD, .CONSTANT, .NEGATE, .SUBTRACT, .RETURN };

        try std.testing.expect(compiled);
        try std.testing.expect(comp.chunk.code.items.len == 10);

        try debug.disassemble_chunk(&cnk, "Test");
        try tst.assertCompiled(&expected, cnk.code);
    }
}

test "Compiler.literals" {
    const tst = @import("test/testing.zig");

    const alloc = std.testing.allocator;
    {
        var source = "true";
        var cnk: Chunk = try Chunk.init(alloc);
        defer cnk.deinit();

        var scan = try Scanner.init(alloc, source);
        var comp = Compiler.init(alloc, &scan);
        var compiled = try comp.compile(&cnk);

        var expected = [_]OpCode{ .TRUE, .RETURN };

        try std.testing.expect(compiled);
        try std.testing.expect(comp.chunk.code.items.len == 2);

        try debug.disassemble_chunk(&cnk, "Test");
        try tst.assertCompiled(&expected, cnk.code);
    }
}

test "Compiler.grouping" {
    const tst = @import("test/testing.zig");

    const alloc = std.testing.allocator;
    {
        var source = "(-1 + (2 * 4)) * 3 - -4";
        var cnk: Chunk = try Chunk.init(alloc);
        defer cnk.deinit();

        var scan = try Scanner.init(alloc, source);
        var comp = Compiler.init(alloc, &scan);
        var compiled = try comp.compile(&cnk);

        var expected = [_]OpCode{
            .CONSTANT,
            .NEGATE,
            .CONSTANT,
            .CONSTANT,
            .MULTIPLY,
            .ADD,
            .CONSTANT,
            .MULTIPLY,
            .CONSTANT,
            .NEGATE,
            .SUBTRACT,
            .RETURN,
        };

        try std.testing.expect(compiled);
        try std.testing.expect(comp.chunk.code.items.len == 17);

        try debug.disassemble_chunk(&cnk, "Test");
        try tst.assertCompiled(&expected, cnk.code);
    }
}

test "Compiler.comparison" {
    const tst = @import("test/testing.zig");

    const alloc = std.testing.allocator;
    {
        var source = "!(5-4 > 3*2 == !nil)";
        var cnk: Chunk = try Chunk.init(alloc);
        defer cnk.deinit();

        var scan = try Scanner.init(alloc, source);
        var comp = Compiler.init(alloc, &scan);
        var compiled = try comp.compile(&cnk);

        var expected = [_]OpCode{
            .CONSTANT,
            .CONSTANT,
            .SUBTRACT,
            .CONSTANT,
            .CONSTANT,
            .MULTIPLY,
            .GREATER,
            .NIL,
            .NOT,
            .EQUAL,
            .NOT,
            .RETURN,
        };

        try std.testing.expect(compiled);
        try std.testing.expect(comp.chunk.code.items.len == 16);

        try debug.disassemble_chunk(&cnk, "Test");
        try tst.assertCompiled(&expected, cnk.code);
    }
}
