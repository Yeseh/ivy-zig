const std = @import("std");
const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const debug = @import("debug.zig");

const Scanner = scanner.Scanner;
const TokenType = scanner.TokenType;
const Token = scanner.Token;
const OpCode = common.OpCode;
const ParseFn = *const fn (*Compiler) void;
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
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // BANG_EQUAL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // EQUAL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // EQUAL_EQUAL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // LESS
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // LESS_EQUAL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // GREATER
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // GREATER_EQUAL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },

                // IDENTIFIER
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // STRING
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // NUMBER
                ParseRule{ .precedence = .NONE, .prefix = &number, .infix = null },

                // AND
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // CLASS
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // ELSE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // FALSE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // FOR
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // FUN
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // IF
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // NIL
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
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
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
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
        var rule = &self.rules[@enumToInt(tt)];
        std.debug.print("get_rule: {any}\n", .{rule});
        return rule;
    }

    pub fn compile(self: *Self, chunk: *Chunk) !bool {
        self.chunk = chunk;

        self.advance();
        self.expression();
        self.eat(TokenType.EOF, "Expect end of expression.");
        self.end() catch {
            self.err("Out of memory.");
        };

        return !self.had_error;
    }

    fn current_chunk(self: *Self) *Chunk {
        return self.chunk;
    }

    fn eat(self: *Self, tt: TokenType, msg: []const u8) void {
        if (self.cur.type == tt) {
            std.debug.print("eat: {s}\n", .{self.cur.lex});
            self.advance();
            return;
        }

        self.err_at_cur(msg);
    }

    fn advance(self: *Self) void {
        self.prev = self.cur;
        while (true) {
            self.cur = self.scan.scan_token();
            std.debug.print("advance: cur {s}\n", .{self.cur.lex});

            if (self.cur.type != TokenType.ERROR) {
                break;
            }

            self.err_at_cur(self.cur.lex);
        }
    }

    fn emit_op(self: *Self, op: OpCode) void {
        self.emit_byte(@enumToInt(op));
    }

    fn emit_byte(self: *Self, byte: u8) void {
        std.debug.print("emit_byte: {any}\n", .{byte});
        self.chunk.write(byte, self.prev.line) catch {
            self.err("Out of memory.");
        };
    }

    fn emit_bytes(self: *Self, byte1: u8, byte2: u8) void {
        std.debug.print("emit_bytes\n", .{});
        self.emit_byte(byte1);
        if (self.had_error) {
            return;
        }
        self.emit_byte(byte2);
    }

    fn emit_return(self: *Self) void {
        std.debug.print("emit_return\n", .{});
        self.emit_op(.OP_RETURN);
    }

    fn emit_constant(self: *Self, value: Value) void {
        std.debug.print("emit_constant: {}\n", .{value});
        var constant = self.make_constant(value);
        std.debug.print("emit_constant: constant: {}\n", .{constant});
        var opcode = @intToEnum(OpCode, constant);
        std.debug.print("emit_constant: opcode: {}\n", .{opcode});

        self.emit_bytes(@enumToInt(OpCode.OP_CONSTANT), self.make_constant(value));
    }

    fn make_constant(self: *Self, value: Value) u8 {
        std.debug.print("make_constant: {any}\n", .{value});
        var constant = self.chunk.add_constant(value) catch {
            self.err("Out of memory.");
            return 0;
        };
        // TODO: Support for more than 256 constants, see 17.4.1
        if (constant > U8Max) {
            self.err("Too many constants in one chunk.");
            return 0;
        }
        return @intCast(u8, constant);
    }

    fn grouping(self: *Self) void {
        self.expression();
        self.eat(.RPAREN, "Expect ')' after expression.");
    }

    fn binary(self: *Self) void {
        std.debug.print("binary: {}\n", .{self.prev});
        var op_type = self.prev.type;
        var rule = self.get_rule(op_type);
        std.debug.print("binary: rule: {any}\n", .{rule});
        var pres = @enumToInt(rule.precedence) + 1;
        self.parse_precedence(@intToEnum(Precedence, pres));

        switch (op_type) {
            TokenType.PLUS => self.emit_op(.OP_ADD),
            TokenType.MINUS => self.emit_op(.OP_SUBTRACT),
            TokenType.STAR => self.emit_op(.OP_MULTIPLY),
            TokenType.FSLASH => self.emit_op(.OP_DIVIDE),
            else => unreachable,
        }
    }

    fn unary(self: *Self) void {
        var op_type = self.prev.type;
        self.parse_precedence(.UNARY);

        switch (op_type) {
            TokenType.MINUS => self.emit_op(.OP_NEGATE),
            else => unreachable,
        }
    }

    fn parse_precedence(self: *Self, pres: Precedence) void {
        std.debug.print("parse_precedence: {any}\n", .{pres});
        self.advance();
        std.debug.print("parse_precedence: {s}\n", .{self.prev.lex});

        var prefix = self.get_rule(self.prev.type).prefix orelse {
            self.err("Expect expression.");
            return;
        };

        std.debug.print("prefix rule: {any}\n", .{prefix});
        prefix(self);

        while (@enumToInt(pres) <= @enumToInt(self.get_rule(self.cur.type).precedence)) {
            std.debug.print("precedence: {} {}\n", .{ @enumToInt(pres), @enumToInt(self.get_rule(self.cur.type).precedence) });
            self.advance();
            std.debug.print("Infix for: {s}\n", .{self.prev.lex});
            var infix = self.get_rule(self.prev.type).infix orelse {
                unreachable;
            };
            std.debug.print("infix rule: {any}\n", .{infix});
            infix(self);
        }
    }

    fn number(self: *Self) void {
        std.debug.print("number: {s}\n", .{self.prev.lex});
        var value = std.fmt.parseFloat(f64, self.prev.lex) catch {
            self.err_at_cur("Invalid number.");
            return;
        };

        self.emit_constant(value);
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

    fn expression(self: *Self) void {
        self.parse_precedence(.ASSIGNMENT);
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

test "Compiler.basic" {
    const tst = @import("test/compiler_test.zig");

    const alloc = std.testing.allocator;
    {
        var source = "1.2 + 3 - -4";
        var cnk: Chunk = try Chunk.init(alloc);
        defer cnk.deinit();

        var scan = try Scanner.init(alloc, source);
        var comp = Compiler.init(alloc, &scan);
        var compiled = try comp.compile(&cnk);

        var expected = [_]OpCode{ .OP_CONSTANT, .OP_CONSTANT, .OP_ADD, .OP_CONSTANT, .OP_NEGATE, .OP_SUBTRACT, .OP_RETURN };

        try std.testing.expect(compiled);
        // CONST CONST ADD CONST RETURN
        try std.testing.expect(comp.chunk.code.items.len == 10);

        try debug.disassemble_chunk(&cnk, "Test");
        try tst.assert_compiled(&expected, cnk.code);
    }
}
