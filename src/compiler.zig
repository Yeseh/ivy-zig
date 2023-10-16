const std = @import("std");
const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;

const Scanner = scanner.Scanner;
const TokenType = scanner.TokenType;
const Token = scanner.Token;
const ParseFn = fn (*Compiler, bool) void;

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
    prefix: ParseFn,
    infix: ParseFn,
};

pub const rules: ParseRule[TokenType] = undefined;

pub const Compiler = struct {
    const Self = @This();

    comptime rules: ParseRule[TokenType] = undefined,

    cur: Token,
    prev: Token,
    scan: *Scanner,
    alloc: *std.mem.Allocator,
    chunk: *Chunk,
    had_error: bool,
    panic: bool,

    pub fn init(alloc: *std.mem.Allocator, scan: *Scanner) Self {
        return Self{ .alloc = alloc, .scan = scan, .chunk = null, .had_error = false, .panic = false, .cur = null, .prev = null };
    }

    pub fn compile(self: *Self, chunk: *Chunk) !bool {
        comptime self.init_rules();

        self.chunk = chunk;

        self.advance();
        self.expression();
        self.eat(TokenType.EOF, "Expect end of expression.");

        return !self.had_error;
    }

    fn init_rules(self: *Self) void {
        self.rules[TokenType.LEFT_PAREN] = ParseRule{ .precedence = .CALL, .prefix = grouping, .infix = null };
        self.rules[TokenType.RIGHT_PAREN] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.RIGHT_BRACE] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.LEFT_BRACE] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.COMMA] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.DOT] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.MINUS] = ParseRule{ .precedence = .TERM, .prefix = unary, .infix = binary };
        self.rules[TokenType.PLUS] = ParseRule{ .precedence = .TERM, .prefix = null, .infix = binary };
        self.rules[TokenType.SEMICOLON] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.SLASH] = ParseRule{ .precedence = .FACTOR, .prefix = null, .infix = binary };
        self.rules[TokenType.STAR] = ParseRule{ .precedence = .FACTOR, .prefix = null, .infix = binary };
        self.rules[TokenType.BANG] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.BANG_EQUAL] = ParseRule{ .precedence = .EQUALITY, .prefix = null, .infix = binary };
        self.rules[TokenType.EQUAL] = ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
        self.rules[TokenType.EQUAL_EQUAL] = ParseRule{ .precedence = .EQUALITY, .prefix = null, .infix = binary };
        self.rules[TokenType.GREATER] = ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = binary };
        self.rules[TokenType.GREATER_EQUAL] = ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = binary };
    }

    fn current_chunk(self: *Self) *Chunk {
        return self.chunk;
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

    fn emit_byte(self: *Self, byte: u8) void {
        self.chunk.write(byte, self.prev.line);
    }

    fn emit_bytes(self: *Self, byte1: u8, byte2: u8) void {
        self.chunk.write(byte1, self.prev.line);
        self.chunk.write(byte2, self.prev.line);
    }

    fn emit_n_bytes(self: *Self, bytes: []u8) void {
        // maybe inline?
        for (bytes) |byte| {
            self.emit_byte(byte);
        }
    }

    fn emit_return(self: *Self) void {
        self.emit_byte(vm.OpCode.RETURN);
    }

    fn emit_constant(self: *Self) void {
        self.emit_bytes(vm.OpCode.OP_CONSTANT);
    }

    fn make_constant(self: *Self, value: Value) !u8 {
        var constant = try self.chunk.add_constant(value);
        // TODO: Support for more than 256 constants, see 17.4.1
        if (constant > u8.max) {
            self.err("Too many constants in one chunk.");
            return 0;
        }
        return constant;
    }

    fn grouping(self: *Self) void {
        self.expression();
        self.eat(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn get_rule(self: *Self, tt: TokenType) *ParseRule {
        _ = tt;
        _ = self;
    }

    fn binary(self: *Self) void {
        var op_type = self.prev.type;
        var rule = get_rule(op_type);
        self.parse_precedence(rule.precedence + 1);

        switch (op_type) {
            TokenType.PLUS => self.emit_byte(vm.OpCode.ADD),
            TokenType.MINUS => self.emit_byte(vm.OpCode.SUBTRACT),
            TokenType.STAR => self.emit_byte(vm.OpCode.MULTIPLY),
            TokenType.SLASH => self.emit_byte(vm.OpCode.DIVIDE),
            else => unreachable,
        }
    }

    fn unary(self: *Self) void {
        var op_type = self.prev.type;
        self.parse_precedence(.UNARY);

        switch (op_type) {
            TokenType.MINUS => self.emit_byte(vm.OpCode.NEGATE),
            else => unreachable,
        }
    }

    fn parse_precedence(pres: Precedence) void {
        _ = pres;
    }

    fn number(self: *Self) void {
        var value = std.fmt.parseFloat(f64, self.cur.lex);
        if (value == null) {
            self.err_at_cur("Invalid number.");
            return;
        }

        self.emit_constant(value);
    }

    fn end(self: *Self) void {
        self.emit_return();
    }

    fn expression(self: *Self) void {
        self.parse_precedence(.ASSIGNMENT);
    }

    fn err_at_cur(self: *Self, msg: []const u8) void {
        self.err_at(&self.current, msg);
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
            .ERROR => std.debug.print(""),
            else => std.debug.print(" at '{s}'", .{at.lex}),
        }

        std.debug.print(": {s}\n", .{msg});
    }
};
