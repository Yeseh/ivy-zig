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
    alloc: *std.mem.Allocator,
    chunk: *Chunk,
    had_error: bool,
    panic: bool,
    rules: [TOKEN_COUNT]ParseRule,

    pub fn init(alloc: *std.mem.Allocator, scan: *Scanner) Compiler {
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
        return &self.rules[@enumToInt(tt)];
    }

    pub fn compile(self: *Self, chunk: *Chunk) !bool {
        self.chunk = chunk;

        self.advance();
        self.expression();
        self.eat(TokenType.EOF, "Expect end of expression.");

        return !self.had_error;
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
            if (self.cur.type != TokenType.EOF) {
                break;
            }
            self.err_at_cur(self.cur.lex);
        }
    }

    fn emit_byte(self: *Self, byte: OpCode) void {
        self.chunk.write(@enumToInt(byte), self.prev.line) catch {
            self.err("Out of memory.");
        };
    }

    fn emit_bytes(self: *Self, byte1: OpCode, byte2: OpCode) void {
        self.emit_byte(byte1);
        if (self.had_error) {
            return;
        }
        self.emit_byte(byte2);
    }

    fn emit_n_bytes(self: *Self, bytes: []OpCode) void {
        // maybe inline?
        for (bytes) |byte| {
            self.emit_byte(byte);
            if (self.had_error) {
                return;
            }
        }
    }

    fn emit_return(self: *Self) void {
        self.emit_byte(.RETURN);
    }

    fn emit_constant(self: *Self, value: Value) void {
        self.emit_bytes(.OP_CONSTANT, @intToEnum(OpCode, self.make_constant(value)));
    }

    fn make_constant(self: *Self, value: Value) u8 {
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
        var op_type = self.prev.type;
        var rule = self.get_rule(op_type);
        var pres = @enumToInt(rule.precedence) + 1;
        self.parse_precedence(@intToEnum(Precedence, pres));

        switch (op_type) {
            TokenType.PLUS => self.emit_byte(.OP_ADD),
            TokenType.MINUS => self.emit_byte(.OP_SUBTRACT),
            TokenType.STAR => self.emit_byte(.OP_MULTIPLY),
            TokenType.FSLASH => self.emit_byte(.OP_DIVIDE),
            else => unreachable,
        }
    }

    fn unary(self: *Self) void {
        var op_type = self.prev.type;
        self.parse_precedence(.UNARY);

        switch (op_type) {
            TokenType.MINUS => self.emit_byte(.OP_NEGATE),
            else => unreachable,
        }
    }

    fn parse_precedence(self: *Self, pres: Precedence) void {
        self.advance();
        var prefix = self.get_rule(self.prev.type).prefix orelse {
            self.err("Expect expression.");
            return;
        };

        prefix(self);

        while (@enumToInt(pres) <= @enumToInt(self.get_rule(self.cur.type).precedence)) {
            self.advance();
            var infix = self.get_rule(self.prev.type).infix orelse {
                unreachable;
            };
            infix(self);
        }
    }

    fn number(self: *Self) void {
        std.debug.print("number: {s}", .{self.cur.lex});
        var value = std.fmt.parseFloat(f64, self.cur.lex) catch {
            self.err_at_cur("Invalid number.");
            return;
        };

        self.emit_constant(value);
    }

    fn end(self: *Self) void {
        if (common.DEBUG_PRINT_CODE) {
            if (self.had_error) {
                return;
            }
            debug.disassemble_chunk(self.chunk, "code");
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
