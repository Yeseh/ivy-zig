const std = @import("std");
const scanner = @import("scanner.zig");
const vm = @import("vm.zig");
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;

const Scanner = scanner.Scanner;
const TokenType = scanner.TokenType;
const Token = scanner.Token;
const ParseFn = fn (*Compiler) void;

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

pub const Compiler = struct {
    cur: Token,
    prev: Token,
    scan: *Scanner,
    alloc: *std.mem.Allocator,
    chunk: *Chunk,
    had_error: bool,
    panic: bool,
};

// TODO: Is there a way to derive this from the enum?
pub const TOKEN_COUNT = 40;
const rules: [TOKEN_COUNT]ParseRule = {
    ParseRule{ .precedence = .CALL, .prefix = grouping, .infix = null };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .TERM, .prefix = unary, .infix = binary };
    ParseRule{ .precedence = .TERM, .prefix = null, .infix = binary };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .FACTOR, .prefix = null, .infix = binary };
    ParseRule{ .precedence = .FACTOR, .prefix = null, .infix = binary };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .EQUALITY, .prefix = null, .infix = binary };
    ParseRule{ .precedence = .NONE, .prefix = null, .infix = null };
    ParseRule{ .precedence = .EQUALITY, .prefix = null, .infix = binary };
    ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = binary };
    ParseRule{ .precedence = .COMPARISON, .prefix = null, .infix = binary };
};

var compiler = Compiler{
    .cur = undefined,
    .prev = undefined,
    .scan = undefined,
    .alloc = undefined,
    .chunk = undefined,
    .had_error = false,
    .panic = false,
};

pub fn init_compiler(alloc: *std.mem.Allocator, scan: *Scanner) *Compiler {
    compiler.alloc = alloc;
    compiler.scan = scan;
    return &compiler;
}

pub fn compile(chunk: *Chunk) !bool {
    compiler.chunk = chunk;

    advance();
    expression();
    eat(TokenType.EOF, "Expect end of expression.");

    return !compiler.had_error;
}

fn current_chunk() *Chunk {
    return compiler.chunk;
}

fn eat(tt: TokenType, msg: []const u8) void {
    if (compiler.cur.type == tt) {
        advance();
        return;
    }

    err_at_cur(msg);
}

fn advance() void {
    compiler.prev = compiler.cur;
    while (true) {
        compiler.cur = compiler.scan.scan_token();
        if (compiler.cur.type != TokenType.ERROR) {
            break;
        }
        err_at_cur(compiler.cur.lex);
    }
}

fn emit_byte(byte: u8) void {
    compiler.chunk.write(byte, compiler.prev.line);
}

fn emit_bytes(byte1: u8, byte2: u8) void {
    compiler.chunk.write(byte1, compiler.prev.line);
    compiler.chunk.write(byte2, compiler.prev.line);
}

fn emit_n_bytes(bytes: []u8) void {
    // maybe inline?
    for (bytes) |byte| {
        compiler.emit_byte(byte);
    }
}

fn emit_return() void {
    emit_byte(vm.OpCode.RETURN);
}

fn emit_constant() void {
    emit_bytes(vm.OpCode.OP_CONSTANT);
}

fn make_constant(value: Value) !u8 {
    var constant = try compiler.chunk.add_constant(value);
    // TODO: Support for more than 256 constants, see 17.4.1
    if (constant > u8.max) {
        compiler.err("Too many constants in one chunk.");
        return 0;
    }
    return constant;
}

fn grouping() void {
    expression();
    eat(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn get_rule(tt: TokenType) *ParseRule {
    _ = tt;
}

fn binary() void {
    var op_type = compiler.prev.type;
    var rule = get_rule(op_type);
    parse_precedence(rule.precedence + 1);

    switch (op_type) {
        TokenType.PLUS => emit_byte(vm.OpCode.ADD),
        TokenType.MINUS => emit_byte(vm.OpCode.SUBTRACT),
        TokenType.STAR => emit_byte(vm.OpCode.MULTIPLY),
        TokenType.SLASH => emit_byte(vm.OpCode.DIVIDE),
        else => unreachable,
    }
}

fn unary() void {
    var op_type = compiler.prev.type;
    parse_precedence(.UNARY);

    switch (op_type) {
        TokenType.MINUS => emit_byte(vm.OpCode.NEGATE),
        else => unreachable,
    }
}

fn parse_precedence(pres: Precedence) void {
    _ = pres;
}

fn number() void {
    var value = std.fmt.parseFloat(f64, compiler.cur.lex);
    if (value == null) {
        err_at_cur("Invalid number.");
        return;
    }

    emit_constant(value);
}

fn end() void {
    emit_return();
}

fn expression() void {
    parse_precedence(.ASSIGNMENT);
}

fn err_at_cur(msg: []const u8) void {
    err_at(&compiler.cur, msg);
}

fn err(msg: []const u8) void {
    err_at(&compiler.prev, msg);
}

fn err_at(at: *Token, msg: []const u8) void {
    if (compiler.panic) {
        return;
    }
    compiler.panic = true;
    std.debug.print("[line: {}] ERR: ", .{at.line});

    switch (at.type) {
        .EOF => std.debug.print(" at end", .{}),
        .ERROR => std.debug.print("", .{}),
        else => std.debug.print(" at '{s}'", .{at.lex}),
    }

    std.debug.print(": {s}\n", .{msg});
}
