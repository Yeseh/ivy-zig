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
const Function = types.Function;
const Scanner = scanner.Scanner;
const TokenType = scanner.TokenType;
const Token = scanner.Token;
const OpCode = common.OpCode;
const ParseFn = *const fn (*Parser, bool) void;
const U8Max = 255;
const U16Max = 65535;
const LOCAL_COUNT = U8Max + 1;

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

// TODO: Implement constants
pub const Local = struct {
    name: Token,
    depth: i32,
};

pub const FunctionType = enum { function, script };

// TODO: Uses simulated stack, can we use the real stack?
// TODO: Expand to support more than 256 locals
pub const Compiler = struct {
    const Self = @This();

    enclosing: ?*Self,
    function: *Function,
    type: FunctionType,
    locals: [LOCAL_COUNT]Local,
    localCount: i32,
    scopeDepth: i32,

    pub fn init(alloc: std.mem.Allocator, ty: FunctionType, name: ?*Token, current: ?*Compiler) !Compiler {
        std.debug.print("creating compiler\n", .{});
        var compiler = Compiler{
            .locals = undefined,
            .localCount = 0,
            .scopeDepth = 0,
            .enclosing = current,
            .type = ty,
            .function = try Function.create(alloc),
        };

        if (ty != .script and name != null) {
            var fnName = try String.copyInterned(alloc, name.?.lex, &vm.strings);
            compiler.function.name = fnName;
        }

        var local = &compiler.locals[@intCast(compiler.localCount)];
        local.depth = 0;
        local.name.lex = "";
        compiler.localCount += 1;

        return compiler;
    }
};

pub const ParseRule = struct {
    precedence: Precedence,
    prefix: ?ParseFn,
    infix: ?ParseFn,
};

// TODO: Use error set/tag for self.err();
pub const Parser = struct {
    const TOKEN_COUNT = @typeInfo(TokenType).Enum.fields.len;
    const Self = @This();

    cur: Token,
    prev: Token,
    scan: *Scanner,
    alloc: std.mem.Allocator,
    had_error: bool,
    panic: bool,
    currentCompiler: ?Compiler,
    rules: [TOKEN_COUNT]ParseRule,

    pub fn init(alloc: std.mem.Allocator) !Parser {
        var parser = Parser{
            .cur = undefined,
            .prev = undefined,
            .scan = undefined,
            .alloc = alloc,
            .had_error = false,
            .currentCompiler = try Compiler.init(alloc, .script, null, null),
            .panic = false,
            .rules = [TOKEN_COUNT]ParseRule{
                // LPAREN
                ParseRule{ .precedence = .CALL, .prefix = &grouping, .infix = &call },
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
                ParseRule{ .precedence = .AND, .prefix = null, .infix = &_and },
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
                ParseRule{ .precedence = .OR, .prefix = null, .infix = &_or },
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
                // CONST
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // WHILE
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // ERROR
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
                // EOF
                ParseRule{ .precedence = .NONE, .prefix = null, .infix = null },
            },
        };

        return parser;
    }

    fn get_rule(self: *Self, tt: TokenType) *ParseRule {
        var rule = &self.rules[@intFromEnum(tt)];
        return rule;
    }

    pub fn compile(self: *Self, source: [:0]u8) !?*Function {
        var scn = try Scanner.init(self.alloc, source);
        self.scan = &scn;

        self.advance();
        while (!self.match(.EOF)) {
            self.declaration();
        }

        var fun = try self.end();
        var retVal = if (self.had_error) null else fun;
        return retVal;
    }

    fn declaration(self: *Self) void {
        if (self.match(.VAR)) {
            self.varDeclaration();
        } else if (self.match(.CONST)) {
            self.constDeclaration();
        } else if (self.match(.FN)) {
            self.fnDeclaration();
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

    fn constDeclaration(self: *Self) void {
        var global = self.parseVariable("Expect variable name.");

        self.eat(.EQUAL, "Expect '=' after variable name.");
        self.expression();
        self.eat(.SEMICOLON, "Expect ';' after variable declaration.");

        self.defineVariable(global);
    }

    fn statement(self: *Self) void {
        if (self.match(.PRINT)) {
            self.printStatement();
        } else if (self.match(.IF)) {
            self.ifStatement();
        } else if (self.match(.WHILE)) {
            self.whileStatement();
        } else if (self.match(.FOR)) {
            self.forStatement();
        } else if (self.match(.LBRACE)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.eat(.SEMICOLON, "Expect ';' after value.");
        self.emit_op(.PRINT);
    }

    // TODO: Implement continue, break
    // TODO: Implement switch
    fn whileStatement(self: *Self) void {
        var loopStart = self.currentChunk().code.items.len;
        self.eat(.LPAREN, "Expect '(' after 'while'.");
        self.expression();
        self.eat(.RPAREN, "Expect ')' after condition.");

        var exitJump = self.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        self.emit_op(.POP);
        self.statement();
        self.emitLoop(loopStart);

        self.patchJump(exitJump);
        self.emit_op(.POP);
    }

    // TODO: I don't want this type of for loop. Replace with iterator/range based for loop
    fn forStatement(self: *Self) void {
        self.beginScope();
        self.eat(.LPAREN, "Expect '(' after 'for'.");
        if (self.match(.SEMICOLON)) {
            // No initializer.
        } else if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.items.len;
        var exitLoop: i32 = -1;
        if (!self.match(.SEMICOLON)) {
            self.expression();
            self.eat(.SEMICOLON, "Expect ';' after loop condition.");

            exitLoop = @intCast(self.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE)));
            self.emit_op(.POP);
        }

        if (!self.match(.RPAREN)) {
            var bodyJump = self.emitJump(@intFromEnum(OpCode.JUMP));
            var incrementStart = self.currentChunk().code.items.len;
            self.expression();
            self.emit_op(.POP);
            self.eat(.RPAREN, "Expect ')' after for clauses.");

            self.emitLoop(loopStart);
            loopStart = incrementStart;
            self.patchJump(@intCast(bodyJump));
        }

        self.statement();
        self.emitLoop(loopStart);

        if (exitLoop != -1) {
            self.patchJump(@intCast(exitLoop));
            self.emit_op(.POP);
        }

        self.endScope();
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.eat(.SEMICOLON, "Expect ';' after value.");
        self.emit_op(.POP);
    }

    fn ifStatement(self: *Self) void {
        self.eat(.LPAREN, "Expect '(' after 'if'.");
        self.expression();
        self.eat(.RPAREN, "Expect ')' after condition.");

        var thenJump = self.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        self.emit_op(.POP);
        self.statement();
        var elseJump = self.emitJump(@intFromEnum(OpCode.JUMP));

        self.patchJump(thenJump);
        self.emit_op(.POP);
        if (self.match(.ELSE)) {
            self.statement();
        }
        self.patchJump(elseJump);
    }

    fn block(self: *Self) void {
        while (!self.check(.RBRACE) and !self.check(.EOF)) {
            self.declaration();
        }

        self.eat(.RBRACE, "Expect '}' after block.");
    }

    fn function(self: *Self, ty: FunctionType) void {
        var fnComp = Compiler.init(self.alloc, ty, &self.prev, &self.currentCompiler.?) catch {
            self.err("Failed to initialize compiler.");
            return;
        };
        self.currentCompiler.? = fnComp;
        self.beginScope();

        self.eat(.LPAREN, "Expect '(' after function name.");
        if (!self.check(.RPAREN)) {
            while (true) {
                self.currentCompiler.?.function.arity += 1;
                if (self.currentCompiler.?.function.arity > U8Max) {
                    self.err("Cannot have more than 255 parameters.");
                }

                var paramConstant = self.parseVariable("Expect parameter name.");
                self.defineVariable(paramConstant);

                if (!self.match(.COMMA)) {
                    break;
                }
            }
        }
        self.eat(.RPAREN, "Expect ')' after parameters.");
        self.eat(.LBRACE, "Expect '{' before function body.");
        self.block();

        var fun = self.end() catch {
            self.err("Failed to end function compilation.");
            return;
        };
        var funType = IvyType.function(fun);
        var constant = self.makeConstant(funType);
        self.emit_ops(.CONSTANT, @enumFromInt(constant));
    }

    fn fnDeclaration(self: *Self) void {
        var global = self.parseVariable("Expect function identifier.");
        self.markInitialized();
        self.function(.function);
        self.defineVariable(global);
    }

    pub fn beginScope(self: *Self) void {
        self.currentCompiler.?.scopeDepth += 1;
    }

    pub fn endScope(self: *Self) void {
        self.currentCompiler.?.scopeDepth -= 1;
        var toPop: u32 = 0;

        while (self.currentCompiler.?.localCount > 0 and self.currentCompiler.?.locals[@intCast(self.currentCompiler.?.localCount - 1)].depth > self.currentCompiler.?.scopeDepth) {
            toPop += 1;
            self.currentCompiler.?.localCount -= 1;
        }

        while (toPop > 0) {
            if (toPop > U8Max) {
                self.emit_bytes(@intFromEnum(OpCode.POP_N), U8Max);
                toPop -= U8Max;
            } else {
                self.emit_bytes(@intFromEnum(OpCode.POP_N), @intCast(toPop));
                toPop = 0;
            }
        }
    }

    pub fn addLocal(self: *Self, name: Token) void {
        if (self.currentCompiler.?.localCount == LOCAL_COUNT) {
            self.err("Too many local variables in function.");
            return;
        }
        var local = &self.currentCompiler.?.locals[@intCast(self.currentCompiler.?.localCount)];
        local.name = name;
        local.depth = -1;

        self.currentCompiler.?.localCount += 1;
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.ASSIGNMENT);
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

        if (!canAssign and self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    fn variable(self: *Self, canAssign: bool) void {
        self.namedVariable(&self.prev, canAssign);
    }

    fn namedVariable(self: *Self, name: *Token, canAssign: bool) void {
        var setOp = OpCode.SET_LOCAL;
        var getOp = OpCode.GET_LOCAL;

        var arg = self.resolveLocal(name);
        if (arg == -1) {
            arg = self.identifierConstant(name);
            setOp = .SET_GLOBAL;
            getOp = .GET_GLOBAL;
        }

        if (canAssign and self.match(.EQUAL)) {
            self.expression();
            self.emit_ops(setOp, @enumFromInt(arg));
            return;
        }

        self.emit_ops(getOp, @enumFromInt(arg));
    }

    fn resolveLocal(self: *Self, name: *Token) i32 {
        var i = self.currentCompiler.?.localCount - 1;
        while (i >= 0) : (i -= 1) {
            var local = &self.currentCompiler.?.locals[@intCast(i)];
            if (identifiersEql(name, &local.name)) {
                if (local.depth == -1) {
                    self.err("Cannot read local variable in its own initializer.");
                }
                var index: i32 = @intCast(i);
                return index;
            }
        }

        return -1;
    }

    fn parseVariable(self: *Self, msg: []const u8) u8 {
        self.eat(.IDENTIFIER, msg);
        self.declareVariable();
        if (self.currentCompiler.?.scopeDepth > 0) {
            return 0;
        }

        return self.identifierConstant(&self.prev);
    }

    fn defineVariable(self: *Self, global: u8) void {
        if (self.currentCompiler.?.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        self.emit_bytes(@intFromEnum(OpCode.DEFINE_GLOBAL), global);
    }

    fn argumentList(self: *Self) u8 {
        var argCount: u8 = 0;
        if (!self.check(.RPAREN)) {
            self.expression();
            argCount += 1;
            while (!self.match(.COMMA)) {
                self.expression();
                if (argCount == U8Max) {
                    self.err("Cannot have more than 255 arguments.");
                }
                argCount += 1;
            }
        }
        self.eat(.RPAREN, "Expect ')' after arguments.");
        return argCount;
    }

    fn markInitialized(self: *Self) void {
        if (self.currentCompiler.?.scopeDepth == 0) {
            return;
        }
        var idx = self.currentCompiler.?.localCount - 1;
        var sd = self.currentCompiler.?.scopeDepth;
        var local = &self.currentCompiler.?.locals[@intCast(idx)];
        local.depth = sd;
    }

    fn declareVariable(self: *Self) void {
        if (self.currentCompiler.?.scopeDepth == 0) {
            return;
        }

        var name = &self.prev;
        var idx = self.currentCompiler.?.localCount - 1;
        while (idx >= 0) : (idx -= 1) {
            var local = &self.currentCompiler.?.locals[@intCast(idx)];
            if (local.depth != -1 and local.depth < self.currentCompiler.?.scopeDepth) {
                break;
            }

            if (identifiersEql(name, &local.name)) {
                self.err("Already variable with this name in this scope.");
            }
        }

        self.addLocal(name.*);
    }

    fn identifierConstant(self: *Self, name: *Token) u8 {
        var str = String.copyInterned(self.alloc, name.*.lex, &vm.strings) catch {
            self.err_at_cur("Out of memory.");
            return 0;
        };
        var obj = IvyType.string(str);
        return self.makeConstant(obj);
    }

    fn identifiersEql(a: *Token, b: *Token) bool {
        if (a.lex.len != b.lex.len) {
            return false;
        }
        return std.mem.eql(u8, a.lex, b.lex);
    }

    fn _and(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var endJump = self.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        self.emit_op(.POP);
        self.parsePrecedence(.AND);
        self.patchJump(endJump);
    }

    fn _or(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var elseJump = self.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        var endJump = self.emitJump(@intFromEnum(OpCode.JUMP));

        self.patchJump(elseJump);
        self.emit_op(.POP);

        self.parsePrecedence(.OR);
        self.patchJump(endJump);
    }

    fn number(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var value = std.fmt.parseFloat(f64, self.prev.lex) catch {
            return;
        };

        var num = IvyType.number(value);
        self.emit_constant(num);
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

    fn call(self: *Self, canAssign: bool) void {
        _ = canAssign;
        var argCount = self.argumentList();
        self.emit_ops(.CALL, @enumFromInt(argCount));
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

    fn makeConstant(self: *Self, value: IvyType) u8 {
        var constant = self.currentChunk().add_constant(value) catch {
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

    fn currentChunk(self: *Self) *Chunk {
        return &self.currentCompiler.?.function.chunk;
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
        self.currentChunk().write(byte, self.prev.line) catch {
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
        self.emit_op(.NIL);
        self.emit_op(.RETURN);
    }

    fn emit_constant(self: *Self, value: IvyType) void {
        self.emit_bytes(@intFromEnum(OpCode.CONSTANT), self.makeConstant(value));
    }

    fn emitJump(self: *Self, op: u8) u32 {
        self.emit_byte(op);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        return @intCast(self.currentChunk().code.items.len - 2);
    }

    fn emitLoop(self: *Self, start: usize) void {
        self.emit_op(.LOOP);

        var offset = self.currentChunk().code.items.len - start + 2;
        if (offset > U16Max) {
            self.err("Loop body too large.");
        }

        self.emit_byte(@intCast((offset >> 8) & 0xff));
        self.emit_byte(@intCast(offset & 0xff));
    }

    fn patchJump(self: *Self, offset: u32) void {
        var jump = self.currentChunk().code.items.len - offset - 2;
        if (jump > U16Max) {
            self.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn end(self: *Self) !*Function {
        self.emit_return();
        var fun = self.currentCompiler.?.function;
        if (self.had_error) {
            if (fun.name == null) {
                try debug.disassemble_chunk(self.currentChunk(), "<script>");
            } else {
                try debug.disassemble_chunk(self.currentChunk(), fun.name.?.asSlice());
            }
        }

        if (self.currentCompiler == null or self.currentCompiler.?.enclosing == null) {
            self.currentCompiler = null;
        } else {
            self.currentCompiler = self.currentCompiler.?.enclosing.?.*;
        }
        return fun;
    }

    fn synchronize(self: *Self) void {
        self.panic = false;

        while (self.cur.type != .EOF) {
            if (self.prev.type == .SEMICOLON) {
                return;
            }

            switch (self.cur.type) {
                .CLASS, .FN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }

            self.advance();
        }
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
        self.had_error = true;
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
        var comp = Parser.init(alloc, &scan);
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
        var comp = Parser.init(alloc, &scan);
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
        var comp = Parser.init(alloc, &scan);
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
        var comp = Parser.init(alloc, &scan);
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
        var comp = Parser.init(alloc, &scan);
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
