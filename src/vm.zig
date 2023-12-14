const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const types = @import("types.zig");
const chunk = @import("chunk.zig");
const compiler = @import("compiler.zig");
const Parser = compiler.Parser;
const Scanner = @import("scanner.zig").Scanner;
const table = @import("table.zig");
const garbage = @import("garbage.zig");
const Stack = @import("stack.zig").Stack;

const Chunk = chunk.Chunk;
const ChunkError = chunk.ChunkError;
const IvyType = common.IvyType;
const OpCode = common.OpCode;
const Object = types.Object;
const Compiler = compiler.Compiler;
const FunctionType = compiler.FunctionType;
const String = types.String;

pub const FRAMES_MAX = 64;
pub const STACK_MAX = FRAMES_MAX * 256;
pub const InterpreterError = error{
    CompiletimeError,
    RuntimeError,
};

var stack: [STACK_MAX]IvyType = undefined;
var frames: [FRAMES_MAX]CallFrame = undefined;

pub const CallFrame = struct {
    const Self = @This();

    function: *types.Function,
    ip: [*]u8,
    slots: []IvyType,
};

// TODO: Look for more efficient ways to store globals
pub var globals: table.Table = undefined;
pub var strings: table.Table = undefined;

pub const VirtualMachine = struct {
    const Self = @This();

    ip: [*]u8,
    frameCount: i32,
    alloc: std.mem.Allocator,
    frames: Stack(CallFrame),
    stack: Stack(IvyType),

    pub fn init(alloc: std.mem.Allocator) !Self {
        globals = try table.init(alloc, 8);
        strings = try table.init(alloc, 8);
        var vm = VirtualMachine{ .ip = undefined, .frameCount = 0, .alloc = alloc, .frames = Stack(CallFrame).init(&frames), .stack = Stack(IvyType).init(&stack) };
        vm.resetStack();
        return vm;
    }

    pub fn deinit(self: *Self) void {
        garbage.free(self.alloc);
        strings.deinit();
        globals.deinit();
    }

    /// Interpret a source buffer
    pub fn interpret(self: *Self, source: [:0]u8) !void {
        if (common.DEBUG_PRINT_SOURCE) {
            std.debug.print("\n=== Source Code ===\n", .{});
            std.debug.print("{s}\n\n", .{source});
        }

        var comp: Compiler = undefined;
        try compiler.initCompiler(&comp, self.alloc, FunctionType.script, null);

        var parser = try Parser.init(self.alloc);
        var compiled = try parser.compile(source);

        if (compiled == null) {
            std.debug.print("\nCompilation failed\n", .{});
            return InterpreterError.CompiletimeError;
        }

        self.pushStack(IvyType.function(compiled.?));
        _ = try self.call(compiled.?, 0);

        try self.run();
    }

    pub fn run(self: *Self) !void {
        var frame = self.frames.cur();
        var stackCount = self.stack.count;

        while (true) {
            if (common.DEBUG_PRINT_CODE) {
                debug.dump_stack(self);
                const offset = @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.get_op_ptr());
                _ = debug.disassemble_instruction(&frame.function.chunk, offset) catch |err| {
                    std.debug.print("{any}\n", .{err});
                    return InterpreterError.RuntimeError;
                };
            }

            const byte = self.read_byte(frame);
            const instruction: OpCode = @enumFromInt(byte);

            switch (instruction) {
                .CONSTANT => self.pushStack(self.read_constant(frame)),
                .NEGATE => b: {
                    const value = self.peekStack(0);
                    switch (value) {
                        .num => {
                            const num = self.popStack().num;
                            self.pushStack(IvyType.number(-num));
                            break :b;
                        },
                        else => {
                            try self.rt_error("Operand must be a number.", .{});
                        },
                    }
                    self.pushStack(value);
                },
                .TRUE => self.pushStack(IvyType.boolean(true)),
                .FALSE => self.pushStack(IvyType.boolean(false)),
                .NIL => self.pushStack(IvyType.nil()),
                .NOT => b: {
                    const value = !self.popStack().as_bool();
                    self.pushStack(IvyType.boolean(value));
                    break :b;
                },
                .ADD => blk: {
                    var pa = self.peekStack(0);
                    var pb = self.peekStack(1);

                    if (pa.is_string() and pb.is_string()) {
                        var b = self.popStack().object_as(String);
                        var a = self.popStack().object_as(String);
                        var str = try self.concatenate(a, b);
                        self.pushStack(str);
                    } else if (pa.is_num() and pb.is_num()) {
                        var nb = self.popStack().num;
                        var na = self.popStack().num;
                        self.pushStack(IvyType.number(na + nb));
                    } else {
                        try self.rt_error("Operands must be two numbers or two strings.", .{});
                    }
                    break :blk;
                },
                .SUBTRACT => try self.binary_operation(instruction),
                .DIVIDE => try self.binary_operation(instruction),
                .MULTIPLY => try self.binary_operation(instruction),
                .EQUAL => try self.binary_operation(instruction),
                .LESS => try self.binary_operation(instruction),
                .GREATER => try self.binary_operation(instruction),
                .GET_LOCAL => {
                    var slot = self.read_byte(frame);
                    self.pushStack(frame.slots[slot]);
                },
                .SET_LOCAL => {
                    var slot = self.read_byte(frame);
                    frame.slots[slot] = self.peekStack(0);
                },
                .DEFINE_GLOBAL => {
                    var name = self.read_constant(frame).object_as(String);
                    var isSet = try globals.set(name, self.peekStack(0));
                    if (!isSet) {
                        try self.rt_error("Redefining existing variable '{s}'.", .{name.asSlice()});
                    }
                    _ = self.popStack();
                },
                .GET_GLOBAL => {
                    var name = self.read_constant(frame).object_as(String);
                    var global = globals.get(name);
                    if (global == null) {
                        try self.rt_error("Undefined variable '{s}'.", .{name.asSlice()});
                    }
                    self.pushStack(global.?);
                },
                .SET_GLOBAL => {
                    var name = self.read_constant(frame).object_as(String);
                    var set = try globals.set(name, self.peekStack(0));
                    if (set) {
                        _ = globals.delete(name);
                        // NOTE: No implicit variable declaration!
                        try self.rt_error("Undefined variable '{s}'.", .{name.asSlice()});
                    }
                },
                .JUMP => {
                    var offset = self.read_short(frame);
                    frame.ip += offset;
                },
                .JUMP_IF_FALSE => {
                    var offset = self.read_short(frame);
                    if (self.is_falsey(self.peekStack(0))) {
                        frame.ip += offset;
                    }
                },
                // TODO: implement loop as JUMP
                .LOOP => {
                    var offset = self.read_short(frame);
                    frame.ip -= offset;
                },
                .PRINT => {
                    var value = self.popStack();
                    value.print();
                    std.debug.print("\n", .{});
                },
                .POP => {
                    _ = self.popStack();
                },
                .CALL => {
                    var argCount = self.read_byte(frame);
                    var peek = self.peekStack(argCount);
                    var called = try self.callValue(peek, argCount);
                    if (!called) {
                        return InterpreterError.RuntimeError;
                    }
                    frame = self.frames.cur();
                },
                .POP_N => {
                    var n = self.read_byte(frame);
                    var i: usize = 0;
                    while (i < n) : (i += 1) {
                        _ = self.popStack();
                    }
                },
                .RETURN => {
                    var result = self.popStack();
                    self.frames.count -= 1;

                    if (self.frames.count == 0) {
                        _ = self.popStack();
                        return;
                    }
                    // Discard CallFrame
                    var dif = self.stack.count - stackCount;
                    self.stack.count -= dif;

                    self.pushStack(result);
                    frame = self.frames.cur();
                },
            }
        }
    }

    pub fn concatenate(self: *Self, a: *String, b: *String) !IvyType {
        var capacity = a._len + b._len + 1;
        var buf = try self.alloc.alloc(u8, capacity);
        errdefer self.alloc.free(buf);

        // TODO: move all this to fn under String for neatness
        @memcpy(buf[0..a._len], a._buf[0..a._len]);
        @memcpy(buf[a._len .. a._len + b._len + 1], b._buf[0 .. b._len + 1]);
        var str = try String.createInterned(self.alloc, buf, &strings);
        return IvyType.string(str);
    }

    pub fn is_falsey(self: *Self, value: IvyType) bool {
        _ = self;
        return value == .nil or (value.is_bool() and !value.as_bool());
    }

    // TODO catch chunk error
    pub fn rt_error(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        var i: usize = @intCast(self.frameCount);
        while (i > 0) {
            i -= 1;
            var frame = &self.frames.at(i);

            const instruction = @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.get_op_ptr()) - 1;
            const line = try frame.function.chunk.get_line_for_op(instruction);

            var function = frame.function;
            var name = function.name;
            std.debug.print("\n[line {}] ", .{line});
            if (name == null) {
                std.debug.print("script\n", .{});
            } else {
                std.debug.print("{s}()\n", .{name.?.asSlice()});
            }
        }
        self.resetStack();
        return InterpreterError.RuntimeError;
    }

    pub fn resetStack(self: *Self) void {
        self.stack.reset();
    }

    pub fn pushStack(self: *Self, value: IvyType) void {
        self.stack.push(value);
    }

    pub fn popStack(self: *Self) IvyType {
        return self.stack.pop();
    }

    pub fn peekStack(self: *Self, distance: usize) IvyType {
        return self.stack.peek(distance);
    }

    fn callValue(self: *Self, callee: IvyType, argCount: u8) !bool {
        if (callee.is_obj()) {
            switch (callee.object.ty) {
                .Function => {
                    return try self.call(callee.object_as(types.Function), argCount);
                },
                else => {},
            }
        }
        try self.rt_error("Can only call functions and classes", .{});
        return false;
    }

    fn call(self: *Self, fun: *types.Function, argc: u8) !bool {
        std.debug.print("call {s}\n", .{fun.getName()});
        if (argc != fun.arity) {
            try self.rt_error("Expected {} arguments but got {}.", .{ fun.arity, argc });
            return false;
        }
        if (self.frameCount == FRAMES_MAX) {
            try self.rt_error("Stack overflow.", .{});
            return false;
        }

        var frame = self.frames.top();

        self.frames.count += 1;
        frame.function = fun;
        frame.ip = fun.chunk.get_op_ptr();
        var frameStart = self.stack.count - argc - 1;
        frame.slots = self.stack.slice[frameStart..];

        return true;
    }

    fn binary_operation(self: *Self, op: OpCode) !void {
        const pa = self.peekStack(0);
        const pb = self.peekStack(1);

        if (pa != IvyType.num or pb != IvyType.num) {
            std.debug.print("Unsupported binary operation - pa: {s}, pb: {s}\n", .{ @tagName(pa), @tagName(pb) });
            try self.rt_error("Operands must be numbers.", .{});
            return InterpreterError.RuntimeError;
        }

        const b = self.popStack();
        const a = self.popStack();

        switch (op) {
            .DIVIDE => self.pushStack(IvyType.number(a.num / b.num)),
            .ADD => self.pushStack(IvyType.number(a.num + b.num)),
            .MULTIPLY => self.pushStack(IvyType.number(a.num * b.num)),
            .SUBTRACT => self.pushStack(IvyType.number(a.num - b.num)),
            .GREATER => self.pushStack(IvyType.boolean(a.num > b.num)),
            .LESS => self.pushStack(IvyType.boolean(a.num < b.num)),
            .EQUAL => self.pushStack(IvyType.boolean(types.eql(a, b))),
            else => return InterpreterError.RuntimeError,
        }
    }

    fn read_short(self: *Self, frame: *CallFrame) u16 {
        _ = self;
        var ptr = frame.*.ip;
        frame.ip += 2;
        var highBits: u16 = @as(u16, @intCast(ptr[0])) << @intCast(8);
        var lowBits: u16 = @intCast(ptr[1]);
        return highBits | lowBits;
    }

    fn read_byte(self: *Self, frame: *CallFrame) u8 {
        _ = self;
        const instruction = frame.ip[0];
        frame.ip += 1;
        return instruction;
    }

    fn read_constant(self: *Self, frame: *CallFrame) IvyType {
        return frame.function.chunk.constants.items[self.read_byte(frame)];
    }
};
