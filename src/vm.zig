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

const Chunk = chunk.Chunk;
const ChunkError = chunk.ChunkError;
const IvyType = common.IvyType;
const OpCode = common.OpCode;
const Object = types.Object;
const String = types.String;

pub const STACK_MAX = 256;
pub const InterpreterError = error{
    CompiletimeError,
    RuntimeError,
};

// TODO: Look for more efficient ways to store globals
pub var globals: table.Table = undefined;
pub var strings: table.Table = undefined;

pub const VirtualMachine = struct {
    const Self = @This();

    ip: [*]u8,
    chunk: *Chunk,
    stack: std.ArrayList(IvyType),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) !Self {
        var stack = try std.ArrayList(IvyType).initCapacity(alloc, STACK_MAX);
        globals = try table.init(alloc, 8);
        strings = try table.init(alloc, 8);
        return VirtualMachine{
            .chunk = undefined,
            .ip = undefined,
            .alloc = alloc,
            .stack = stack,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        garbage.free(self.alloc);
        strings.deinit();
        globals.deinit();
    }

    /// Interpret a source string and return the value of the RETURN operation
    pub fn interpret(self: *Self, source: [:0]u8) !void {
        var cnk: Chunk = try Chunk.init(self.alloc);
        defer cnk.deinit();

        var scanner = try Scanner.init(self.alloc, source);
        var comp = Parser.init(self.alloc, &scanner);
        var compiled = try comp.compile(&cnk);

        if (!compiled) {
            return InterpreterError.CompiletimeError;
        }

        self.chunk = &cnk;
        self.ip = self.chunk.get_op_ptr();
        try self.run();
    }

    pub fn run(self: *Self) !void {
        while (true) {
            if (common.DEBUG_PRINT_CODE) {
                debug.dump_stack(self);
                const offset = @intFromPtr(self.ip) - @intFromPtr(self.chunk.get_op_ptr());
                _ = debug.disassemble_instruction(self.chunk, offset) catch |err| {
                    std.debug.print("{any}\n", .{err});
                    return InterpreterError.RuntimeError;
                };
            }

            const byte = self.read_byte();
            const instruction: OpCode = @enumFromInt(byte);

            switch (instruction) {
                .CONSTANT => try self.stack.append(self.read_constant()),
                .NEGATE => b: {
                    const value = self.peek_stack(0);
                    switch (value) {
                        .num => {
                            const num = self.stack.pop().num;
                            try self.stack.append(IvyType.number(-num));
                            break :b;
                        },
                        else => {
                            try self.rt_error("Operand must be a number.", .{});
                            return InterpreterError.RuntimeError;
                        },
                    }
                    try self.stack.append(value);
                },
                .TRUE => try self.stack.append(IvyType.boolean(true)),
                .FALSE => try self.stack.append(IvyType.boolean(false)),
                .NIL => try self.stack.append(IvyType.nil()),
                .NOT => b: {
                    const value = !self.stack.pop().as_bool();
                    try self.stack.append(IvyType.boolean(value));
                    break :b;
                },
                .ADD => blk: {
                    var pa = self.peek_stack(0);
                    var pb = self.peek_stack(1);

                    if (pa.is_string() and pb.is_string()) {
                        var b = self.stack.pop().object_as(String);
                        var a = self.stack.pop().object_as(String);
                        var str = try self.concatenate(a, b);
                        try self.stack.append(str);
                    } else if (pa.is_num() and pb.is_num()) {
                        var nb = self.stack.pop().num;
                        var na = self.stack.pop().num;
                        try self.stack.append(IvyType.number(na + nb));
                    } else {
                        try self.rt_error("Operands must be two numbers or two strings.", .{});
                        return InterpreterError.RuntimeError;
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
                    var slot = self.read_byte();
                    try self.stack.append(self.stack.items[slot]);
                },
                .SET_LOCAL => {
                    var slot = self.read_byte();
                    self.stack.items[slot] = self.peek_stack(0);
                },
                .DEFINE_GLOBAL => {
                    var name = self.read_constant().object_as(String);
                    var isSet = try globals.set(name, self.peek_stack(0));
                    if (!isSet) {
                        try self.rt_error("Redefining existing variable '{s}'.", .{name.asSlice()});
                        return InterpreterError.RuntimeError;
                    }
                    _ = self.stack.pop();
                },
                .GET_GLOBAL => {
                    var name = self.read_constant().object_as(String);
                    var global = globals.get(name);
                    if (global == null) {
                        try self.rt_error("Undefined variable '{s}'.", .{name.asSlice()});
                        return InterpreterError.RuntimeError;
                    }
                    try self.stack.append(global.?);
                },
                .SET_GLOBAL => {
                    var name = self.read_constant().object_as(String);
                    var set = try globals.set(name, self.peek_stack(0));
                    if (set) {
                        _ = globals.delete(name);
                        // NOTE: No implicit variable declaration!
                        try self.rt_error("Undefined variable '{s}'.", .{name.asSlice()});
                        return InterpreterError.RuntimeError;
                    }
                },
                .JUMP => {
                    var offset = self.read_short();
                    self.ip += offset;
                },
                .JUMP_IF_FALSE => {
                    var offset = self.read_short();
                    if (self.is_falsey(self.peek_stack(0))) {
                        self.ip += offset;
                    }
                },
                // TODO: implement loop as JUMP
                .LOOP => {
                    var offset = self.read_short();
                    self.ip -= offset;
                },
                .PRINT => {
                    var value = self.stack.pop();
                    value.print();
                    std.debug.print("\n", .{});
                },
                .POP => {
                    _ = self.stack.pop();
                },
                .POP_N => {
                    var n = self.read_byte();
                    var i: usize = 0;
                    while (i < n) : (i += 1) {
                        _ = self.stack.pop();
                    }
                },
                .RETURN => {
                    break;
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

    pub fn rt_error(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const instruction = @intFromPtr(self.ip) - @intFromPtr(self.chunk.get_op_ptr()) - 1;
        const line = try self.chunk.get_line_for_op(instruction);
        std.debug.print("\n[line {}] ERR: ", .{line});
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
    }

    pub fn peek_stack(self: *Self, distance: usize) IvyType {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn binary_operation(self: *Self, op: OpCode) !void {
        const pa = self.peek_stack(0);
        const pb = self.peek_stack(1);

        if (pa != IvyType.num or pb != IvyType.num) {
            std.debug.print("Unsupported binary operation - pa: {s}, pb: {s}\n", .{ @tagName(pa), @tagName(pb) });
            try self.rt_error("Operands must be numbers.", .{});
            return InterpreterError.RuntimeError;
        }

        const b = self.stack.pop();
        const a = self.stack.pop();

        switch (op) {
            .DIVIDE => try self.stack.append(IvyType.number(a.num / b.num)),
            .ADD => try self.stack.append(IvyType.number(a.num + b.num)),
            .MULTIPLY => try self.stack.append(IvyType.number(a.num * b.num)),
            .SUBTRACT => try self.stack.append(IvyType.number(a.num - b.num)),
            .GREATER => try self.stack.append(IvyType.boolean(a.num > b.num)),
            .LESS => try self.stack.append(IvyType.boolean(a.num < b.num)),
            .EQUAL => try self.stack.append(IvyType.boolean(types.eql(a, b))),
            else => return InterpreterError.RuntimeError,
        }
    }

    fn read_short(self: *Self) u16 {
        var ptr = self.ip;
        self.ip += 2;
        var highBits: u16 = @as(u16, @intCast(ptr[0])) << @intCast(8);
        var lowBits: u16 = @intCast(ptr[1]);
        return highBits | lowBits;
    }

    fn read_byte(self: *Self) u8 {
        const instruction = self.ip[0];
        self.ip += 1;
        return instruction;
    }

    fn read_constant(self: *Self) IvyType {
        return self.chunk.constants.items[self.read_byte()];
    }
};

test "test interpreter" {
    var alloc = std.heap.page_allocator;
    var vm = try VirtualMachine.init(alloc);
    vm.strings = try table.Table.init(alloc, 8);
    defer vm.deinit();
    const source = "1 + 2 * 3 - 4 / 5";
    try vm.interpret(@constCast(source));
}
