const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const types = @import("types.zig");
const chunk = @import("chunk.zig");
const compiler = @import("compiler.zig");
const Compiler = compiler.Compiler;
const Scanner = @import("scanner.zig").Scanner;

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

pub const VirtualMachine = struct {
    const Self = @This();

    ip: [*]u8,
    chunk: *Chunk,
    stack: std.ArrayListUnmanaged(IvyType),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) !Self {
        var stack = try std.ArrayListUnmanaged(IvyType).initCapacity(alloc, STACK_MAX);
        return VirtualMachine{ .chunk = undefined, .ip = undefined, .alloc = alloc, .stack = stack };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit(self.alloc);
    }

    /// Interpret a source string and return the value of the RETURN operation
    pub fn interpret(self: *Self, source: [:0]u8) !IvyType {
        var cnk: Chunk = try Chunk.init(self.alloc);
        // TODO: Or maybe write the chunk somewhere for a next pass?
        defer cnk.deinit();

        // TODO: Do this at comptime/make these global?
        var scanner = try Scanner.init(self.alloc, source);
        var comp = Compiler.init(self.alloc, &scanner);
        var compiled = try comp.compile(&cnk);

        if (!compiled) {
            return InterpreterError.CompiletimeError;
        }

        self.chunk = &cnk;
        self.ip = self.chunk.get_op_ptr();

        var retval = try self.run();
        std.debug.print("\n=> ", .{});
        retval.print();
        std.debug.print("\n", .{});

        return retval;
    }

    pub fn run(self: *Self) !IvyType {
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
                .CONSTANT => try self.stack.append(self.alloc, self.read_constant()),
                .NEGATE => b: {
                    const value = self.peek_stack(0);
                    switch (value) {
                        .num => {
                            const num = self.stack.pop().num;
                            try self.stack.append(self.alloc, IvyType.number(-num));
                            break :b;
                        },
                        else => {
                            try self.rt_error("Operand must be a number.", .{});
                            return InterpreterError.RuntimeError;
                        },
                    }
                    try self.stack.append(self.alloc, value);
                },
                .TRUE => try self.stack.append(self.alloc, IvyType.boolean(true)),
                .FALSE => try self.stack.append(self.alloc, IvyType.boolean(false)),
                .NIL => try self.stack.append(self.alloc, IvyType.nil()),
                .NOT => b: {
                    const value = !self.stack.pop().to_bool();
                    try self.stack.append(self.alloc, IvyType.boolean(value));
                    break :b;
                },
                .ADD => blk: {
                    var a = self.peek_stack(0);
                    var b = self.peek_stack(1);

                    if (a.is_string() and b.is_string()) {
                        self.concatenate();
                    } else if (a.is_num() and b.is_num()) {
                        b = self.stack.pop().num;
                        a = self.stack.pop().num;
                        try self.stack.append(self.alloc, IvyType.number(a + b));
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
                .RETURN => {
                    var value = if (self.stack.items.len == 0) IvyType.nil() else self.stack.pop();
                    return value;
                },
            }
        }
    }

    pub fn concatenate(self: *Self) void {
        var a = self.stack.pop().as_obj_type(String);
        var b = self.stack.pop().as_obj_type(String);

        var str = try String.initCapacity(self.alloc, a._len + b._len);
        str.appendSlice(self.alloc, a.asSlice());
        str.appendSlice(self.alloc, b.asSlice());

        try self.stack.append(self.alloc, IvyType.string(str));
    }

    pub fn is_falsey(self: *Self, value: IvyType) bool {
        _ = self;
        return value == .nil or (value.is_bool() and !value.as_bool());
    }

    pub fn rt_error(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        std.debug.print(fmt, args);
        const instruction = @intFromPtr(self.ip) - @intFromPtr(self.chunk.get_op_ptr()) - 1;
        const line = try self.chunk.get_line_for_op(instruction);
        std.debug.print("\n[line {}] in script\n", .{line});
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

        const b = self.stack.pop().num;
        const a = self.stack.pop().num;

        switch (op) {
            .DIVIDE => try self.stack.append(self.alloc, IvyType.number(a / b)),
            .ADD => try self.stack.append(self.alloc, IvyType.number(a + b)),
            .MULTIPLY => try self.stack.append(self.alloc, IvyType.number(a * b)),
            .SUBTRACT => try self.stack.append(self.alloc, IvyType.number(a - b)),
            .GREATER => try self.stack.append(self.alloc, IvyType.boolean(a > b)),
            .LESS => try self.stack.append(self.alloc, IvyType.boolean(a < b)),
            .EQUAL => try self.stack.append(self.alloc, IvyType.eql(a, b)),
            else => return InterpreterError.RuntimeError,
        }
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
    defer vm.deinit();
    const source = "1 + 2 * 3 - 4 / 5";
    try vm.interpret(source);
}
