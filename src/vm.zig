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

const DEBUG = true;

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
    // TEMP:
    retval: ?IvyType = null,

    pub fn init(alloc: std.mem.Allocator) !Self {
        var stack = try std.ArrayListUnmanaged(IvyType).initCapacity(alloc, STACK_MAX);
        return VirtualMachine{ .chunk = undefined, .ip = undefined, .alloc = alloc, .stack = stack };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit(self.alloc);
    }

    pub fn free() !void {}

    pub fn interpret(self: *Self, source: [:0]u8) !IvyType {
        var cnk: Chunk = try Chunk.init(self.alloc);
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

        return try self.run();
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

    pub fn run(self: *Self) !IvyType {
        while (true) {
            if (DEBUG) {
                const offset = @intFromPtr(self.ip) - @intFromPtr(self.chunk.get_op_ptr());

                std.debug.print(" \n", .{});
                for (0..self.stack.items.len) |i| {
                    var item = self.stack.items[i];
                    std.debug.print("[ {any} {any} ]\n", .{ item, &item });
                }

                _ = debug.disassemble_instruction(self.chunk, offset) catch |err| {
                    std.debug.print("{any}", .{err});
                    return InterpreterError.RuntimeError;
                };
            }

            const byte = self.read_byte();
            const instruction: OpCode = @enumFromInt(byte);

            switch (instruction) {
                .CONSTANT => {
                    const constant = self.read_constant();
                    try self.stack.append(self.alloc, constant);
                },
                .NEGATE => {
                    const value = self.peek_stack(0);
                    switch (value) {
                        .num => {
                            const num = self.stack.pop().num;
                            try self.stack.append(self.alloc, types.number(-num));
                        },
                        else => {
                            try self.rt_error("Operand must be a number.", .{});
                            return InterpreterError.RuntimeError;
                        },
                    }
                    try self.stack.append(self.alloc, value);
                },
                .TRUE => {
                    try self.stack.append(self.alloc, types.boolean(true));
                },
                .FALSE => {
                    try self.stack.append(self.alloc, types.boolean(false));
                },
                .NIL => {
                    try self.stack.append(self.alloc, types.nil());
                },
                .NOT => {
                    const value = self.stack.pop().to_bool();
                    try self.stack.append(self.alloc, types.boolean(value));
                },
                .ADD => {
                    try self.binary_operation(instruction);
                },
                .SUBTRACT => {
                    try self.binary_operation(instruction);
                },
                .DIVIDE => {
                    try self.binary_operation(instruction);
                },
                .MULTIPLY => {
                    try self.binary_operation(instruction);
                },
                .EQUAL => {
                    try self.binary_operation(instruction);
                },
                .LESS => {
                    try self.binary_operation(instruction);
                },
                .GREATER => {
                    try self.binary_operation(instruction);
                },
                .RETURN => {
                    const value = self.stack.pop();
                    std.debug.print("---\nRETURN: {any}\n---\n", .{value});
                    return value;
                },
            }
        }
    }

    fn read_byte(self: *Self) u8 {
        const instruction = self.ip[0];
        self.ip += 1;
        return instruction;
    }

    fn binary_operation(self: *Self, op: OpCode) !void {
        const pa = self.peek_stack(0);
        const pb = self.peek_stack(1);

        if (pa != IvyType.num or pb != IvyType.num) {
            try self.rt_error("Operands must be numbers.", .{});
            return InterpreterError.RuntimeError;
        }

        const b = self.stack.pop().num;
        const a = self.stack.pop().num;

        switch (op) {
            .DIVIDE => try self.stack.append(self.alloc, types.number(a / b)),
            .ADD => try self.stack.append(self.alloc, types.number(a + b)),
            .MULTIPLY => try self.stack.append(self.alloc, types.number(a * b)),
            .SUBTRACT => try self.stack.append(self.alloc, types.number(a - b)),
            else => return InterpreterError.RuntimeError,
        }
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
