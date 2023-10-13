const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const chunk = @import("chunk.zig");

const Chunk = chunk.Chunk;
const ChunkError = chunk.ChunkError;
const Value = common.Value;
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
    stack: std.ArrayListUnmanaged(Value),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) !Self {
        return VirtualMachine{ .chunk = undefined, .ip = undefined, .alloc = alloc, .stack = undefined };
    }

    pub fn deinit(self: *Self) !void {
        self.stack.deinit(self.allocator);
    }

    pub fn free() !void {}

    pub fn interpret(self: *Self, cnk: *Chunk) !void {
        self.chunk = cnk;
        self.ip = self.chunk.get_op_ptr();
        self.stack = try std.ArrayListUnmanaged(Value).initCapacity(self.alloc, STACK_MAX);

        run: while (true) {
            if (DEBUG) {
                const offset = @ptrToInt(self.ip) - @ptrToInt(self.chunk.get_op_ptr());

                std.debug.print(" \n", .{});
                for (0..self.stack.items.len) |i| {
                    var item = &self.stack.items[i];
                    std.debug.print("[ {d} {} ]\n", .{ item.*, item });
                }

                _ = debug.disassemble_instruction(self.chunk, offset) catch |err| {
                    std.debug.print("{any}", .{err});
                    return InterpreterError.RuntimeError;
                };
            }

            const byte = self.read_byte();
            const instruction = @intToEnum(OpCode, byte);
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.read_constant();
                    try self.stack.append(self.alloc, constant);
                },
                .OP_NEGATE => {
                    const value = -self.stack.pop();
                    try self.stack.append(self.alloc, value);
                },
                .OP_ADD => {
                    try self.binary_operation(instruction);
                },
                .OP_SUBTRACT => {
                    try self.binary_operation(instruction);
                },
                .OP_DIVIDE => {
                    try self.binary_operation(instruction);
                },
                .OP_MULTIPLY => {
                    try self.binary_operation(instruction);
                },
                .OP_RETURN => {
                    const value = self.stack.pop();
                    std.debug.print("---\nRETURN: {d}\n---\n", .{value});
                    break :run;
                },
                //else => {
                //    std.debug.print("Unknown OP byte {d}\n", .{instruction});
                //   return InterpreterError.RuntTimeError;
                //},
            }
        }
    }

    fn read_byte(self: *Self) u8 {
        const instruction = self.ip[0];
        self.ip += 1;
        return instruction;
    }

    fn binary_operation(self: *Self, op: OpCode) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        switch (op) {
            .OP_DIVIDE => try self.stack.append(self.alloc, a / b),
            .OP_ADD => try self.stack.append(self.alloc, a + b),
            .OP_MULTIPLY => try self.stack.append(self.alloc, a * b),
            .OP_SUBTRACT => try self.stack.append(self.alloc, a - b),
            else => return InterpreterError.RuntimeError,
        }
    }

    fn read_constant(self: *Self) Value {
        return self.chunk.constants.items[self.read_byte()];
    }
};
