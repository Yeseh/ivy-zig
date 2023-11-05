const std = @import("std");
const common = @import("common.zig");

const Chunk = common.Chunk;
const ChunkError = common.ChunkError;
const OpCode = common.OpCode;

pub fn disassemble_chunk(chunk: *Chunk, name: []const u8) !void {
    std.debug.print("=== {s} ===\n", .{name});
    std.debug.print("== Instructions ==\n", .{});
    var offset: usize = 0;

    while (offset < chunk.code.items.len) {
        offset = try disassemble_instruction(chunk, offset);
    }

    disassemble_lines(chunk);
}

pub fn disassemble_lines(chunk: *Chunk) void {
    std.debug.print("== Lines ==\n", .{});

    for (chunk.lines.items) |line| {
        std.debug.print("Line: {d} ({d})\n", .{ line.line, line.inst_count });
    }
}

pub fn disassemble_instruction(chunk: *Chunk, offset: usize) ChunkError!usize {
    std.debug.print("{d:0>4} ", .{offset});
    const line = try chunk.get_line_for_op(offset);
    const prev_line = if (offset > 0) try chunk.get_line_for_op(offset - 1) else null;
    const instruction = @intToEnum(OpCode, chunk.get_op(offset));

    if (offset > 0 and line == prev_line) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:>4} ", .{line});
    }

    return switch (instruction) {
        .OP_RETURN => simple_instruction("OP_RETURN", offset),
        .OP_CONSTANT => constant_instruction("OP_CONSTANT", chunk, offset),
        .OP_NEGATE => simple_instruction("OP_NEGATE", offset),
        .OP_ADD => simple_instruction("OP_ADD", offset),
        .OP_SUBTRACT => simple_instruction("OP_SUBTRACT", offset),
        .OP_DIVIDE => simple_instruction("OP_DIVIDE", offset),
        .OP_MULTIPLY => simple_instruction("OP_MULTIPLY", offset),
        //else => {
        //    std.debug.print("Unknown opcode {}\n", .{instruction});
        //    return offset + 1;
        //},
    };
}

pub fn simple_instruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constant_instruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var constant = chunk.get_op(offset + 1);
    std.debug.print("{s} {d} '{d}'\n", .{ name, constant, chunk.get_constant(constant).* });
    return offset + 2;
}
