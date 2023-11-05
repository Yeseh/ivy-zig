const std = @import("std");
const common = @import("common.zig");
const String = @import("object/String.zig");

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
        .RETURN => simple_instruction("RETURN", offset),
        .CONSTANT => constant_instruction("CONSTANT", chunk, offset),
        .NEGATE => simple_instruction("NEGATE", offset),
        .ADD => simple_instruction("ADD", offset),
        .SUBTRACT => simple_instruction("SUBTRACT", offset),
        .DIVIDE => simple_instruction("DIVIDE", offset),
        .MULTIPLY => simple_instruction("MULTIPLY", offset),
        .FALSE => simple_instruction("FALSE", offset),
        .TRUE => simple_instruction("TRUE", offset),
        .NIL => simple_instruction("NIL", offset),
        .NOT => simple_instruction("NOT", offset),
        .EQUAL => simple_instruction("EQUAL", offset),
        .LESS => simple_instruction("LESS", offset),
        .GREATER => simple_instruction("GREATER", offset),
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
    var ty = chunk.get_constant(constant).*;
    switch (ty) {
        .num => {
            std.debug.print("{s} {d} '{d}'\n", .{ name, constant, ty.num });
        },
        // .obj => {
        //     switch (ty.obj.type) {
        //         .String => {
        //             var string = String.from_obj(ty.obj);
        //             std.debug.print("{s} {d} '{s}'\n", .{ name, constant, string.chars.items });
        //         },
        //     }
        //     std.debug.print("{s} {d} '{any}'\n", .{ name, constant, ty.obj });
        // },
        .bool => {
            std.debug.print("{s} {d} '{}'\n", .{ name, constant, ty.bool });
        },
        .nil => {
            std.debug.print("{s} {d} 'nil'\n", .{ name, constant });
        },
        .object => {
            var str = @ptrCast(*String, @alignCast(@alignOf(*String), ty.object));
            _ = str;
            std.debug.print("{s} {d} 'string'\n", .{ name, constant });
        },
    }

    return offset + 2;
}
