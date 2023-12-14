const std = @import("std");
const common = @import("common.zig");
const String = @import("object/string.zig").String;
const VM = @import("vm.zig").VirtualMachine;

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

pub fn dump_stack(vm: *VM) void {
    for (0..vm.stack.count) |i| {
        var item = vm.stack.slice[i];
        switch (item) {
            .object => |obj| {
                switch (obj.ty) {
                    .String => std.debug.print("        | [ string ", .{}),
                    .Function => std.debug.print("        | [ ", .{}),
                    .NativeFunction => std.debug.print("        | [ ", .{}),
                }
            },
            else => std.debug.print("        | [ {s} ", .{@tagName(item)}),
        }
        item.print();
        std.debug.print(" ]\n", .{});
    }
}

pub fn disassemble_instruction(chunk: *Chunk, offset: usize) ChunkError!usize {
    std.debug.print("{d:0>4} ", .{offset});
    const line = try chunk.get_line_for_op(offset);
    const prev_line = if (offset > 0) try chunk.get_line_for_op(offset - 1) else null;
    const instruction = @as(OpCode, @enumFromInt(chunk.get_op(offset)));

    if (offset > 0 and line == prev_line) {
        std.debug.print("   > ", .{});
    } else {
        std.debug.print("{d:>4} ", .{line});
    }

    return switch (instruction) {
        .RETURN => simpleInstruction("RETURN", offset),
        .CONSTANT => constantInstruction("CONSTANT", chunk, offset),
        .NEGATE => simpleInstruction("NEGATE", offset),
        .ADD => simpleInstruction("ADD", offset),
        .SUBTRACT => simpleInstruction("SUBTRACT", offset),
        .DIVIDE => simpleInstruction("DIVIDE", offset),
        .MULTIPLY => simpleInstruction("MULTIPLY", offset),
        .FALSE => simpleInstruction("FALSE", offset),
        .TRUE => simpleInstruction("TRUE", offset),
        .NIL => simpleInstruction("NIL", offset),
        .NOT => simpleInstruction("NOT", offset),
        .EQUAL => simpleInstruction("EQUAL", offset),
        .LESS => simpleInstruction("LESS", offset),
        .GREATER => simpleInstruction("GREATER", offset),
        .PRINT => simpleInstruction("PRINT", offset),
        .POP => simpleInstruction("POP", offset),
        .POP_N => constantInstruction("POP_N", chunk, offset),
        .DEFINE_GLOBAL => constantInstruction("DEFINE_GLOBAL", chunk, offset),
        .GET_LOCAL => byteInstruction("GET_LOCAL", chunk, offset),
        .SET_LOCAL => byteInstruction("SET_LOCAL", chunk, offset),
        .GET_GLOBAL => constantInstruction("GET_GLOBAL", chunk, offset),
        .SET_GLOBAL => constantInstruction("SET_GLOBAL", chunk, offset),
        .LOOP => jumpInstruction("LOOP", -1, chunk, offset),
        .JUMP => jumpInstruction("JUMP", 1, chunk, offset),
        .JUMP_IF_FALSE => jumpInstruction("JUMP_IF_FALSE", 1, chunk, offset),
        .CALL => byteInstruction("CALL", chunk, offset),
        //else => {
        //    std.debug.print("Unknown opcode {}\n", .{instruction});
        //    return offset + 1;
        //},
    };
}

pub fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var slot = chunk.get_op(offset + 1);
    std.debug.print("{s} {d}\n", .{ name, slot });
    return offset + 2;
}

pub fn jumpInstruction(name: []const u8, sign: i8, chunk: *Chunk, offset: usize) usize {
    var jump: u16 = @as(u16, @intCast(chunk.get_op(offset + 1))) << 8;
    jump |= chunk.get_op(offset + 2);
    var offsetIncr = @as(i32, @intCast(offset)) + 3;
    var signedJump = @as(i32, @intCast(jump)) * sign;

    std.debug.print("{s} {d} -> {d}\n", .{ name, offset, offsetIncr + signedJump });
    return offset + 3;
}

pub fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var constant = chunk.get_op(offset + 1);
    var ty = chunk.get_constant(constant).*;

    std.debug.print("{s} {d} ", .{ name, constant });
    ty.print();
    std.debug.print("\n", .{});
    return offset + 2;
}
