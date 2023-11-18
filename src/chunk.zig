const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const common = @import("common.zig");
const types = @import("types.zig");
const OpCode = common.OpCode;
const IvyType = common.IvyType;

const print = std.debug.print;
const String = types.String;
const Object = types.Object;

pub const ChunkError = error{OperationOutOfBounds};
pub const ChunkList = ArrayList(Chunk);
pub const LineInfo = struct { line: u32, inst_count: u32 };

pub const Instruction = union(enum) {
    const Self = @This();

    op: u8,
    line: u32,

    pub fn init(op: u8, line: u32) Self {
        return Self{ .op = op, .line = line };
    }
};

pub const Chunk = struct {
    const Self = @This();

    alloc: Allocator,
    code: std.ArrayList(u8),
    lines: std.ArrayList(LineInfo),
    constants: std.ArrayList(IvyType),

    pub fn init(alloc: Allocator) !Self {
        var constants = try std.ArrayList(IvyType).initCapacity(alloc, 8);
        var code = try std.ArrayList(u8).initCapacity(alloc, 8);
        var lineInfo = try std.ArrayList(LineInfo).initCapacity(alloc, 8);
        return Self{
            .alloc = alloc,
            .lines = lineInfo,
            .code = code,
            .constants = constants,
        };
    }

    pub fn get_op(self: *Self, idx: usize) u8 {
        return self.code.items[idx];
    }

    pub fn get_op_ptr(self: *Self) [*]u8 {
        return self.code.items.ptr;
    }

    pub fn get_line(self: *Self, idx: usize) *LineInfo {
        return &self.lines.items[idx];
    }

    pub fn get_line_ptr(self: *Self) [*]LineInfo {
        return self.lines.items.ptr;
    }

    pub fn get_constant(self: *Self, idx: usize) *IvyType {
        return &self.constants.items[idx];
    }

    pub fn add_constant(self: *Self, val: IvyType) Allocator.Error!usize {
        try self.constants.append(val);
        return self.constants.items.len - 1;
    }

    pub fn write(self: *Self, op: u8, line: u32) !void {
        try self.code.append(op);
        const line_len = self.lines.items.len;

        const sameLine = line_len > 0 and self.get_line(line_len - 1).line == line;
        if (sameLine) {
            var cur_line = self.get_line(line_len - 1);
            cur_line.inst_count += 1;
        } else {
            var info = LineInfo{ .line = line, .inst_count = 1 };
            try self.lines.append(info);
        }
    }

    pub fn get_line_for_op(self: *Self, op_idx: usize) !u32 {
        if (op_idx == 0) {
            return self.get_line(op_idx).line;
        }

        if (op_idx > self.code.items.len) {
            return ChunkError.OperationOutOfBounds;
        }

        var line_idx: usize = 0;
        var cnt: usize = 0;

        while (line_idx < op_idx and line_idx < self.lines.items.len) {
            var cur = self.get_line(line_idx);
            cnt += cur.inst_count;
            if (cnt > op_idx) {
                return cur.line;
            }

            line_idx += 1;
        }

        unreachable;
    }

    pub fn deinit(self: *Self) void {
        self.constants.deinit();
        self.code.deinit();
        self.lines.deinit();
    }
};

test "Chunk.basic" {
    const alloc = std.testing.allocator;
    {
        var cnk = try Chunk.init(alloc);
        defer cnk.deinit();

        var constant = try cnk.add_constant(IvyType.number(1.2));
        var constant2 = try cnk.add_constant(IvyType.number(3));

        var string = try String.copy(alloc, "Hello");
        defer string.deinit(alloc);
        var constant3 = try cnk.add_constant(IvyType.string(string));

        try cnk.write(@intFromEnum(OpCode.CONSTANT), 123);
        try cnk.write(@as(u8, @intCast(constant)), 123);
        try cnk.write(@intFromEnum(OpCode.NEGATE), 123);
        try cnk.write(@intFromEnum(OpCode.CONSTANT), 124);
        try cnk.write(@as(u8, @intCast(constant2)), 124);
        try cnk.write(@intFromEnum(OpCode.CONSTANT), 124);
        try cnk.write(@as(u8, @intCast(constant3)), 124);
        try cnk.write(@intFromEnum(OpCode.RETURN), 125);

        var cns1 = cnk.get_constant(0).*;
        try std.testing.expect(cns1.num == 1.2);

        var cns2 = cnk.get_constant(1).*;
        try std.testing.expect(cns2.num == 3);

        var op0 = try cnk.get_line_for_op(0);
        try std.testing.expect(op0 == 123);

        var op1 = try cnk.get_line_for_op(1);
        try std.testing.expect(op1 == 123);

        var op2 = try cnk.get_line_for_op(3);
        try std.testing.expect(op2 == 124);

        var op5 = try cnk.get_line_for_op(5);
        try std.testing.expect(op5 == 124);

        var op6 = try cnk.get_line_for_op(7);
        try std.testing.expect(op6 == 125);
    }
}
