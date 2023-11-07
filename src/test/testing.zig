const std = @import("std");
const common = @import("../common.zig");
const VM = @import("../vm.zig").VirtualMachine;
const types = @import("../types.zig");

const OpCode = common.OpCode;
const IvyType = common.IvyType;
const Object = types.Object;
const String = types.String;

pub fn assertCompiled(expected: []OpCode, code: std.ArrayListUnmanaged(u8)) !void {
    var expPtr: usize = 0;
    var gotPtr: usize = 0;

    while (expPtr < expected.len) {
        var got = code.items[gotPtr];
        // If CONSTANT
        if (got == 0) {
            gotPtr += 1;
        }

        var exp = expected[expPtr];
        std.debug.print("Expected: {}, Got: {}\n", .{ @intFromEnum(exp), got });
        try std.testing.expect(got == @intFromEnum(exp));

        gotPtr += 1;
        expPtr += 1;
    }
}

pub const VmTest = union(enum) {
    ok: IvyType,
    runtimeErr: common.RuntimeError,
};

pub fn runVm(allocator: std.mem.Allocator, source: []const u8, expected: VmTest) !void {
    std.debug.print("Running: {s}\n", .{source});
    var vm = try VM.init(allocator);
    defer vm.deinit();

    var sent_buf: [:0]u8 = try allocator.allocSentinel(u8, source.len, 0);
    @memcpy(sent_buf, source.ptr);
    defer allocator.free(sent_buf);

    var val = try vm.interpret(sent_buf); //catch |err| {
    switch (expected) {
        .ok => try assertReturn(expected.ok, val),
        else => unreachable,
        // .runtimeErr => try std.testing.expectError(expected.runtimeErr, val),
    }
}

pub fn assertReturn(expected: IvyType, actual: IvyType) !void {
    switch (expected) {
        .bool => {
            try std.testing.expect(actual == .bool);
            try std.testing.expect(expected.bool == actual.bool);
        },
        .nil => {
            try std.testing.expect(actual == .nil);
        },
        .num => {
            try std.testing.expect(actual == .num);
            try std.testing.expect(expected.num == actual.num);
        },
        .object => {
            try std.testing.expect(actual == .object);
            try std.testing.expect(actual.object.ty == expected.object.ty);
            switch (expected.object.ty) {
                .String => {
                    var exp = IvyType.as_obj_type(String, expected.object);
                    var act = IvyType.as_obj_type(String, actual.object);
                    try std.testing.expectEqual(exp.asSlice(), act.asSlice());
                },
            }
            try std.testing.expect(expected.object == actual.object);
        },
    }
}
