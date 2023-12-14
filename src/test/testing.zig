const std = @import("std");
const common = @import("../common.zig");
const VM = @import("../vm.zig").VirtualMachine;
const types = @import("../types.zig");

const OpCode = common.OpCode;
const IvyType = common.IvyType;
const Object = types.Object;
const String = types.String;

pub fn assertCompiled(expected: []OpCode, code: std.ArrayList(u8)) !void {
    var expPtr: usize = 0;
    var gotPtr: usize = 0;

    while (expPtr < expected.len) {
        var got = code.items[gotPtr];
        // If CONSTANT
        if (got == 0) {
            gotPtr += 1;
        }

        var exp = expected[expPtr];
        try std.testing.expectEqual(got, @intFromEnum(exp));

        gotPtr += 1;
        expPtr += 1;
    }
}

pub const VmTest = union(enum) {
    ok: IvyType,
    runtimeErr: common.RuntimeError,
};

pub fn runVm(allocator: std.mem.Allocator, source: [:0]const u8, expected: VmTest) !void {
    var vm = try VM.init(allocator);
    defer vm.deinit();

    var val = try vm.interpret(@constCast(source)); //catch |err| {
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
            try std.testing.expectEqual(expected.bool, actual.bool);
        },
        .nil => {
            try std.testing.expect(actual == .nil);
        },
        .num => {
            try std.testing.expect(actual == .num);
            try std.testing.expectEqual(expected.num, actual.num);
        },
        .object => {
            try std.testing.expectEqual(expected.object.ty, actual.object.ty);
            switch (expected.object.ty) {
                .String => {
                    var act = actual.object.as(String);
                    var exp = expected.object.as(String);
                    try std.testing.expectEqualStrings(exp.asSlice(), act.asSlice());
                },
            }
        },
    }
}
