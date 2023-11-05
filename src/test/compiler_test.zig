const std = @import("std");
const OpCode = @import("../common.zig").OpCode;

pub fn assert_compiled(expected: []OpCode, code: std.ArrayListUnmanaged(u8)) !void {
    var expPtr: usize = 0;
    var gotPtr: usize = 0;

    while (expPtr < expected.len) {
        var got = code.items[gotPtr];
        // If OP_CONSTANT
        if (got == 0) {
            gotPtr += 1;
        }

        var exp = expected[expPtr];
        std.debug.print("Expected: {}, Got: {}\n", .{ @enumToInt(exp), got });
        try std.testing.expect(got == @enumToInt(exp));

        gotPtr += 1;
        expPtr += 1;
    }
}
