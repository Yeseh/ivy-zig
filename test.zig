const std = @import("std");
const assert = std.testing.expect;
const print = std.debug.print;

const Test = enum(u8) {
    A = 1,
    B = 2,
    C = 3,
};

test "test" {
    comptime var ti = @typeInfo(Test).Enum;
    try assert(ti.fields.len == 3);
    try assert(ti.fields[0].value == 1);
}
