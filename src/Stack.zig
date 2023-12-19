const std = @import("std");

var arr: [10]u8 = undefined;

pub fn Stack(comptime T: type) type {
    return struct {
        const Self = @This();

        max: usize,
        count: usize,
        slice: []T,

        pub fn init(a: []T) Self {
            return Self{
                .max = a.len,
                .count = 0,
                .slice = a[0..a.len],
            };
        }

        pub fn at(self: *Self, index: usize) T {
            std.debug.assert(index < self.count);
            return self.slice[index];
        }

        pub fn top(self: *Self) *T {
            std.debug.assert(self.count >= 0);
            return &self.slice[self.count];
        }

        pub fn cur(self: *Self) *T {
            std.debug.assert(self.count > 0);
            return &self.slice[self.count - 1];
        }

        pub fn reset(self: *Self) void {
            self.count = 0;
        }

        pub fn push(self: *Self, value: T) void {
            std.debug.assert(self.count < self.max);
            self.slice[self.count] = value;
            self.count += 1;
        }

        pub fn pop(self: *Self) T {
            std.debug.assert(self.count > 0);
            self.count -= 1;
            return self.slice[self.count];
        }

        pub fn peek(self: *Self, distance: usize) T {
            std.debug.assert(self.count > (distance));
            return self.slice[self.count - 1 - distance];
        }
    };
}

test "basic" {
    var stack = Stack(u8).init(&arr);

    stack.push(1);
    var peek = stack.peek(0);
    try std.testing.expectEqual(peek, 1);
    try std.testing.expectEqual(stack.count, 1);

    stack.push(2);
    try std.testing.expectEqual(stack.count, 2);
    peek = stack.peek(0);
    try std.testing.expectEqual(peek, 2);
    peek = stack.peek(1);
    try std.testing.expectEqual(peek, 1);

    const pop = stack.pop();
    try std.testing.expectEqual(pop, 2);

    stack.reset();
    stack.push(3);
    peek = stack.peek(0);
    try std.testing.expectEqual(peek, 3);
    try std.testing.expectEqual(stack.count, 1);
}
