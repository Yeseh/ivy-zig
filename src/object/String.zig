const std = @import("std");
const Object = @import("Object.zig");
const ObjectType = Object.Type;

pub const String = struct {
    chars: std.ArrayList(u8),

    pub fn object(self: *String) Object {
        return .{ .ptr = self, .ty = .String, .vtable = &.{ .print = _print } };
    }

    fn _print(ctx: *anyopaque) void {
        const self: *String = from_obj(ctx);
        print(self);
    }

    pub fn print(self: *String) void {
        std.debug.print("String: {s}\n", .{self.chars.items});
    }

    /// Initializes a new string object with the provided capacity
    /// The string will be initialized as undefined memory with a null-terminator
    pub fn init_capacity(alloc: std.mem.Allocator, capacity: usize) !*String {
        var buf = try alloc.allocSentinel(u8, capacity, 0);
        var string = try alloc.create(String);
        string.chars = try std.ArrayList(u8).init(alloc, buf);
        return string;
    }

    /// Takes ownership of the provided buffer and initializes a new string object
    pub fn init(alloc: std.mem.Allocator, buf: [:0]u8) !*String {
        var string = try alloc.create(String);
        var chrs = std.ArrayList(u8).fromOwnedSliceSentinel(alloc, 0, buf);
        string.chars = chrs;
        return string;
    }

    /// Clears the internal character array and destroys the heap-allocated string.
    /// Should be called with the same allocator that was used to initialize the string
    pub fn deinit(self: *String, alloc: std.mem.Allocator) void {
        self.chars.deinit();
        alloc.destroy(self);
    }

    pub fn len(self: *String) usize {
        return self.chars.items.len;
    }

    /// Copies the provided slice into a new string object
    pub fn from_slice(alloc: std.mem.Allocator, slice: []const u8) !*String {
        // Ensure we have a null-terminated slice
        var buf = try alloc.allocSentinel(u8, slice.len, 0);
        @memcpy(buf, slice.ptr);
        std.debug.print("copy_string: {s}\n", .{buf});
        // We can use fromOwnedSliceSentinel here because we used the same allocator to copy the input chars
        return String.init(alloc, buf);
    }

    pub fn from_obj(ctx: *anyopaque) *String {
        return Object.cast(String, ctx);
    }
};

test "String.basic" {
    var allocator = std.testing.allocator;
    var string = try String.from_slice(allocator, "Hello, World!");
    defer string.deinit(allocator);

    var string_obj = string.object();
    try std.testing.expect(string.len() == 13);
    try std.testing.expect(string_obj.ty == .String);

    var str_recast = String.from_obj(string_obj.ptr);
    try std.testing.expect(str_recast.len() == 13);
}
