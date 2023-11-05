pub const String = @This();

const std = @import("std");
const Object = @import("Object.zig");
const ObjectType = Object.Type;

obj: *const Object,
chars: std.ArrayList(u8),

pub fn from_slice(alloc: std.mem.Allocator, slice: []const u8) !*String {
    // Ensure we have a null-terminated slice
    var buf = try alloc.allocSentinel(u8, slice.len, 0);
    @memcpy(buf, slice.ptr);
    std.debug.print("copy_string: {s}\n", .{buf});
    // We can use fromOwnedSliceSentinel here because we used the same allocator to copy the input chars
    var chrs = std.ArrayList(u8).fromOwnedSliceSentinel(alloc, 0, buf);
    return String.init(alloc, chrs);
}

/// Creates a new String on the Heap
pub fn init(alloc: std.mem.Allocator, chars: std.ArrayList(u8)) !*String {
    var obptr = try Object.init(alloc, ObjectType.String);
    var string = try alloc.create(String);
    string.obj = obptr;
    string.chars = chars;
    return string;
}

/// Clears the internal character array and destroys the heap-allocated string.
/// Should be called with the same allocator that was used to initialize the string
pub fn deinit(self: *String, alloc: std.mem.Allocator) void {
    self.chars.deinit();
    alloc.destroy(self.obj);
    alloc.destroy(self);
}

pub fn cmp(ctx: *anyopaque, other: *Object) bool {
    const self = @ptrCast(*String, @alignCast(@alignOf(*String), ctx));
    if (self.obj.type != ObjectType.String or other.type != ObjectType.String) {
        return false;
    }
    const other_str: *String = @ptrCast(*String, other);
    return std.mem.eql(u8, self.chars.items, other_str.chars.items);
}

pub fn as_obj(self: *String) *Object {
    return @ptrCast(*Object, @alignCast(@alignOf(*Object), self));
}

pub fn from_obj(obj: *Object) *String {
    return @ptrCast(*String, @alignCast(@alignOf(*String), obj));
}

pub fn len(self: *String) usize {
    return self.chars.len;
}
