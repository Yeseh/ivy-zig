const std = @import("std");
pub const Object = @This();
const Self = @This();

// See : https://ziglang.org/documentation/master/std/src/std/mem/Allocator.zig.html#L186
// and : https://ziglang.org/documentation/master/std/src/std/heap/arena_allocator.zig.html#L26
// For example on how to implement an interface like in zig

pub fn cast(comptime T: type, objptr: *anyopaque) *T {
    return @ptrCast(@alignCast(objptr));
}

pub const ObjectType = enum(u8) {
    /// A heap allocated string
    String,
};

/// Interface for a heap allocated object
pub const VTable = struct {
    /// Prints the type of the object
    /// This is used for debugging
    print: *const fn (self: *anyopaque) void,
};

ty: ObjectType,
ptr: *anyopaque,
vtable: *const VTable,

pub fn print(self: *Object) void {
    std.debug.print("({any})", .{self.ptr});
}

pub fn _init(alloc: std.mem.Allocator, ty: ObjectType) !*Object {
    var obj = try alloc.create(Object);
    obj.ptr = obj;
    obj.ty = ty;
    return obj;
}

pub fn deinit(self: *Object, alloc: std.mem.Allocator) void {
    alloc.destroy(self);
}

test "Object" {
    const allocator = std.testing.allocator;
    const obj = try _init(allocator, ObjectType.String);
    deinit(obj, allocator);
}
