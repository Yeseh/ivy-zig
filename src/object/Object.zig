pub const Object = @This();

const std = @import("std");
// See : https://ziglang.org/documentation/master/std/src/std/mem/Allocator.zig.html#L186
// and : https://ziglang.org/documentation/master/std/src/std/heap/arena_allocator.zig.html#L26
// For example on how to implement an interface like in zig
pub const Type = enum(u8) {
    String,
};

ptr: *anyopaque,
ty: Type,
vtable: *const VTable,

pub const VTable = struct {
    cmp: *const fn (ctx: *anyopaque, b: *Object) bool,
};

pub fn init(alloc: std.mem.Allocator, ty: Type) !*Object {
    var obj = try alloc.create(Object);
    obj.ptr = obj;
    obj.ty = ty;
    return obj;
}

pub fn deinit(self: *Object, alloc: std.mem.Allocator) void {
    alloc.destroy(self);
}
