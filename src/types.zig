const std = @import("std");
const common = @import("common.zig");
const ArrayList = std.ArrayList;
const RuntimeErrror = common.RuntimeError;

pub const ObjectType = enum(u8) {
    String,
};

/// This is a generic object type. It is used to represent all objects in Ivy.
/// It is based on struct inheritance in accordance with the book.
/// There might be a more zig-native way to do this, but we'll look at that when we are done.
pub const Object = extern struct {
    const Self = @This();

    ty: ObjectType,
};

/// Raw string type. This is a wrapper around a null-terminated slice of bytes.
/// This is very C-like, in accordance with the book.
/// Can investigate the use of an ArrayList in a more ziggy implementation after we are done.
pub const String = extern struct {
    const Self = @This();

    /// The object header. This should not be used directly.
    _obj: Object,
    /// Internal length of the string. This should not be used directly, use `len` instead.
    _len: usize,
    _capacity: usize,
    /// Heap allocated buffer of characters, should not be accessed directly.
    _chars: [*:0]u8,

    // Not sure if the string should keep its own allocator
    // Makes it easier to do operations on the string
    // NOTE: extern structs can't have them anyways. Puh.
    // allocator: std.mem.Allocator,

    /// Initializes a string with capacity 8.
    pub fn init(alloc: std.mem.Allocator) !*Self {
        var buf: [:0]u8 = try alloc.allocSentinel(u8, 8, 0);
        // @memset(buf, 0);

        var str = try alloc.create(Self);
        str.ty = ObjectType.String;
        str._len = buf.len;
        str._len = buf.len;
        str._chars = buf.ptr;
        return str;
    }

    /// Initializes an empty string
    pub fn empty(alloc: std.mem.Allocator) !*Self {
        var str = try alloc.create(Self);
        var buf: [:0]u8 = try alloc.allocSentinel(u8, 0, 0);
        str._obj.ty = ObjectType.String;
        str._chars = buf;
        str._len = buf.len;
        return str;
    }

    /// Returns a new slice that is a view into the string
    pub fn asSlice(self: *Self) [:0]const u8 {
        var slice: [:0]const u8 = std.mem.sliceTo(self._chars, 0);
        return slice;
    }

    /// Copies a slice to the heap and creates a string from it
    pub fn fromSlice(alloc: std.mem.Allocator, slice: []const u8) !*Self {
        var buf: [:0]u8 = try alloc.allocSentinel(u8, slice.len, 0);
        @memcpy(buf, slice.ptr);

        var str = try alloc.create(Self);
        str._obj.ty = ObjectType.String;
        str._chars = buf.ptr;
        str._len = slice.len;
        return str;
    }

    /// Copies a terminated slice to the heap and creates a string from it
    pub fn fromSliceSentinel(alloc: std.mem.Allocator, slice: [:0]const u8) !*Self {
        const buf = try alloc.allocSentinel(u8, slice.len, 0);
        @memcpy(buf, slice.ptr);

        var str = try alloc.create(Self);
        str._obj.ty = ObjectType.String;
        str._chars = buf.ptr;
        str._len = slice.len;
        return str;
    }

    /// Deinitializes a string. Should be called with the same allocator that was used to create it.
    pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        var slice = std.mem.sliceTo(self._chars, 0);
        alloc.free(slice);
        alloc.destroy(self);
    }

    /// Returns the character at the given index
    pub fn at(self: *Self, index: usize) !u8 {
        if (index >= self._len) {
            return common.RuntimeError.IndexOutOfBounds;
        }
        return self._chars[index];
    }

    /// Returns the index of the given character
    pub fn indexOf(self: *Self, c: u8) ?usize {
        var slice = self.asSlice();
        return std.mem.indexOf(u8, slice, c);
    }

    pub fn len(self: *Self) usize {
        return self._len;
    }
};

pub const IvyType = union(enum) {
    pub const Self = @This();

    nil,
    bool: bool,
    num: f64,
    // TODO: Change to *SpecificObjectType? Then we can switch on IvyType neatly;
    object: *Object,

    pub fn boolean(b: bool) Self {
        return Self{ .bool = b };
    }

    pub fn nil() Self {
        return Self{ .nil = undefined };
    }

    pub fn number(n: f64) Self {
        return Self{ .num = n };
    }

    pub fn string(str: *String) Self {
        return Self{ .object = @ptrCast(@alignCast(str)) };
    }

    // TODO: Is there a zig comptime magic way to infer T from the pointer type?
    pub fn obj(comptime T: type, ptr: T) IvyType {
        return IvyType{ .object = @ptrCast(@alignCast(ptr)) };
    }

    pub fn print(self: Self) void {
        switch (self) {
            .bool => std.debug.print("{}", .{self.bool}),
            .num => std.debug.print("{}", .{self.num}),
            .nil => std.debug.print("nil", .{}),
            .object => {
                switch (self.object.ty) {
                    .String => std.debug.print("'{s}'", .{@constCast(&self).as_obj_type(String).asSlice()}),
                }
            },
        }
    }

    pub fn to_bool(self: @This()) bool {
        return switch (self) {
            .bool => self.bool,
            .num => true,
            .object => true,
            .nil => false,
        };
    }

    pub fn cmp(self: @This(), other: IvyType) bool {
        return switch (self) {
            .bool => switch (other) {
                .bool => self.bool == other.bool,
                _ => false,
            },
            .num => switch (other) {
                .number => self.num == other.number,
                _ => false,
            },
            .nil => switch (other) {
                .nil => other == IvyType.nil,
                _ => false,
            },
            .object => switch (other) {
                .object => self.cmp(other),
                _ => false,
            },
        };
    }

    pub fn as_obj_type(self: *Self, comptime DestType: type) *DestType {
        return @ptrCast(@alignCast(self.object));
    }

    pub fn is_obj(self: *Self) bool {
        return self == .object;
    }

    pub fn is_obj_type(self: *Self, ot: ObjectType) bool {
        return self == .object and self.object.ty == ot;
    }

    pub fn is_string(self: *Self) bool {
        return self.is_obj_type(ObjectType.String);
    }

    pub fn obj_type(self: *Self) ?ObjectType {
        switch (self) {
            .object => return self.object.ty,
            else => null,
        }
    }
};

test "String" {
    const allocator = std.testing.allocator;
    var slice = "Hello, World!";
    {
        var string = try String.init(allocator);
        defer string.deinit(allocator);
        try std.testing.expectEqual(string.len(), 8);
        try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(8));
    }
    {
        var string = try String.fromSlice(allocator, slice);
        defer string.deinit(allocator);
        try std.testing.expectEqual(string.len(), 13);
        try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(13));
        try std.testing.expect(std.mem.eql(u8, slice, string.asSlice()));
    }
    {
        var string = try String.fromSliceSentinel(allocator, slice);
        defer string.deinit(allocator);
        try std.testing.expectEqual(string.len(), 13);
        try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(13));
        try std.testing.expect(std.mem.eql(u8, slice, string.asSlice()));
    }
    {
        var string = try String.empty(allocator);
        defer string.deinit(allocator);
        try std.testing.expectEqual(string.len(), 0);
        try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(0));
    }
}
