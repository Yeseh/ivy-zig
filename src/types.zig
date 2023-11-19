const std = @import("std");
const common = @import("common.zig");
const ArrayList = std.ArrayList;
const RuntimeErrror = common.RuntimeError;
const Table = common.Table;
const OM = @import("garbage.zig").OM;

pub const ObjectType = enum(u8) {
    String,
};

/// This is a generic object type. It is used to represent all objects in Ivy.
/// It is based on struct inheritance in accordance with the book.
/// There might be a more zig-native way to do this, but we'll look at that when we are done.
pub const Object = extern struct {
    const Self = @This();

    ty: ObjectType,
    next: ?*Self,

    pub fn as(self: *Self, comptime T: type) *T {
        return @ptrCast(@alignCast(self));
    }
};

/// Raw string type. This is a wrapper around a null-terminated slice of bytes.
/// This is very C-like, in accordance with the book.
/// Can investigate the use of an ArrayList in a more ziggy implementation after we are done.
/// Size: 25 + n*1 bytes
pub const String = extern struct {
    const Self = @This();

    /// The object header. This should not be used directly.
    _obj: Object,
    /// Internal length of the string. This should not be used directly, use `len()` instead.
    _len: usize,
    /// Internal capacity of the string. This should not be used directly, use `capacity()` instead.
    _capacity: usize,
    hash: u32,
    /// Heap allocated buffer of characters, should not be accessed directly.
    _buf: [*]u8,

    // Not sure if the string should keep its own allocator
    // Makes it easier to do operations on the string
    // NOTE: extern structs can't have them anyways. Puh.
    // allocator: std.mem.Allocator,

    /// Creates a new string Object from a slice.
    /// Takes ownership of the slice passed
    pub fn createInterned(alloc: std.mem.Allocator, chars: []const u8, table: *Table) !*Self {
        var hsh = String.hash(chars);
        var interned = table.findString(chars, hsh);
        if (interned != null) {
            alloc.free(std.mem.sliceTo(chars, 0));
            return interned.?;
        }
        var str = try String.create(alloc, chars);
        str.hash = hsh;
        _ = try table.set(str, IvyType.nil());

        return str;
    }

    /// Creates a new string Object from a slice.
    /// Copies the slice passed in, and takes ownership of the copy.
    pub fn copyInterned(alloc: std.mem.Allocator, chars: []const u8, table: *Table) !*Self {
        var hsh = String.hash(chars);
        var interned = table.findString(chars, hsh);
        if (interned != null) {
            return interned.?;
        }
        var str = try String.copy(alloc, chars);
        str.hash = hsh;
        _ = try table.set(str, IvyType.nil());

        return str;
    }

    /// Initializes a string by taking ownership of the slice passed in.
    pub fn create(alloc: std.mem.Allocator, chars: []const u8) !*Self {
        var str = try alloc.create(Self);
        str._obj.ty = ObjectType.String;
        str._len = chars.len;
        str._buf = @constCast(chars.ptr);
        str._capacity = chars.len;
        return str;
    }

    pub fn copy(alloc: std.mem.Allocator, slice: []const u8) !*Self {
        var buf: [:0]u8 = try alloc.allocSentinel(u8, slice.len, 0);
        @memcpy(buf, slice.ptr);

        var str = try alloc.create(Self);
        str._obj.ty = ObjectType.String;
        str._buf = buf.ptr;
        str._capacity = buf.len + 1;
        str._len = slice.len;
        return str;
    }

    pub fn hash(key: []const u8) u32 {
        @setRuntimeSafety(false);
        var hsh: u32 = 2166136261;
        const prime: u32 = 16777619;
        for (key) |c| {
            hsh ^= c;
            hsh *= prime;
        }
        return @as(u32, @intCast(hsh));
    }

    fn growCapacityTo(self: *Self, minimum: usize) usize {
        var new = self._capacity;
        while (true) {
            new +|= new / 2 + 8;
            if (new >= minimum)
                return new;
        }
    }

    fn allocatedSlice(self: *Self) []u8 {
        return self._buf[0..self._capacity];
    }

    /// Resizes the internal buffer.
    /// Invalidates pointers to the strings characters
    fn resize(self: *Self, alloc: std.mem.Allocator, new: usize) !void {
        var newCapacity = self.growCapacityTo(new);
        // TODO: realloc
        var newBuf: []u8 = try alloc.alloc(u8, newCapacity);
        @memcpy(newBuf, self._buf);

        alloc.free(self.allocatedSlice());
        self._buf = newBuf.ptr;
        self._capacity = newCapacity;
    }

    /// Appends a slice to the string with the assumption that there is enough capacity.
    /// Does not attempt to resize the string if capacity is not met
    ///
    /// Safety: Ensure capacity is sufficient
    pub fn appendSliceRaw(self: *Self, slice: [:0]const u8) !void {
        std.debug.assert(self._capacity >= self._len + slice.len);
        @memcpy(self._buf[0..self._len], slice._buf[0..self._len]);
        @memcpy(self._buf, slice);
        self._len += slice.len;
    }

    /// Appends a slice to the string
    /// Invalidates pointers to the strings characters if resizing is needed
    pub fn appendSlice(self: *Self, alloc: std.mem.Allocator, slice: [:0]const u8) !void {
        const needed = self._len + slice.len;
        if (self._capacity < needed) {
            try self.resize(alloc, needed);
        }
        try self.appendSliceRaw(slice);
    }

    /// Returns a new slice that is a view into the string
    pub fn asSlice(self: *Self) []const u8 {
        var slice: []const u8 = self._buf[0..self._len]; //std.mem.sliceTo(self._buf, 0);
        return slice;
    }

    /// Deinitializes a string. Should be called with the same allocator that was used to create it.
    pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        var slice = self.allocatedSlice();
        alloc.free(slice);
        self._len = 0;
        self._capacity = 0;
        alloc.destroy(self);
    }

    /// Returns the character at the given index
    pub fn at(self: *Self, index: usize) !u8 {
        if (index >= self._len) {
            return common.RuntimeError.IndexOutOfBounds;
        }
        return self._buf[index];
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

// 1 byte tag,
// 8 byte data
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
        var objptr: *Object = @ptrCast(@alignCast(str));
        OM.register(objptr);
        return Self{ .object = objptr };
    }

    // TODO: Is there a zig comptime magic way to infer T from the pointer type?
    pub fn obj(comptime T: type, ptr: T) IvyType {
        var o = IvyType{ .object = @ptrCast(@alignCast(ptr)) };
        OM.register(o.object);
        return o;
    }

    pub fn print(self: *const Self) void {
        switch (self.*) {
            .bool => std.debug.print("{}", .{self.bool}),
            .num => std.debug.print("{}", .{self.num}),
            .nil => std.debug.print("nil", .{}),
            .object => {
                switch (self.object.ty) {
                    .String => std.debug.print("\"{s}\"", .{self.object_as(String).asSlice()}),
                }
            },
        }
    }

    pub inline fn as_bool(self: @This()) bool {
        return switch (self) {
            .bool => self.bool,
            .num => true,
            .object => true,
            .nil => false,
        };
    }

    pub fn clone(self: *const Self, alloc: std.mem.Allocator) !Self {
        return switch (self.*) {
            .num => IvyType.number(self.num),
            .bool => IvyType.boolean(self.bool),
            .nil => IvyType.nil(),
            .object => switch (self.object.ty) {
                .String => blk: {
                    var str = self.object_as(String);
                    var copy = try String.copy(alloc, std.mem.sliceTo(str.allocatedSlice(), 0));
                    break :blk IvyType.string(copy);
                },
            },
        };
    }

    pub inline fn object_as(self: *const Self, comptime DestType: type) *DestType {
        std.debug.assert(@constCast(self).is_obj());
        return @ptrCast(@alignCast(self.object));
    }

    pub inline fn is_obj(self: *const Self) bool {
        return self.* == .object;
    }

    pub inline fn is_obj_type(self: *const Self, ot: ObjectType) bool {
        return self.* == .object and self.object.ty == ot;
    }

    pub inline fn is_string(self: *const Self) bool {
        return self.is_obj_type(ObjectType.String);
    }

    pub inline fn is_bool(self: *const Self) bool {
        return self.* == .bool;
    }

    pub inline fn is_num(self: *const Self) bool {
        return self.* == .num;
    }

    pub inline fn obj_type(self: *const Self) ?ObjectType {
        switch (self) {
            .object => return self.object.ty,
            else => null,
        }
    }
};

pub fn eql(a: IvyType, b: IvyType) bool {
    return switch (a) {
        .bool => a.bool == b.bool,
        .num => a.num == b.num,
        .nil => b == .nil,
        .object => {
            if (a.object.ty != b.object.ty) {
                return false;
            }
            switch (a.object.ty) {
                .String => {
                    return a.object_as(String) == b.object_as(String);
                },
            }
        },
    };
}

// test "String.resizing" {
//     const a = std.testing.allocator;
//     var string = try String.init(a);
//     defer string.deinit(a);

//     try std.testing.expectEqual(string._capacity, 8);
//     try std.testing.expectEqual(string.len(), 0);
//     try string.appendSlice(a, "Hello, World!");
//     try std.testing.expectEqual(string.len(), 13);
//     try string.appendSlice(a, "Hello, World!");
//     try std.testing.expectEqual(string.len(), 26);

//     var nstring = try String.fromSlice(a, "FUBAR");
//     defer nstring.deinit(a);

//     try string.appendSlice(a, nstring.asSlice());
//     try std.testing.expectEqual(string.len(), 31);
// }

// TODO: Test for string interning
test "String" {
    const allocator = std.testing.allocator;
    var slice = "Hello, World!";
    {
        var string = try String.create(allocator, slice);
        //defer string.deinit(allocator);
        try std.testing.expectEqual(string.len(), 8);
        try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(8));
    }
    {
        var string = try String.copy(allocator, slice);
        defer string.deinit(allocator);
        try std.testing.expectEqual(string.len(), 13);
        try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(13));
        try std.testing.expect(std.mem.eql(u8, slice, string.asSlice()));
    }
    // {
    //     var string = try String.fromSliceSentinel(allocator, slice);
    //     defer string.deinit(allocator);
    //     try std.testing.expectEqual(string.len(), 13);
    //     try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(13));
    //     try std.testing.expect(std.mem.eql(u8, slice, string.asSlice()));
    // }
    // {
    //     var string = try String.empty(allocator);
    //     defer string.deinit(allocator);
    //     try std.testing.expectEqual(string.len(), 0);
    //     try std.testing.expectError(RuntimeErrror.IndexOutOfBounds, string.at(0));
    // }
}
