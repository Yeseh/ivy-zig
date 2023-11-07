// Language experiments

const std = @import("std");

const State = enum {
    Meh,
    Alive,
    Dead,
};

fn Person(comptime StateType: type) type {
    return packed struct { ty: StateType, state: State };
}

pub const DeadPerson = struct {
    pub fn resurrect(self: *Person(DeadPerson)) *Person(AlivePerson) {
        var person: *Person = @ptrCast(self);
        person.state = false;
        return person;
    }
};

pub const AlivePerson = struct {
    pub fn kill(self: *Person(AlivePerson)) *Person(DeadPerson) {
        var person: *Person = @ptrCast(self);
        person.state = true;
        return person;
    }
};

// Structs should be 'extern' to guarantee C ABI compatible layout
const Object = extern struct { id: u8 };
const Child = extern struct { obj: Object, age: u8 };
const GrandChild = extern struct { child: Child, class: u8 };

test "inheritance" {
    const allocator = std.testing.allocator;

    var child = try allocator.create(Child);
    defer allocator.destroy(child);
    child.obj = Object{
        .id = 3,
    };
    child.age = 10;

    var gc = try allocator.create(GrandChild);
    gc.child.age = 2;
    gc.child.obj.id = 4;
    gc.class = 2;
    defer allocator.destroy(gc);

    var child_as_obj: *Object = @ptrCast(child);
    try std.testing.expect(child_as_obj.id == 3);

    var obj_as_child: *Child = @ptrCast(child_as_obj);
    try std.testing.expect(obj_as_child.age == 10);

    var gc_as_child: *Child = @ptrCast(gc);
    var gc_as_obj: *Object = @ptrCast(gc);
    try std.testing.expect(gc_as_child.age == 2);
    try std.testing.expect(gc_as_obj.id == 4);

    var child_as_gc: *GrandChild = @ptrCast(gc_as_child);
    var obj_as_gc: *GrandChild = @ptrCast(gc_as_obj);
    try std.testing.expect(child_as_gc.class == 2);
    try std.testing.expect(obj_as_gc.class == 2);
}

fn init_child(alloc: std.mem.Allocator, object: *Object) !*Child {
    const child = try alloc.create(Child);
    child.obj = object;
    child.age = 10;
    return child;
}

fn free_child(alloc: std.mem.Allocator, child: *Child) void {
    alloc.destroy(child.obj);
    alloc.destroy(child);
}

test "allocation" {
    const allocator = std.testing.allocator;
    var obj = try allocator.create(Object);
    obj.id = 1;
    obj.id2 = 10;

    const child = try init_child(allocator, obj);
    defer free_child(allocator, child);

    child.obj.id = 2;
    try std.testing.expect(obj.id == 2);
    try std.testing.expect(obj.id2 == 10);

    // Reinterpret casting
    const ptr = @TypeOf(child);
    const ptr_info = @typeInfo(ptr);

    try std.testing.expect(ptr_info == .Pointer);
    try std.testing.expect(ptr_info.Pointer.size == .One);

    const child_obj = @as(@TypeOf(obj), @ptrCast(@alignCast(child)));

    std.debug.print("child_obj.id: {}\n", .{child_obj.id});
}

test "strings" {
    const allocator = std.testing.allocator;
    const bla = "bla";
    const buf: [:0]u8 = try allocator.allocSentinel(u8, bla.len, 0);
    @memcpy(buf, bla.ptr);
    defer allocator.free(buf);

    const str: [:0]const u8 = buf.ptr[0..bla.len :0];
    std.debug.print("str: {s}\n", .{str});
    try std.testing.expect(true);
}

test "pointers" {}
