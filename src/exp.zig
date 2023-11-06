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

const Object = packed struct { id: u8, id2: u8 };
const Child = packed struct { obj: *Object, age: u8 };

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

test "inheritance" {
    const allocator = std.testing.allocator;
    _ = allocator;
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
