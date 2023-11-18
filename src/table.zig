const std = @import("std");
const types = @import("types.zig");

const IvyType = types.IvyType;
const String = types.String;

pub const Table = @This();
const Self = @This();
// How full the table can be before we grow it. As a fraction of the capacity.
const TABLE_MAX_LOAD: f64 = 0.75;

const EntryHandle = u32;
const Entry = struct { key: ?*String = null, value: IvyType = .nil };

count: u32,
capacity: u32,
entries: [*]Entry,
alloc: std.mem.Allocator,

pub fn init(alloc: std.mem.Allocator, capacity: u32) !Self {
    var entries = try alloc.alloc(Entry, capacity);
    for (entries) |*entry| {
        entry.key = null;
        entry.value = .nil;
    }
    return .{ .alloc = alloc, .capacity = capacity, .count = 0, .entries = entries.ptr };
}

pub fn deinit(self: *Self) void {
    self.alloc.free(self.allocatedSlice());
    self.capacity = 0;
    self.count = 0;
    self.* = undefined;
}

pub fn addAll(self: *Self, other: *Self) !void {
    for (other.allocatedSlice()) |*entry| {
        if (entry.key == null) continue;
        _ = try self.set(entry.key.?, entry.value);
    }
}

pub fn set(self: *Self, key: *String, value: IvyType) !bool {
    var maxCapacity = @as(f64, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD;
    var addCount = @as(f64, @floatFromInt(self.count + 1));
    if (addCount > maxCapacity) {
        try self.adjustCapacity(self.capacity + 1);
    }
    var entry = self.get(key);
    var isNew = entry.key == null;
    if (isNew) {
        entry.key = key;
        entry.value = value;
        self.count += 1;
    }
    return isNew;
}

// NOTE: Weird API where we return a pointer to an entry in the table even if that entry is empty.
//      This is because we need to return a pointer to the entry so that the caller can mutate it.
//      But do we want that? It's not safe if we reallocate the table and the pointer becomes invalid.
// TODO: Replace with EntryHandle?
pub fn get(self: *Self, key: *String) *Entry {
    var index = key.hash % self.capacity;

    while (true) {
        var entry = &self.entries[index];
        if (entry.key == key or entry.key == null) {
            return entry;
        }
        index = (index + 1) % self.capacity;
    }
}

fn allocatedSlice(self: *Self) []Entry {
    return self.entries[0..self.capacity];
}

fn growCapacity(self: *Self, minimum: u32) u32 {
    var newCapacity = self.capacity;
    while (true) {
        newCapacity +|= newCapacity / 2 + 8;
        if (newCapacity >= minimum)
            return newCapacity;
    }
}

fn adjustCapacity(self: *Self, new: u32) !void {
    var newTable = try Table.init(self.alloc, new);
    for (self.allocatedSlice()) |*entry| {
        if (entry.key == null) continue;
        var dest = newTable.get(entry.key.?);
        dest.key = entry.key;
        dest.value = entry.value;
        if (dest.key != null) {
            newTable.count += 1;
        }
    }

    self.deinit();
    self.* = newTable;
}

test "Table.basic" {
    const a = std.testing.allocator;
    var table = try Table.init(a, 2);
    defer table.deinit();

    var str1 = try String.fromSlice(a, "foo");
    defer str1.deinit(a);
    var val1 = IvyType.number(10);

    var str2 = try String.fromSlice(a, "bar");
    defer str2.deinit(a);
    var val2 = IvyType.boolean(true);

    try std.testing.expect(try table.set(str1, val1));
    // try std.testing.expectEqual(table.capacity, 2);

    try std.testing.expect(try table.set(str2, val2));
    try std.testing.expect(!(try table.set(str2, val2)));
    try std.testing.expect(table.capacity > 2);
    try std.testing.expectEqual(table.count, 2);

    var got1 = table.get(str1);
    try std.testing.expect(got1.value == .num);
    try std.testing.expect(got1.value.num == 10);

    var got2 = table.get(str2);
    try std.testing.expect(got2.value == .bool);
    try std.testing.expect(got2.value.bool == true);

    var str3 = try String.fromSlice(a, "baz");
    defer str3.deinit(a);
    var val3 = IvyType.number(20);

    // Resize happens here
    try std.testing.expect(try table.set(str3, val3));
    var got3 = table.get(str3);
    try std.testing.expect(got3.value == .num);
    try std.testing.expect(got3.value.num == 20);
    try std.testing.expectEqual(table.count, 3);
}

test "Table.addAll" {
    const a = std.testing.allocator;
    var tableA = try Table.init(a, 2);
    defer tableA.deinit();
    var tableB = try Table.init(a, 2);
    defer tableB.deinit();

    var k1 = try String.fromSlice(a, "foo");
    var k2 = try String.fromSlice(a, "bar");
    var k3 = try String.fromSlice(a, "baz");
    var k4 = try String.fromSlice(a, "zab");

    defer k1.deinit(a);
    defer k2.deinit(a);
    defer k3.deinit(a);
    defer k4.deinit(a);

    var v1 = IvyType.number(1);
    var v2 = IvyType.number(2);
    var v3 = IvyType.number(3);
    var v4 = IvyType.number(4);

    try std.testing.expect(try tableA.set(k1, v1));
    try std.testing.expect(try tableA.set(k2, v2));
    try std.testing.expect(try tableB.set(k3, v3));
    try std.testing.expect(try tableB.set(k4, v4));

    try tableA.addAll(&tableB);
    try std.testing.expectEqual(tableA.count, 4);

    var got1 = tableA.get(k1);
    var got2 = tableA.get(k2);
    var got3 = tableA.get(k3);
    var got4 = tableA.get(k4);

    try std.testing.expect(got1.value.num == 1);
    try std.testing.expect(got2.value.num == 2);
    try std.testing.expect(got3.value.num == 3);
    try std.testing.expect(got4.value.num == 4);
}
