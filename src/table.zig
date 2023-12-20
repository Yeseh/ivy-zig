const std = @import("std");
const types = @import("types.zig");
const IvyType = types.IvyType;
const String = types.String;

// NOTE: The current implementation passes handles around instead of referring directly to entries by reference.
//       Not sure if that comes at a major performance cost. It's probably fine, and the API is more explicit.
//       The union approach feels very nice to use, but it's not clear if it's worth it.
//       Lets try this more expressive ziggy way first.
// TODO: research chained scatter table with Brent’s variation [3] (from LUA paper)

pub const Table = @This();
const Self = @This();
// How full the table can be before we grow it. As a fraction of the capacity.
const TABLE_MAX_LOAD: f64 = 0.75;
const TableSize = u32;

const Entry = union(Tag) {
    const Bucket = struct { key: ?*String = null, value: IvyType = .nil };
    const Tag = enum {
        tombstone,
        empty,
        full,
    };
    const Handle = union(Tag) {
        tombstone: TableSize,
        empty: TableSize,
        full: TableSize,
    };

    tombstone,
    empty: Bucket,
    full: Bucket,

    pub fn empty() Entry {
        return Entry{ .empty = .{ .key = null, .value = .nil } };
    }

    pub fn bucket(b: Entry.Bucket) Entry {
        return Entry{ .full = b };
    }

    pub fn tombstone() Entry {
        return Entry{ .tombstone = undefined };
    }
};

count: TableSize,
capacity: TableSize,
entries: [*]Entry,
alloc: std.mem.Allocator,

pub fn init(alloc: std.mem.Allocator, capacity: TableSize) !Self {
    var entries = try alloc.alloc(Entry, capacity);
    for (0..capacity) |i| {
        entries[i] = Entry.empty();
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
        if (entry.* != .full) continue;
        _ = try self.set(entry.full.key.?, entry.full.value);
    }
}

pub fn set(self: *Self, key: *String, value: IvyType) !bool {
    const maxCapacity = @as(f64, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD;
    const addCount = @as(f64, @floatFromInt(self.count + 1));
    if (addCount > maxCapacity) {
        const newCapacity = self.growCapacity();
        try self.adjustCapacity(newCapacity);
    }
    const handle = self.find(key);

    switch (handle) {
        // Key already exists
        .full => {
            self.entries[handle.full].full.value = value;
            return false;
        },
        // Recycle a tombstone
        .tombstone => {
            const bucket = Entry.Bucket{
                .key = key,
                .value = value,
            };
            self.entries[handle.tombstone] = Entry.bucket(bucket);
            return true;
        },
        // Use an empty bucket
        .empty => {
            const bucket = Entry.Bucket{
                .key = key,
                .value = value,
            };
            self.entries[handle.empty] = Entry.bucket(bucket);
            self.count += 1;
            return true;
        },
    }
}

pub fn get(self: *Self, key: *String) ?IvyType {
    if (self.count == 0) return null;

    const handle = self.find(key);
    if (handle == .full) {
        return self.entries[handle.full].full.value;
    }

    return null;
}

pub fn delete(self: *Self, key: *String) bool {
    if (self.count == 0) return false;

    const handle = self.find(key);
    if (handle == .full) {
        self.entries[handle.full] = Entry.tombstone();
        return true;
    }

    return false;
}

pub fn findString(self: *Self, chars: []const u8, hash: u32) ?*String {
    if (self.count == 0) return null;
    var index = hash % self.capacity;
    while (true) {
        var entry = &self.entries[index];
        switch (entry.*) {
            .full => {
                var key = entry.full.key.?;
                const leneql = key._len == chars.len;
                const memeql = std.mem.eql(u8, chars, key.asSlice());
                const hasheql = key.hash == hash;

                if (leneql and hasheql and memeql)
                    return entry.full.value.object_as(String);
            },
            // string is not in the table
            .empty => return null,
            // Skip tombstones
            .tombstone => {},
        }

        index = (index + 1) % self.capacity;
    }
}

fn growCapacity(self: *Self) TableSize {
    const newCapacity = self.capacity * 2;
    if (newCapacity < 8) return 8;
    return newCapacity;
}

fn find(self: *Self, key: *String) Entry.Handle {
    var index = key.hash % self.capacity;
    var tombstone: ?Entry.Handle = null;

    while (true) {
        const entry = &self.entries[index];
        switch (entry.*) {
            // There is a bucket entry here
            .full => {
                if (entry.full.key == key) return .{ .full = index };
            },
            // We found an empty bucket
            // If we previously found a tombstone, we'll return that instead so it can be recycled
            .empty => {
                return tombstone orelse .{ .empty = index };
            },
            // We found a tombstone entry, set it and keep going
            .tombstone => {
                if (tombstone == null) tombstone = .{ .tombstone = index };
            },
        }

        index = (index + 1) % self.capacity;
    }
}

fn allocatedSlice(self: *Self) []Entry {
    return self.entries[0..self.capacity];
}

fn adjustCapacity(self: *Self, capacity: u32) !void {
    var newTable = try Table.init(self.alloc, capacity);
    for (self.allocatedSlice()) |entry| {
        // Don't copy empty or tombstone entries
        if (entry != .full) continue;

        const handle = newTable.find(entry.full.key.?);
        switch (handle) {
            .tombstone => {},
            .full => {
                newTable.entries[handle.full] = Entry.bucket(entry.full);
                newTable.count += 1;
            },
            .empty => {
                newTable.entries[handle.empty] = Entry.bucket(entry.full);
                newTable.count += 1;
            },
        }
    }

    self.deinit();
    self.* = newTable;
}

test "Table.basic" {
    const a = std.testing.allocator;
    var table = try Table.init(a, 2);
    defer table.deinit();

    var str1 = try String.copy(a, "foo");
    var str2 = try String.copy(a, "bar");
    var str3 = try String.copy(a, "baz");
    var str4 = try String.copy(a, "zab");

    defer str1.deinit(a);
    defer str2.deinit(a);
    defer str3.deinit(a);
    defer str4.deinit(a);

    const val1 = IvyType.number(10);
    const val2 = IvyType.boolean(true);
    const val3 = IvyType.number(20);

    try std.testing.expect(try table.set(str1, val1));
    // try std.testing.expectEqual(table.capacity, 2);
    try std.testing.expectEqual(table.count, 1);

    try std.testing.expect(try table.set(str2, val2));
    try std.testing.expect(!(try table.set(str2, val2)));
    try std.testing.expectEqual(table.count, 2);
    try std.testing.expectEqual(table.capacity, 8);

    const got1 = table.get(str1);
    try std.testing.expect(got1.?.num == 10);

    const got2 = table.get(str2);
    try std.testing.expect(got2.?.bool == true);

    try std.testing.expect(try table.set(str3, val3));
    try std.testing.expect(table.capacity > 2);

    const got3 = table.get(str3);
    try std.testing.expect(got3.?.num == 20);
    try std.testing.expectEqual(table.count, 3);

    const got4 = table.get(str4);
    try std.testing.expect(got4 == null);

    try std.testing.expect(table.delete(str1));
    try std.testing.expect(!table.delete(str4));

    var str5 = try String.copy(a, "foo");
    const val5 = IvyType.number(30);
    defer str5.deinit(a);

    try std.testing.expect(try table.set(str5, val5));
}

test "Table.addAll" {
    const a = std.testing.allocator;
    var tableA = try Table.init(a, 2);
    defer tableA.deinit();
    var tableB = try Table.init(a, 2);
    defer tableB.deinit();

    var k1 = try String.copy(a, "foo");
    var k2 = try String.copy(a, "bar");
    var k3 = try String.copy(a, "baz");
    var k4 = try String.copy(a, "zab");

    defer k1.deinit(a);
    defer k2.deinit(a);
    defer k3.deinit(a);
    defer k4.deinit(a);

    const v1 = IvyType.number(1);
    const v2 = IvyType.number(2);
    const v3 = IvyType.number(3);
    const v4 = IvyType.number(4);

    try std.testing.expect(try tableA.set(k1, v1));
    try std.testing.expect(try tableA.set(k2, v2));
    try std.testing.expect(try tableB.set(k3, v3));
    try std.testing.expect(try tableB.set(k4, v4));

    try tableA.addAll(&tableB);
    try std.testing.expectEqual(tableA.count, 4);

    const got1 = tableA.get(k1).?;
    const got2 = tableA.get(k2).?;
    const got3 = tableA.get(k3).?;
    const got4 = tableA.get(k4).?;

    try std.testing.expect(got1.num == 1);
    try std.testing.expect(got2.num == 2);
    try std.testing.expect(got3.num == 3);
    try std.testing.expect(got4.num == 4);
}
