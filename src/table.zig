const std = @import("std");
const types = @import("types.zig");

const IvyType = types.IvyType;
const String = types.String;

pub const Table = @This();
const Self = @This();
// TODO: Revisit load factor and arraylist resizing @
const TABLE_MAX_LOAD = 0.75;

const EntryHandle = u32;

alloc: std.mem.Allocator,
entries: std.ArrayList(Entry),

pub fn init(alloc: std.mem.Allocator) !Self {
    return .{
        .alloc = alloc,
        .entries = try std.ArrayList(Entry).initCapacity(alloc, 8),
    };
}

pub fn deinit(self: *Self) void {
    self.entries.deinit();
}

pub fn set(self: *Self, key: *String, value: IvyType) !bool {
    var existing = self.get(key);
    var bNew = existing == null;
    if (bNew) {
        var entry = .{
            .key = key,
            .value = value,
        };
        try self.entries.append(entry);
    }
    return bNew;
}

pub fn get(self: *Self, key: *String) ?*Entry {
    var index = key.hash % self.entries.capacity;
    std.debug.print("len:{} i:{}\n", .{ self.entries.items.len, index });
    if (self.entries.capacity <= index) {
        return null;
    }
    while (true) {
        var entry = &self.entries.allocatedSlice()[index];
        if (entry.key == key or entry.key == null) {
            return entry;
        }
        index = (index + 1) % self.entries.capacity;
    }
}

const Entry = struct { key: ?*String, value: IvyType };

test "Table" {
    const a = std.testing.allocator;
    var table = try Table.init(a);
    defer table.deinit();

    var str1 = try String.fromSlice(a, "foo");
    defer str1.deinit(a);
    var val1 = IvyType.number(10);

    var str2 = try String.fromSlice(a, "bar");
    defer str2.deinit(a);
    var val2 = IvyType.boolean(false);

    try std.testing.expect(try table.set(str1, val1));
    try std.testing.expect(try table.set(str2, val2));

    try std.testing.expect(!(try table.set(str2, val2)));

    var got1 = table.get(str1);
    try std.testing.expect(got1.?.value == .num);

    var got2 = table.get(str2);
    try std.testing.expect(got2.?.value == .bool);
}
