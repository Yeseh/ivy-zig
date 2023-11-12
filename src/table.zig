const std = @import("std");
const types = @import("types.zig");

const IvyType = types.IvyType;
const String = types.String;

pub const Table = @This();
const Self = @This();
// TODO: Revisit load factor and arraylist resizing @
const TABLE_MAX_LOAD = 0.75;

alloc: std.mem.Allocator,
entries: std.ArrayList(Entry),

pub fn init(alloc: std.mem.Allocator) !Self {
    return .{
        .alloc = alloc,
        .entries = std.ArrayList(Entry).init(alloc),
    };
}

pub fn deinit(self: *Self) void {
    self.entries.deinit(self.alloc);
}

pub fn set(self: *Self, key: *String, value: IvyType) bool {
    var keystr: ?*String = key;
    var entry = self.get(keystr);
    var bNew = keystr != null;
    entry.key = key;
    entry.value = value;
    if (bNew) {
        self.entries.append(entry);
    }
}

pub fn get(self: *Self, key: ?*String) *Entry {
    var index = key.?.hash % self.entries.capacity;
    while (true) {
        var entry = &self.entries.items[index];
        if (entry.key == key.? or entry.key == null) {
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

    try std.testing.expect(table.set(str1, val1));
    try std.testing.expect(table.set(str2, val2));
    try std.testing.expect(!(table.set(str2, val2)));

    var got1 = table.get(str1);
    try std.testing.expect(got1 == .num);

    var got2 = table.get(str2);
    try std.testing.expect(got2 == .bool);
}
