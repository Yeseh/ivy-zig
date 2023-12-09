const std = @import("std");
const types = @import("types.zig");
const common = @import("common.zig");

const IvyType = types.IvyType;
const Object = types.Object;
const ObjectType = types.ObjectType;
const String = types.String;
const ObjectManager = @This();

var objects: ?*Object = null;

pub fn mark(obj: *Object) void {
    obj.next = objects;
    objects = obj;
}

pub fn free(alloc: std.mem.Allocator) void {
    if (common.DEBUG_PRINT_GC) {
        std.debug.print("\n===GARBAGE COLLECTION===\n", .{});
    }
    var obj = objects;
    var count: u32 = 0;
    while (obj != null) {
        var next = obj.?.next;
        switch (obj.?.ty) {
            .String => {
                var str = obj.?.as(String);
                if (common.DEBUG_PRINT_GC) {
                    std.debug.print("GC: String '{s}'\n", .{str.asSlice()});
                }
                str.deinit(alloc);
                count += 1;
            },
            // .Function => {
            //     var func = obj.?.as(types.Function);
            //     if (common.DEBUG_PRINT_GC) {
            //         if (func.name != null)
            //             std.debug.print("GC: Function <{s}>\n", .{func.name.?.asSlice()})
            //         else
            //             std.debug.print("GC: Script\n", .{});
            //     }
            //     func.deinit(alloc);
            //     count += 1;
            // },
            else => {},
        }
        obj = next;
    }

    if (common.DEBUG_PRINT_GC) {
        std.debug.print("GC: {d} objects freed\n\n", .{count});
    }
}
