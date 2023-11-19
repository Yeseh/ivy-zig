const std = @import("std");
const types = @import("types.zig");
const common = @import("common.zig");

const IvyType = types.IvyType;
const Object = types.Object;
const String = types.String;

/// Global linked list of all objects
/// Primitive garbage collector
pub const OM = ObjectManager{};

pub const ObjectManager = struct {
    objects: ?*Object = null,

    pub fn register(self: ObjectManager, obj: *Object) void {
        std.debug.print("Registering object\n", .{});
        obj.next = self.objects;
        @constCast(&self).objects = obj;
    }

    /// Frees all the objects in the object manager
    /// Should only be called from the VM
    pub fn free(self: *ObjectManager, alloc: std.mem.Allocator) void {
        if (common.DEBUG_PRINT_GC) {
            std.debug.print("\n=== GARBAGE COLLECTION ===\n", .{});
            var count: u32 = 0;
            var obj = self.objects;
            while (obj != null) {
                count += 1;
                obj = obj.?.next;
            }
            std.debug.print("GC: {d} objects before collection\n", .{count});
        }
        var obj = self.objects;
        var count: u32 = 0;
        while (obj != null) {
            var next = obj.?.next;
            switch (obj.?.ty) {
                .String => {
                    var str = obj.?.as(String);
                    if (common.DEBUG_PRINT_GC) {
                        std.debug.print("GC: String '{s}' {any}\n", .{ str.asSlice(), str });
                    }
                    str.deinit(alloc);
                    count += 1;
                },
            }
            obj = next;
        }

        if (common.DEBUG_PRINT_GC) {
            std.debug.print("GC: {d} objects freed\n\n", .{count});
        }
    }
};
