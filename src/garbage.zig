const std = @import("std");
const types = @import("types.zig");

const IvyType = types.IvyType;
const Object = types.Object;
const String = types.String;

/// Global linked list of all objects
/// Primitive garbage collector
pub const OM = ObjectManager{};

pub const ObjectManager = struct {
    objects: ?*Object = null,

    pub fn register(self: *ObjectManager, obj: *Object) void {
        obj.next = self.objects;
        self.objects = obj;
    }

    /// Frees all the objects in the object manager
    /// Should only be called from the VM
    pub fn free(self: *ObjectManager, alloc: std.mem.Allocator) void {
        var obj = self.objects;
        while (obj != null) {
            var next = obj.?.next;
            switch (obj.?.ty) {
                .String => obj.?.as(String).deinit(alloc),
            }
            obj = next;
        }
    }
};
