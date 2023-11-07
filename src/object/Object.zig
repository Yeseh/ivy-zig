/// 'Object' is not really anything.
/// It is not heap allocated (yet) it is just an interface that is implemented by all object types so they can be
/// passed around interchangably.
///
/// NOTE to self: we can NEVER cast a pointer of type *Object to a more specific type because they share nothing.
/// Might need to try 'extern' structs to be compatible with the C ABI.
///
/// It still remains to be seen if this is at all useful and leads to something that looks like inheritance in the target language.
///
/// It is also possible that this is a bad idea and that it is better to just use
/// unions and enums to represent objects. We will see when we get to the end of the book :)
pub const Object = @This();
const std = @import("std");
const Self = @This();

// See : https://ziglang.org/documentation/master/std/src/std/mem/Allocator.zig.html#L186
// and : https://ziglang.org/documentation/master/std/src/std/heap/arena_allocator.zig.html#L26
// For example on how to implement an interface like in zig

/// The compiler type of an object
/// This has no relation to the runtime type of an object
pub const ObjectType = enum(u8) {
    /// A heap allocated string
    String,
};

/// Interface for a generic object
pub const VTable = struct {
    /// Prints the type of the object
    /// This is used for debugging
    print: *const fn (self: *anyopaque) void,
    // can_cast: *const fn (self: *anyopaque, objptr: *Object) bool,
};

/// The compiler type of an object
ty: ObjectType,
/// The type-erased pointer to the object
ptr: *anyopaque,
/// Pointer to a vtable containing the interface functions
vtable: *const VTable,

/// Casts an object pointer to a more specific type.
///
/// Safety: The caller must ensure that the object pointer is of the specified type
pub fn cast(comptime T: type, obj: *Object) *T {
    return @ptrCast(@alignCast(obj.ptr));
}

/// Upcasts an object to a more specific type
/// Tests if the object is of the specified type
pub fn safe_cast(comptime DestType: type, comptime SrcType: type, objptr: *SrcType) ?*DestType {
    if (@alignOf(DestType) != @alignOf(SrcType)) {
        return null;
    }

    if (objptr == null) {
        return null;
    }
    return @ptrCast(@alignCast(objptr));
}
