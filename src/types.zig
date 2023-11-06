const std = @import("std");
const Object = @import("object/Object.zig");
const String = @import("object/string.zig").String;

const common = @import("common.zig");

const RuntimeError = common.RuntimeError;
const ArrayList = std.ArrayList;

pub const IvyType = union(enum) {
    bool: bool,
    num: f64,
    nil: u1,
    // TODO: Change to *SpecificObjectType? Then we can switch on IvyType neatly;
    object: Object,

    pub fn to_bool(self: @This()) bool {
        return switch (self) {
            .bool => self.bool,
            .num => true,
            .object => true,
            .nil => false,
        };
    }

    pub fn cast_obj(self: @This(), comptime DestType: type) !*DestType {
        var obj = switch (self) {
            .object => {
                switch (self.obj.ty) {
                    .String => String.from_obj(self.object),
                }
            },
            else => return RuntimeError.InvalidCast,
        };

        return obj;
    }

    pub fn cmp(self: @This(), other: IvyType) bool {
        return switch (self) {
            .bool => switch (other) {
                .bool => self.bool == other.bool,
                _ => false,
            },
            .num => switch (other) {
                .number => self.num == other.number,
                _ => false,
            },
            .nil => switch (other) {
                .nil => other == IvyType.nil,
                _ => false,
            },
            .object => switch (other) {
                .object => self.cmp(other),
                _ => false,
            },
        };
    }
};

pub fn boolean(b: bool) IvyType {
    return IvyType{ .bool = b };
}

pub fn nil() IvyType {
    return IvyType{ .nil = 0 };
}

pub fn number(n: f64) IvyType {
    return IvyType{ .num = n };
}

pub fn is_obj(t: IvyType) bool {
    return t == IvyType.object;
}

pub fn is_obj_type(t: IvyType, ot: Object.ObjectType) bool {
    return t == IvyType.object and t.object.ty == ot;
}

const IvyTypeList = std.ArrayList(IvyType);
