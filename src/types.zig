const std = @import("std");
const String = @import("object/String.zig").String;
const Object = @import("object/Object.zig").Object;

const ArrayList = std.ArrayList;

pub const IvyType = union(enum) {
    bool: bool,
    num: f64,
    nil: u1,
    object: *Object,

    pub fn to_bool(self: *@This()) bool {
        return switch (self) {
            .bool => self.bool,
            .number => true,
            .object => true,
            .nil => false,
        };
    }

    pub fn cmp(self: *@This(), other: IvyType) bool {
        return switch (self) {
            .bool => switch (other) {
                .bool => self.bool == other.bool,
                _ => false,
            },
            .number => switch (other) {
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

pub fn is_obj_type(t: IvyType, ot: Object.Type) bool {
    return t == IvyType.object and t.obj.type == ot;
}

const IvyTypeList = std.ArrayList(IvyType);
