const std = @import("std");

pub const IvyType = union(enum) {
    bool: bool,
    number: f64,
    nil: u1,

    pub fn to_bool(self: @This()) bool {
        return switch (self) {
            .bool => self.bool,
            .number => true,
            .nil => false,
        };
    }

    pub fn cmp(self: @This(), other: IvyType) bool {
        return switch (self) {
            .bool => switch (other) {
                .bool => self.bool == other.bool,
                _ => false,
            },
            .number => switch (other) {
                .number => self.number == other.number,
                _ => false,
            },
            .nil => switch (other) {
                .nil => other == IvyType.nil,
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
    return IvyType{ .number = n };
}

pub const IvyTypeList = std.ArrayList(IvyType);
