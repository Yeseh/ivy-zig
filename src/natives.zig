const std = @import("std");
const types = @import("types.zig");
const VM = @import("vm.zig").VirtualMachine;

const IvyType = types.IvyType;

pub fn defineNatives(vm: *VM) !void {
    try vm.defineNative("prnt", &print);
}

fn print(args: []IvyType) IvyType {
    for (args) |arg| {
        arg.print();
        std.debug.print("\n", .{});
    }
    return IvyType.nil();
}
