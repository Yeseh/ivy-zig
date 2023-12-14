const std = @import("std");
const types = @import("types.zig");
const VM = @import("vm.zig").VirtualMachine;

const IvyType = types.IvyType;

pub fn defineNatives(vm: *VM) !void {
    try vm.defineNative("prnt", &ivyPrint);
}

fn ivyPrint(args: []IvyType) IvyType {
    std.debug.print("Printing: {any}\n", .{args});
    for (args) |arg| {
        std.debug.print("  {}\n", .{arg.print()});
    }

    return IvyType.nil();
}
