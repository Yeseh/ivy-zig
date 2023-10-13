const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VirtualMachine;

const OpCode = common.OpCode;
const List = common.List;
const Chunk = chunk.Chunk;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var vm = try VM.init(allocator);
    defer vm.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.warn("Usage: zivy [path]\n", .{});
        return;
    }
}
