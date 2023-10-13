const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const alloc = arena.allocator();
    var chunk = try Chunk.init(alloc);
    defer chunk.deinit(alloc);

    try chunk.write_op(alloc, 0);
    try chunk.write_op(alloc, 0);
    try chunk.write_op(alloc, 1);

    try chunk.write_line(alloc, 1);
    try chunk.write_line(alloc, 1);
    try chunk.write_line(alloc, 2);

    var op0 = try chunk.get_line_for_op(0);
    std.debug.print("Got line: {}\n", .{op0});
    var op1 = try chunk.get_line_for_op(1);
    std.debug.print("Got line: {}\n", .{op1});
    var op2 = try chunk.get_line_for_op(2);
    std.debug.print("Got line: {}\n", .{op2});
}
