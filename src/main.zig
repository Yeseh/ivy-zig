const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VirtualMachine;
const IvyType = @import("types.zig").IvyType;

const OpCode = common.OpCode;
const List = common.List;
const Chunk = chunk.Chunk;

// TODO: Args and vm are not deinitialized properly with GPA
pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    var alloc_type = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer alloc_type.deinit();
    const allocator = alloc_type.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    const args = try std.process.argsAlloc(allocator);
    errdefer std.process.argsFree(allocator, args);

    const argc = args.len;
    if (argc == 1) {
        try repl(allocator, &vm);
    } else if (argc == 2) {
        try run_direct(allocator, &vm, args[1]);
    } else {
        std.debug.print("Usage: zivy [src]\n", .{});
    }
    // var leaked = gpa.deinit();
    // std.debug.print("Ivy leaked: {}\n", .{leaked});

    std.process.argsFree(allocator, args);
    vm.deinit();
}

fn repl(alloc: std.mem.Allocator, vm: *VM) !void {
    while (true) {
        std.debug.print("> ", .{});
        const stdin = std.io.getStdIn().reader();
        const buf = try stdin.readUntilDelimiterAlloc(alloc, '\n', 1024);

        const sent_buf: [:0]u8 = try alloc.allocSentinel(u8, buf.len, 0);

        if (buf.len == 0) {
            std.debug.print("\n", .{});
            continue;
        }

        @memcpy(sent_buf, buf.ptr);
        try vm.interpret(sent_buf);
    }
}

fn run_direct(alloc: std.mem.Allocator, vm: *VM, cmd: []u8) !IvyType {
    // 4096 is the maximum up front allocated size of the buffer in readToEndAlloc
    const sent_buf: [:0]u8 = try alloc.allocSentinel(u8, cmd.len, 0);
    @memcpy(sent_buf, cmd.ptr);
    defer alloc.free(sent_buf);

    return try vm.interpret(sent_buf);
}

// fn run_file(vm: *VM, path: []u8) !void {
//     const source = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
//     defer source.close();

//     // 4096 is the maximum up front allocated size of the buffer in readToEndAlloc
//     const buf = try source.readToEndAllocOptions(vm.alloc, 4096, null, @alignOf(u8), 0);
//     defer vm.alloc.free(buf);

//     try vm.interpret(buf);
//     // vm.alloc.free(buf);
// }

test "Ivy.addition" {
    const allocator = std.testing.allocator;
    var vm = try VM.init(allocator);
    defer vm.deinit();

    var source = "1 + 2";
    var sent_buf: [:0]u8 = try allocator.allocSentinel(u8, source.len, 0);
    @memcpy(sent_buf, source.ptr);
    defer allocator.free(sent_buf);

    var result = try vm.interpret(@constCast(sent_buf));
    switch (result) {
        .num => try std.testing.expect(result.num == 3.0),
        else => try std.testing.expect(false),
    }
}

test "Ivy.booleans" {
    const allocator = std.testing.allocator;
    var vm = try VM.init(allocator);
    defer vm.deinit();

    var source = "!false";
    var sent_buf: [:0]u8 = try allocator.allocSentinel(u8, source.len, 0);
    @memcpy(sent_buf, source.ptr);
    defer allocator.free(sent_buf);

    var result = try vm.interpret(@constCast(sent_buf));
    switch (result) {
        .bool => try std.testing.expect(result.bool),
        else => try std.testing.expect(false),
    }
}
