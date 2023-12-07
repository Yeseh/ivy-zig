const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const chunk = @import("chunk.zig");
const testing = @import("test/testing.zig");
const types = @import("types.zig");
const VM = @import("vm.zig").VirtualMachine;
const IvyType = @import("types.zig").IvyType;

const OpCode = common.OpCode;
const List = common.List;
const Chunk = chunk.Chunk;
const String = types.String;
const Object = types.Object;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    const argc = args.len;
    _ = argc;

    var vm = try VM.init(allocator);
    try run_file(allocator, &vm, "./programs/echo.ivy");
    // if (argc == 1) {
    //     std.debug.print("Ivy REPL v0.0.1\n", .{});
    //     try repl(allocator, &vm);
    // } else if (argc == 2) {
    //     _ = try run_direct(allocator, &vm, "Hello, World!");
    // } else {
    //     std.debug.print("Usage: zivy [src]\n", .{});
    // }

    std.process.argsFree(allocator, args);
    vm.deinit();
    _ = gpa.deinit();
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
        _ = try vm.interpret(sent_buf);
    }
}

fn run_direct(alloc: std.mem.Allocator, vm: *VM, cmd: []const u8) !void {
    // 4096 is the maximum up front allocated size of the buffer in readToEndAlloc
    const sent_buf: [:0]u8 = try alloc.allocSentinel(u8, cmd.len, 0);
    @memcpy(sent_buf, cmd.ptr);
    defer alloc.free(sent_buf);

    try vm.interpret(sent_buf);
}

fn run_file(alloc: std.mem.Allocator, vm: *VM, path: []const u8) !void {
    const source = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer source.close();

    // 4096 is the maximum up front allocated size of the buffer in readToEndAlloc
    const buf = try source.readToEndAllocOptions(vm.alloc, 4096, null, @alignOf(u8), 0);
    defer alloc.free(buf);

    try vm.interpret(buf[0..buf.len :0]);
    // vm.alloc.free(buf);
}

// test "Ivy.arith" {
//     std.debug.print("\n", .{});
//     const a = std.testing.allocator;
//     try testing.runVm(a, "1 + 2", .{ .ok = IvyType.number(3) });
//     try testing.runVm(a, "1 - 2", .{ .ok = IvyType.number(-1) });
//     try testing.runVm(a, "1 - -2", .{ .ok = IvyType.number(3) });
//     try testing.runVm(a, "2 * (1 - -2)", .{ .ok = IvyType.number(6) });
//     try testing.runVm(a, "(-1 + (2 * 4)) * 3 - -4", .{ .ok = IvyType.number(25) });
// }

// test "Ivy.booleans" {
//     std.debug.print("\n", .{});
//     const a = std.testing.allocator;

//     try testing.runVm(a, "true", .{ .ok = IvyType.boolean(true) });
//     try testing.runVm(a, "!true", .{ .ok = IvyType.boolean(false) });
//     try testing.runVm(a, "5>4", .{ .ok = IvyType.boolean(true) });
//     try testing.runVm(a, "3>4", .{ .ok = IvyType.boolean(false) });
//     try testing.runVm(a, "3<4", .{ .ok = IvyType.boolean(true) });
//     try testing.runVm(a, "5<4", .{ .ok = IvyType.boolean(false) });
//     try testing.runVm(a, "5==4", .{ .ok = IvyType.boolean(false) });
//     try testing.runVm(a, "5==5", .{ .ok = IvyType.boolean(true) });
// }

// test "Ivy.strings" {
//     std.debug.print("\n", .{});
//     const a = std.testing.allocator;

//     var str1 = try String.copy(a, "string");
//     try testing.runVm(a, "\"string\"", .{ .ok = IvyType.string(str1) });

//     var str2 = try String.copy(a, "Hello, World!");
//     try testing.runVm(a, "\"Hello, \" + \"World!\"", .{ .ok = IvyType.string(str2) });
// }
