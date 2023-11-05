const std = @import("std");
const common = @import("common.zig");
const debug = @import("debug.zig");
const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VirtualMachine;

const OpCode = common.OpCode;
const List = common.List;
const Chunk = chunk.Chunk;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{});
    defer gpa.deinit();
    const allocator = gpa.allocator();
    var vm = try VM.init(allocator);
    defer vm.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const argc = args.len;
    if (argc == 1) {
        try repl(&vm);
    } else if (argc == 2) {
        try run_file(&vm, args[1]);
    } else {
        std.debug.print("Usage: zivy [path]\n", .{});
        return;
    }
}

fn repl(vm: *VM) !void {
    while (true) {
        std.debug.print("> ", .{});
        const stdin = std.io.getStdIn().reader();
        const buf = try stdin.readUntilDelimiterAlloc(vm.alloc, '\n', 1024);
        const sent_buf: [:0]u8 = try vm.alloc.allocSentinel(u8, buf.len, 0);

        if (buf.len == 0) {
            std.debug.print("\n", .{});
            break;
        }
        @memcpy(sent_buf, buf.ptr);
        vm.alloc.free(buf);

        try vm.interpret(sent_buf);
    }
}

fn run_file(vm: *VM, path: []const u8) !void {
    const source = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer source.close();

    // 4096 is the maximum up front allocated size of the buffer in readToEndAlloc
    const buf = try source.readToEndAlloc(vm.alloc, 4096);
    const sent_buf: [:0]const u8 = try vm.alloc.allocSentinel(u8, buf.len, 0);
    @memcpy(sent_buf, buf.ptr);
    vm.alloc.free(buf);

    try vm.interpret(sent_buf);
}
