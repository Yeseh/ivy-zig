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

    const argc = args.len;
    if (argc == 1) {
        try repl(allocator, &vm);
    } else if (argc == 2) {
        try run_file(&vm, args[1]);
    } else {
        std.debug.print("Usage: zivy [path]\n", .{});
        return;
    }
}

fn repl(alloc: std.mem.Allocator, vm: *VM) !void {
    while (true) {
        std.debug.print("> ", .{});
        const stdin = std.io.getStdIn().reader();
        const buf = try stdin.readUntilDelimiterAlloc(alloc, '\n', 1024);

        if (buf.len == 0) {
            std.debug.print("\n", .{});
            break;
        }

        try vm.interpret(buf);
    }
}

fn run_file(vm: *VM, path: []const u8) !void {
    const source = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer source.close();

    // 4096 is the maximum up front allocated size of the buffer in readToEndAlloc
    const buf = try source.readToEndAlloc(vm.alloc, 4096);
    try vm.interpret(buf);
}
