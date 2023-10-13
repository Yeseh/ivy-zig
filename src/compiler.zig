const std = @import("std");
const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const TokenType = scanner.TokenType;

pub fn compile(alloc: std.mem.Allocator, source: []u8) !void {
    var line: i32 = -1;
    var scan = try Scanner.init(alloc, source);

    while (true) {
        var token = try scan.scan_token();
        if (token.line != line) {
            std.debug.print("{:0>4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("  | ", .{});
        }
        token.dump();

        if (token.type == TokenType.EOF) break;
    }
}
