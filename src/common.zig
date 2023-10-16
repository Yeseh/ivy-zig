pub const Chunk = @import("chunk.zig").Chunk;
pub const ChunkError = @import("chunk.zig").ChunkError;
pub const Value = @import("value.zig").Value;

pub const DEBUG_PRINT_CODE = true;

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_RETURN,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_DIVIDE,
    OP_MULTIPLY,
};
