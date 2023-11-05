pub const Chunk = @import("chunk.zig").Chunk;
pub const ChunkError = @import("chunk.zig").ChunkError;
pub const IvyType = @import("types.zig").IvyType;

pub const DEBUG_PRINT_CODE = true;

pub const OpCode = enum(u8) {
    CONSTANT,
    RETURN,
    NEGATE,
    ADD,
    SUBTRACT,
    DIVIDE,
    MULTIPLY,
    NOT,
    NIL,
    TRUE,
    FALSE,
    EQUAL,
    GREATER,
    LESS,
    // TODO: INCREMENT and DECREMENT
    // INCREMENT,
};
