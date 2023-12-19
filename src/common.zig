pub const Chunk = @import("chunk.zig").Chunk;
pub const ChunkError = @import("chunk.zig").ChunkError;
pub const IvyType = @import("types.zig").IvyType;
pub const Table = @import("table.zig");

// TODO: Should be cmdline flags
pub const DEBUG_PRINT_CODE = false;
pub const DEBUG_PRINT_RETURN = false;
pub const DEBUG_PRINT_GC = false;
pub const DEBUG_PRINT_SOURCE = false;

pub const RuntimeError = error{IndexOutOfBounds};

pub const OpCode = enum(u8) {
    CONSTANT,
    RETURN,
    NEGATE,
    ADD,
    // TODO: Replace with ADD+NEGATE, MULTIPLY*.5
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
    POP,
    POP_N,
    GET_LOCAL,
    SET_LOCAL,
    DEFINE_GLOBAL,
    GET_GLOBAL,
    SET_GLOBAL,
    LOOP,
    JUMP,
    JUMP_IF_FALSE,
    CALL,
    // TODO: INCREMENT and DECREMENT
    // INCREMENT,
};
