#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// TODO: Make an OpCode for !=, <=, and >=

typedef enum {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_RETURN,
} OpCode;

typedef struct {
    int count; // How many allocated entries are actually in use
    int capacity;  // The number of elements in the array we've allocated
    uint8_t* code;
    int* lines; // Line numbers corresponding to the bytecode instructions
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif