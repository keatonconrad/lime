#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapactiy = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapactiy);  // Figures out new capacity
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapactiy, chunk->capacity);  // Grows array to the new capacity
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapactiy, chunk->capacity);  // Grows array to the new capacity
    }

    chunk->code[chunk->count] = byte;  // Actually insert the data into the new space in the array
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

int addConstant(Chunk* chunk, Value value) {
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1; // Return the index where the constant was appended
}