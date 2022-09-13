#ifndef clox_debug_h
#define clox_debug_h

#include <stdio.h>
#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
int disassembleInstruction(FILE* fptr, Chunk* chunk, int offset);

#endif