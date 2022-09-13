#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "value.h"

static FILE* openFile() {
    FILE* fptr = fopen("./output.my", "w");
    if (fptr == NULL) {
        printf("Oh no");   
        exit(1);             
    }
    return fptr;
}

void disassembleChunk(Chunk* chunk, const char* name) {
    FILE* fptr = openFile();
    printf("== %s ==\n", name);
    fprintf(fptr, "== %s ==\n", name);

    for (int offset = 0; offset < chunk-> count;) {
        offset = disassembleInstruction(fptr, chunk, offset);
    }
}

static int constantInstruction(FILE* fptr, const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant);
    fprintf(fptr, "%-16s %4d '", name, constant);
    printValue(fptr, chunk->constants.values[constant]);
    printf("'\n");
    fprintf(fptr, "'\n");
    return offset + 2;
}

static int simpleInstruction(FILE* fptr, const char* name, int offset) {
    printf("%s\n", name);
    fprintf(fptr, "%s\n", name);
    return offset + 1;
}

static int byteInstruction(FILE* fptr, const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    fprintf(fptr, "%-16s %4d\n", name, slot);
    return offset + 2;
}

int disassembleInstruction(FILE* fptr, Chunk* chunk, int offset) {
    printf("%04d ", offset);
    fprintf(fptr, "%04d ", offset);

    if (offset > 0 &&
        chunk->lines[offset] == chunk->lines[offset - 1]) {
        printf("   | ");
        fprintf(fptr, "   | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
        fprintf(fptr, "%4d ", chunk->lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_CONSTANT:
            return constantInstruction(fptr, "OP_CONSTANT", chunk, offset);
        case OP_NIL:
            return simpleInstruction(fptr, "OP_NIL", offset);
        case OP_TRUE:
            return simpleInstruction(fptr, "OP_TRUE", offset);
        case OP_FALSE:
            return simpleInstruction(fptr, "OP_FALSE", offset);
        case OP_POP:
            return simpleInstruction(fptr, "OP_POP", offset);
        case OP_POPN:
            return byteInstruction(fptr, "OP_POPN", chunk, offset);
        case OP_GET_LOCAL:
            return byteInstruction(fptr, "OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return byteInstruction(fptr, "OP_SET_LOCAL", chunk, offset);
        case OP_GET_GLOBAL:
            return constantInstruction(fptr, "OP_GET_GLOBAL", chunk, offset);
        case OP_DEFINE_GLOBAL:
            return constantInstruction(fptr, "OP_DEFINE_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL:
            return constantInstruction(fptr, "OP_SET_GLOBAL", chunk, offset);
        case OP_EQUAL:
            return simpleInstruction(fptr, "OP_EQUAL", offset);
        case OP_GREATER:
            return simpleInstruction(fptr, "OP_GREATER", offset);
        case OP_LESS:
            return simpleInstruction(fptr, "OP_LESS", offset);
        case OP_ADD:
            return simpleInstruction(fptr, "OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction(fptr, "OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction(fptr, "OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction(fptr, "OP_DIVIDE", offset);
        case OP_NOT:
            return simpleInstruction(fptr, "OP_NOT", offset);
        case OP_NEGATE:
            return simpleInstruction(fptr, "OP_NEGATE", offset);
        case OP_PRINT:
            return simpleInstruction(fptr, "OP_PRINT", offset);
        case OP_RETURN:
            return simpleInstruction(fptr, "OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}
