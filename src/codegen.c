#include "codegen.h"
#include "ast.h"
#include "chunk.h"
#include "value.h"
#include "scanner.h"

// Sends in previous token's line information so that runtime
// errors are associated with that line
static void emitByte(uint8_t byte) {
    // writeChunk(currentChunk(), byte, 1);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    // The + 2 is to take into account the size of the OP_LOOP instruction's
    // own operands which we also need to jump over
    // int offset = currentChunk()->count - loopStart + 2;
    int offset = 0;
    if (offset > UINT16_MAX) return; // error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // Two bytes (16-bit offset) lets us jump over up to 65,535 bytes of code
    emitByte(0xff);
    emitByte(0xff);

    return 3;
    // return currentChunk()->count - 2;
}

static void emitReturn() {

    if (true) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static void emitConstant(Value value) {
    // emitBytes(OP_CONSTANT, makeConstant(value));
}


void emit_bytecode_from_ast(ASTNode* node) {
    if (node == NULL) return;
    
    switch (node->type) {
        case NODE_BINARY:
            emit_bytecode_from_ast(node->as.binary.left);
            emit_bytecode_from_ast(node->as.binary.right);
            switch (node->as.binary.operator) {
                case TOKEN_PLUS: emitByte(OP_ADD); break;
                case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
                case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
                case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
                case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
                case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
                case TOKEN_GREATER: emitByte(OP_GREATER); break;
                case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
                case TOKEN_LESS: emitByte(OP_LESS); break;
                case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;
                default:
                    return; // Unreachable.
            }
            break;
        case NODE_CALL:
            emit_bytecode_from_ast(node->as.call.callee);
            for (int i = 0; i < node->as.call.arg_count; i++) {
                emit_bytecode_from_ast(node->as.call.arguments[i]);
            }
            emitBytes(OP_CALL, node->as.call.arg_count);
            break;
        case NODE_GET_PROPERTY:
            emit_bytecode_from_ast(node->as.get_property.object);
            emitConstant(OBJ_VAL(copyString(node->as.get_property.name.start, node->as.get_property.name.length)));
            emitByte(OP_GET_PROPERTY);
            break;
        case NODE_SET_PROPERTY:
            emit_bytecode_from_ast(node->as.set_property.object);
            emitConstant(OBJ_VAL(copyString(node->as.set_property.name.start, node->as.set_property.name.length)));
            emit_bytecode_from_ast(node->as.set_property.value);
            emitByte(OP_SET_PROPERTY);
            break;
        case NODE_LITERAL:
            switch (node->as.literal.token_type) {
                case TOKEN_FALSE: emitByte(OP_FALSE); break;
                case TOKEN_NIL: emitByte(OP_NIL); break;
                case TOKEN_TRUE: emitByte(OP_TRUE); break;
                default:
                    return; // Unreachable.
            }
            break;
        case NODE_NUMBER:
            emitConstant(NUMBER_VAL(node->as.number.value));
            break;
        default:
            return; // Unreachable.
    }
}