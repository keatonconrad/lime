#include <stdio.h>

#include "codegen.h"
#include "ast.h"
#include "chunk.h"
#include "value.h"
#include "scanner.h"

// Stores local variable
typedef struct {
    Token name; // The name of the variable
    int depth; // The scope depth of the block where the local variable was declared
    bool isCaptured; // Whether the local variable is captured by a closure
} Local;

typedef struct {
    uint8_t index; // Stores which local slot the upvalue is capturing
    bool isLocal; // Whether the closure captures a local variable or an upvalue from the surrounding function
} Upvalue;

typedef struct _Compiler {
    struct _Compiler* enclosing;
    ObjFunction* function; // A reference to the function object being built
    FunctionType type; // Signals function body vs top-level code

    Local locals[UINT8_COUNT];
    int localCount; // How many local variables are in scope
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth; // Number of blocks surrounding the current bit of code being compiled
} Compiler;

typedef struct {
	bool isInLoop;
	int loopStart; // Stores current loop starting position to use with continue.
	int jumpToExit; // Stores break statement JUMP to patch for exit. This allows only one break per loop
} LoopMetadata;

static Compiler* current;
LoopMetadata loopMetadata;

static void errorAt(Token* token, const char* message) {
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
}

static void error(const char* message) {
    errorAt("token", message);
}

static void errorAtCurrent(const char* message) {
    errorAt("token", message);
}

static Chunk* currentChunk() {
    return &current->function->chunk;
}

// Sends in previous token's line information so that runtime
// errors are associated with that line
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, 1);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int offset) {
    printf("offset: %d", offset);
    emitByte(OP_LOOP);

    // The + 2 is to take into account the size of the OP_LOOP instruction's
    // own operands which we also need to jump over
    // int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // Two bytes (16-bit offset) lets us jump over up to 65,535 bytes of code
    emitByte(0xff);
    emitByte(0xff);

    return currentChunk()->count - 2;
}

// Goes back into the bytecode and replaces the operand at the given
// location with the calculated jump offset
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void emitReturn() {

    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static void initLoopMetadata() {
	loopMetadata.isInLoop = false;
	loopMetadata.loopStart = -1;
	loopMetadata.jumpToExit = -1;
}

static void handleLoopMetadata() {
    if (loopMetadata.jumpToExit != -1) {
        patchJump(loopMetadata.jumpToExit);
    }
    initLoopMetadata();
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    // The compiler claims stack slot zero for the VM's own internal use
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
    return function;
}

static void emitBinary(ASTNode* node) {
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
    };
}


void emit_bytecode_from_ast(ASTNode* node, Compiler* compiler) {
    if (node == NULL) return;
    
    switch (node->type) {
        case NODE_BINARY: {
            emit_bytecode_from_ast(node->as.binary.left, compiler);
            emit_bytecode_from_ast(node->as.binary.right, compiler);
            emitBinary(node);
            break;
        }
        case NODE_CALL: {
            print_ast_node(node->as.call.callee, 0);
            printf("printed callee node ------- \n");
            print_ast_node((ASTNode*)(node->as.call.arguments->values[0]), 0);
            printf("printed values ------- \n");
            emit_bytecode_from_ast(node->as.call.callee, compiler);
            if (node->as.call.arguments != NULL) {
                printf("arguments count: %d\n", node->as.call.arguments->count);
                for (int i = 0; i < node->as.call.arguments->count; i++) {
                    emit_bytecode_from_ast((ASTNode*)node->as.call.arguments->values[i], compiler);
                }
            } else {
                printf("no arguments ------- \n");
            }
            emitBytes(OP_CALL, node->as.call.arguments->count);
            emitByte(OP_POP);
            break;
        }
        case NODE_GET_PROPERTY: {
            emit_bytecode_from_ast(node->as.get_property.object, compiler);
            emitConstant(OBJ_VAL(copyString(node->as.get_property.name.start, node->as.get_property.name.length)));
            emitByte(OP_GET_PROPERTY);
            break;
        }
        case NODE_SET_PROPERTY: {
            emit_bytecode_from_ast(node->as.set_property.object, compiler);
            emitConstant(OBJ_VAL(copyString(node->as.set_property.name.start, node->as.set_property.name.length)));
            emit_bytecode_from_ast(node->as.set_property.value, compiler);
            emitByte(OP_SET_PROPERTY);
            break;
        }
        case NODE_LITERAL: {
            switch (node->as.literal.token_type) {
                case TOKEN_FALSE: emitByte(OP_FALSE); break;
                case TOKEN_NIL: emitByte(OP_NIL); break;
                case TOKEN_TRUE: emitByte(OP_TRUE); break;
                default:
                    return; // Unreachable.
            }
            break;
        }
        case NODE_NUMBER: {
            emitConstant(NUMBER_VAL(node->as.number.value));
            break;
        }
        case NODE_VARIABLE_ASSIGNMENT: {
            emit_bytecode_from_ast(node->as.variableAssignment.value, compiler);
            switch (node->as.variableAssignment.accessType) {
                case ACCESS_LOCAL: {
                    emitBytes(OP_SET_LOCAL, node->as.variableAssignment.arg);
                    break;
                }
                case ACCESS_GLOBAL: {
                    emitBytes(OP_DEFINE_GLOBAL, makeConstant(OBJ_VAL(copyString(node->as.variableAssignment.name.start, node->as.variableAssignment.name.length))));
                    break;
                }
                case ACCESS_UPVALUE: {
                    emitBytes(OP_SET_UPVALUE, node->as.variableAssignment.arg);
                    break;
                }
            }
            break;
        }
        case NODE_VARIABLE_ACCESS: {
            switch (node->as.variableAccess.accessType) {
                case ACCESS_LOCAL: {
                    emitBytes(OP_GET_LOCAL, node->as.variableAccess.arg);
                    break;
                }
                case ACCESS_GLOBAL: {
                    emitBytes(OP_GET_GLOBAL, makeConstant(OBJ_VAL(copyString(node->as.variableAccess.name.start, node->as.variableAccess.name.length))));
                    break;
                }
                case ACCESS_UPVALUE: {
                    emitBytes(OP_GET_UPVALUE, node->as.variableAccess.arg);
                    break;
                }
            }
            break;
        }
        case NODE_STRING: {
            emitConstant(OBJ_VAL(copyString(node->as.string.string, node->as.string.length)));
            break;
        }
        case NODE_WHILE_STATEMENT: {            
            emit_bytecode_from_ast(node->as.while_statement.condition, compiler);
            int exitJump = emitJump(OP_JUMP_IF_FALSE);
            
            emitByte(OP_POP);
            emit_bytecode_from_ast(node->as.while_statement.body, compiler);
            
            emitLoop(node->as.while_statement.offset);
            
            patchJump(exitJump);
            emitByte(OP_POP);
            break;
        }
        case NODE_BREAK_STATEMENT: {
            break;
        }
        case NODE_BLOCK: {
            for (int i = 0; i < node->as.block.statement_count; i++) {
                emit_bytecode_from_ast((ASTNode*)listGet(&node->as.block.statements, i), compiler);
            }
            break;
        }
        case NODE_IF_STATEMENT: {
            emit_bytecode_from_ast(node->as.if_statement.condition, compiler);
            int thenJump = emitJump(OP_JUMP_IF_FALSE);
            emitByte(OP_POP);
            emit_bytecode_from_ast(node->as.if_statement.then_branch, compiler);
            int elseJump = emitJump(OP_JUMP);
            
            patchJump(thenJump);
            emitByte(OP_POP);
            
            if (node->as.if_statement.else_branch != NULL) {
                emit_bytecode_from_ast(node->as.if_statement.else_branch, compiler);
            }
            
            patchJump(elseJump);
            break;
        }
        case NODE_CLASS_DECLARATION: {
            emitConstant(OBJ_VAL(copyString(node->as.classDeclaration.className.start, node->as.classDeclaration.className.length)));
            emitByte(OP_CLASS);
            break;
        }
        default:
            return; // Unreachable.
    }
    
    return;
}

ObjFunction* compileASTToBytecode(ASTNode* node) {
    if (node == NULL) return NULL;

    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);
    
    // Recursively emit bytecode for the AST
    emit_bytecode_from_ast(node, &compiler);

    // Create the ObjFunction and return it
    return endCompiler();
}
