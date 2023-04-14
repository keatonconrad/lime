#ifndef clox_compiler_h
#define clox_compiler_h

// #include "ast.h"
#include "object.h"
#include "vm.h"

struct ASTNode;

typedef enum {
    TYPE_FUNCTION,
    TYPE_METHOD,
    TYPE_INITIALIZER,
    TYPE_SCRIPT
} FunctionType;

struct ASTNode* compile(const char* source);
void markCompilerRoots();

#endif