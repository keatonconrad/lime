#ifndef clox_codegen_h
#define clox_codegen_h

#include <stdbool.h>

#include "ast.h"
#include "object.h"

ObjFunction* compileASTToBytecode(ASTNode* node);

#endif