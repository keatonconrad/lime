#ifndef clox_lib_h
#define clox_lib_h

#include <stdbool.h>

#include "value.h"

#define initNativePack NativePack pack;\
    pack.hadError = false;\
    pack.value = NIL_VAL;

typedef struct _NativePack {
    Value value;
    bool hadError;
} NativePack;

NativePack clockNative(int argCount, Value* args);
NativePack assertNative(int argCount, Value* args);
NativePack lenNative(int argCount, Value* args);

#endif