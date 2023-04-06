#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// Represents a single outgoing function call
typedef struct {
    ObjClosure* closure;
    uint8_t* ip; // When we return from a function, the VM jumps to the ip of the caller's CallFrame and resume from there
    Value* slots; // Points to the first slot in the VM's value stack this function can use
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop; // Points to where the next value to be pushed into the array will go
    Table globals;
    Table strings; // Interned strings
    ObjUpvalue* openUpvalues;
    Obj* objects;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
bool isFalsey(Value value);
void runtimeError(const char* format, ...);
void push(Value value);
Value pop();

#endif