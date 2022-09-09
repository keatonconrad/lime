#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type) // Extracts the object type tag from a Value
#define IS_STRING(value)    isObjType(value, OBJ_STRING)

#define AS_STRING(value)    ((ObjString*)AS_OBJ(value)) // Casts value to string object
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars) // Casts value to string object, then extracts the character array

typedef enum {
    OBJ_STRING
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next;
};

struct ObjString {
    Obj obj;
    int length;
    char* chars; // TODO: Implement using a flexible array member instead
};

ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif