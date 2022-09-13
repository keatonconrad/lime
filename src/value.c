#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapactiy = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapactiy);
        array->values = GROW_ARRAY(Value, array->values, oldCapactiy, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(FILE* fptr, Value value) {
    switch (value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            fprintf(fptr, AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL: printf("nil"); fprintf(fptr, "nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); fprintf(fptr, "%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(fptr, value); break;
    }
}

bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL:   return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:    return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:    return AS_OBJ(a) == AS_OBJ(b);
        default:         return false; // Unreachable
    }
}