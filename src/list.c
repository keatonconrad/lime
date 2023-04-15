#include <stdio.h>
#include <string.h>

#include "list.h"
#include "object.h"
#include "memory.h"
#include "value.h"

void initList(List* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeList(List* array, void* value) {
    if (array->capacity < array->count + 1) {
        int oldCapactiy = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapactiy);
        array->values = GROW_ARRAY(void*, array->values, oldCapactiy, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeList(List* array) {
    FREE_ARRAY(void*, array->values, array->capacity);
    initList(array);
}

void* listGet(List* list, int index) {
    if (index < 0 || index >= list->count) {
        fprintf(stderr, "List index out of bounds: %d\n", index);
        exit(1);
    }
    return list->values[index];
}
