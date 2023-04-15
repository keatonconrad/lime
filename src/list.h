#ifndef clox_list_h
#define clox_list_h

#include <stdlib.h>

typedef struct {
    int capacity;
    int count;
    void** values;
} List;

void initList(List* array);
void writeList(List* array, void* value);
void* listGet(List* array, int index);
void freeList(List* array);

#endif