#include <stdio.h>
#include <time.h>

#include "common.h"
#include "lib.h"
#include "value.h"
#include "vm.h"

static bool checkArity(NativePack* pack, int expectedArgCount, int actualArgCount) {
    if (expectedArgCount != actualArgCount) {
        if (expectedArgCount == 1) {
            runtimeError("Expected 1 argument but got %d.", actualArgCount);
        } else {
            runtimeError("Expected %d arguments but got %d.", expectedArgCount, actualArgCount);
        }
        pack->hadError = true;
    }
    return !pack->hadError;
}

NativePack clockNative(int argCount, Value* args) {
    initNativePack;

    if (!checkArity(&pack, 0, argCount)) return pack;

    pack.value = NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
    return pack;
}

NativePack assertNative(int argCount, Value* args) {
    initNativePack;

    if (!checkArity(&pack, 1, argCount)) return pack;

    if (isFalsey(args[0])) {
        runtimeError("Assertion error.");
        pack.hadError = true;
    }

    pack.value = args[0];
    return pack;
}

NativePack lenNative(int argCount, Value* args) {
    initNativePack;

    if (!checkArity(&pack, 1, argCount)) return pack;

    if (IS_STRING(args[0])) pack.value = NUMBER_VAL(AS_STRING(args[0])->length);
    else pack.hadError = true;

    return pack;
}