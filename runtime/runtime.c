#include <stdio.h>
#include <stdint.h>

// Used by CurryNodes that represent applied
// data constructors.
const uint8_t TAG_DATA = 0;

// Used by CurryNodes that represent functions
// partial applications.
const uint8_t TAG_FUNCTION = 2;

// Represents an applied (data) constructor.
struct CurryData {

};

// Represents a (possibly partially applied)
// function.
struct CurryFunction {

};

// Represents a Curry value at runtime.
// Generally allocated on the heap.
struct CurryNode {
    uint8_t tag;
    union {
        struct CurryData data;
        struct CurryFunction function;
    } value;
};
