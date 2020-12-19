#include <stdio.h>
#include <stdint.h>

// CurryNodes are represented as tagged unions,
// the following constants define the semantics
// of the tag.
const uint8_t TAG_DATA      = 0; // Applied (data) constructors
const uint8_t TAG_FUNCTION  = 1; // (Partially applied) functions
const uint8_t TAG_INTEGER   = 2; // 64-bit integers
const uint8_t TAG_FLOATING  = 3; // Floating-point numbers
const uint8_t TAG_CHARACTER = 4; // 8-bit characters

// TODO: Failures and choices!

struct CurryNode;

// Represents an applied (data) constructor.
struct CurryData {
    uint8_t arity;
    uint64_t type;                // The runtime type of the value
    uint64_t constructor;         // The constructor index of the value
    struct CurryNode **arguments; // The constructor's children.
                                  // Generally allocated on the heap with
                                  // exactly 'arity' elements.
};

// Represents a (possibly partially applied) function.
struct CurryFunction {
    uint8_t arity;
    struct CurryNode **arguments; // The partially applied parameters.
                                  // Generally allocated on the heap with
                                  // exactly 'arity' elements.
    struct CurryNode *(*funcPtr)(struct CurryNode *);
};

// Represents a Curry value at runtime.
// Generally allocated on the heap.
struct CurryNode {
    uint8_t tag;
    union {
        struct CurryData data;
        struct CurryFunction function;
        uint64_t integer;
        double floating;
        uint8_t character;
    } value;
};
