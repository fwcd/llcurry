/*
 * The Curry runtime defines the 'central' type that represents
 * Curry values (on the heap) at runtime: CurryNode. Additionally,
 * it provides operations for allocating, initializing and freeing
 * such nodes, providing a layer of abstraction that the generated
 * LLVM IR code operates on.
 * 
 * It uses assertions to check for invariants that the code generator
 * should maintain, these can however be disabled for performance.
 */

#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// CurryNodes are represented as tagged unions,
// the following constants define the semantics
// of the tag.
const uint8_t TAG_DATA        = 0; // Applied (data) constructors
const uint8_t TAG_FUNCTION    = 1; // (Partially applied) functions
const uint8_t TAG_INTEGER     = 2; // 64-bit integers
const uint8_t TAG_FLOATING    = 3; // Floating-point numbers
const uint8_t TAG_CHARACTER   = 4; // 8-bit characters
const uint8_t TAG_PLACEHOLDER = 5; // Empty notes

// TODO: Failures and choices!

struct CurryNode;

// Represents an applied (data) constructor.
struct CurryData {
    uint8_t arity;                // The maximum number of arguments.
    uint64_t type;                // The runtime type of the value
    uint64_t constructor;         // The constructor index of the value
    uint8_t argumentCount;        // The number of already-applied arguments.
    struct CurryNode **arguments; // The constructor's children.
                                  // Generally allocated on the heap with
                                  // exactly 'arity' elements.
};

// Represents a (possibly partially applied) function.
struct CurryFunction {
    uint8_t arity;                // The maximum number of arguments.
    uint8_t argumentCount;        // The number of already-applied arguments.
    struct CurryNode **arguments; // The partially applied arguments.
                                  // Generally allocated on the heap with
                                  // exactly 'arity' elements.
    struct CurryNode *(*funcPtr)(struct CurryNode *);
};

// Represents a value from Curry at runtime. Curry nodes are allocated on
// the heap and reference-counted. The initial reference count is always
// 1 and it is the caller's responsibility to insert retain/release calls.
struct CurryNode {
    uint8_t refCount;
    uint8_t tag;
    union {
        struct CurryData data;
        struct CurryFunction function;
        uint64_t integer;
        double floating;
        uint8_t character;
    } value;
};

// Allocates a new Curry node. This function is internal to the runtime
// and should not be called by generated code.
struct CurryNode *curryNodeAllocate(uint8_t tag) {
    struct CurryNode *node = malloc(sizeof(struct CurryNode));
    node->tag = tag;
    node->refCount = 1;
    return node;
}

// Creates a new data constructor with no arguments applied to it.
struct CurryNode *curryNodeNewData(uint8_t arity, uint64_t type, uint64_t constructor) {
    assert(arity >= 0);
    assert(constructor >= 0);

    struct CurryNode *node = curryNodeAllocate(TAG_DATA);
    struct CurryData *data = &node->value.data;
    data->arity = arity;
    data->type = type;
    data->constructor = constructor;
    data->argumentCount = 0;
    data->arguments = malloc(sizeof(struct CurryNode *) * arity);
    return node;
}

// Creates a new function with no arguments applied to it.
struct CurryNode *curryNodeNewFunction(uint8_t arity, struct CurryNode *(*funcPtr)(struct CurryNode *)) {
    assert(arity > 0);
    assert(funcPtr != NULL);

    struct CurryNode *node = curryNodeAllocate(TAG_DATA);
    struct CurryFunction *function = &node->value.function;
    function->arity = arity;
    function->argumentCount = 0;
    function->arguments = malloc(sizeof(struct CurryNode *) * arity);
    function->funcPtr = funcPtr;
    return node;
}

// Creates a new node holding a 64-bit integer.
struct CurryNode *curryNodeNewInteger(uint64_t integer) {
    struct CurryNode *node = curryNodeAllocate(TAG_INTEGER);
    node->value.integer = integer;
    return node;
}

// Creates a new node holding a single floating point number.
struct CurryNode *curryNodeNewFloating(double floating) {
    struct CurryNode *node = curryNodeAllocate(TAG_FLOATING);
    node->value.floating = floating;
    return node;
}

// Creates a new placeholder node.
struct CurryNode *curryNodeNewPlaceholder(void) {
    return curryNodeAllocate(TAG_PLACEHOLDER);
}

// Accesses a node's nth argument.
struct CurryNode *curryNodeAccess(struct CurryNode *node, uint64_t n) {
    assert(node->tag == TAG_FUNCTION || node->tag == TAG_DATA);
    assert(n >= 0);
    switch (node->tag) {
    case TAG_FUNCTION:
        assert(n < node->value.function.argumentCount);
        return node->value.function.arguments[n];
    case TAG_DATA:
        assert(n < node->value.data.argumentCount);
        return node->value.data.arguments[n];
    default:
        return NULL;
    }
}

// Creates a new node holding an 8-bit character.
struct CurryNode *curryNodeNewCharacter(uint8_t character) {
    struct CurryNode *node = curryNodeAllocate(TAG_CHARACTER);
    node->value.character = character;
    return node;
}

// Increments a Curry node's reference count.
void curryNodeRetain(struct CurryNode *node) {
    node->refCount++;
}

// Decrements a Curry node's reference count. Once the reference count
// drops to zero, this function will also _deallocate_ the node, which
// means that every further access is undefined behavior.
void curryNodeRelease(struct CurryNode *node) {
    node->refCount--;
    if (node->refCount <= 0) {
        struct CurryNode **arguments = NULL;
        int argumentCount;

        switch (node->tag) {
        case TAG_DATA:
            arguments = node->value.data.arguments;
            argumentCount = node->value.data.argumentCount;
            break;
        case TAG_FUNCTION:
            arguments = node->value.function.arguments;
            argumentCount = node->value.function.argumentCount;
            break;
        default:
            break;
        }

        if (arguments != NULL) {
            for (int i = 0; i < argumentCount; i++) {
                curryNodeRelease(arguments[i]);
            }
        }

        free(node);
    }
}

// Applies an argument to an unsatisfied data/constructor node.
void curryNodeDataApply(struct CurryNode *node, struct CurryNode *argument) {
    assert(node->tag == TAG_DATA);
    struct CurryData *data = &node->value.data;
    assert(data->argumentCount < data->arity);
    data->arguments[data->argumentCount] = argument;
    data->argumentCount++;
    curryNodeRetain(argument);
}

// Applies an argument to a function node. Note that this does NOT
// automatically evaluate the result once fully applied.
void curryNodeFunctionApply(struct CurryNode *node, struct CurryNode *argument) {
    assert(node->tag == TAG_FUNCTION);
    struct CurryFunction *function = &node->value.function;
    assert(function->argumentCount < function->arity);
    function->arguments[function->argumentCount] = argument;
    function->argumentCount++;
    curryNodeRetain(argument);
}

// Prints a Curry node to stdout for debugging purposes.
void curryNodePrint(struct CurryNode *node) {
    printf("CurryNode [%d refs] ", node->refCount);
    switch (node->tag) {
    case TAG_DATA:
        {
            struct CurryData *data = &node->value.data;
            printf(
                "DATA %" PRIu8 "/%" PRIu8 " args, type: %" PRIu64 ", constr: %" PRIu64,
                data->argumentCount, data->arity, data->type, data->constructor
            );
        }
        break;
    case TAG_FUNCTION:
        {
            struct CurryFunction *function = &node->value.function;
            printf(
                "FUNCTION %" PRIu8 "/%" PRIu8 " args, %p",
                function->argumentCount, function->arity, function->funcPtr
            );
        }
        break;
    case TAG_INTEGER:
        printf("INTEGER %" PRIu64, node->value.integer);
        break;
    case TAG_FLOATING:
        printf("FLOATING %lf", node->value.floating);
        break;
    case TAG_CHARACTER:
        printf("CHARACTER %c", node->value.character);
        break;
    case TAG_PLACEHOLDER:
        printf("PLACEHOLDER");
        break;
    default:
        printf("UNKNOWN");
        break;
    }
    printf("\n");
}
