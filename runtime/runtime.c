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
const uint8_t TAG_CHOICE      = 2; // Non-deterministic choice nodes
const uint8_t TAG_INTEGER     = 3; // 64-bit integers
const uint8_t TAG_FLOATING    = 4; // Floating-point numbers
const uint8_t TAG_CHARACTER   = 5; // 8-bit characters
const uint8_t TAG_PLACEHOLDER = 6; // Empty nodes
const uint8_t TAG_FAILURE     = 7; // Failure nodes
const uint8_t TAG_FREE        = 8; // Free nodes

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

// Represents a non-deterministic, binary choice.
struct CurryChoice {
    struct CurryNode *left;
    struct CurryNode *right;
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
        struct CurryChoice choice;
        uint64_t integer;
        double floating;
        uint8_t character;
    } value;
};

void curryNodeRetain(struct CurryNode *node);

void curryNodeRelease(struct CurryNode *node);

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

// Creates a new choice node from the given two nodes.
struct CurryNode *curryNodeNewChoice(struct CurryNode *left, struct CurryNode *right) {
    assert(left != NULL);
    assert(right != NULL);

    struct CurryNode *node = curryNodeAllocate(TAG_CHOICE);
    struct CurryChoice *choice = &node->value.choice;
    choice->left = left;
    choice->right = right;
    curryNodeRetain(left);
    curryNodeRetain(right);
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

// Creates a new free node.
struct CurryNode *curryNodeNewFree(void) {
    return curryNodeAllocate(TAG_FREE);
}

// Creates a new failure node.
struct CurryNode *curryNodeNewFailure(void) {
    return curryNodeAllocate(TAG_FAILURE);
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

// Internal function for releasing AND FREEING the arguments.
void curryNodeReleaseArguments(struct CurryNode *node) {
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
    case TAG_CHOICE:
        curryNodeRelease(node->value.choice.left);
        curryNodeRelease(node->value.choice.right);
        break;
    default:
        break;
    }

    if (arguments != NULL) {
        for (int i = 0; i < argumentCount; i++) {
            curryNodeRelease(arguments[i]);
        }
        free(arguments);
    }
}

// Internal function for retaining only the arguments.
void curryNodeRetainArguments(struct CurryNode *node) {
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
    case TAG_CHOICE:
        curryNodeRetain(node->value.choice.left);
        curryNodeRetain(node->value.choice.right);
        break;
    default:
        break;
    }

    if (arguments != NULL) {
        for (int i = 0; i < argumentCount; i++) {
            curryNodeRetain(arguments[i]);
        }
    }
}

// Decrements a Curry node's reference count. Once the reference count
// drops to zero, this function will also _deallocate_ the node, which
// means that every further access is undefined behavior.
void curryNodeRelease(struct CurryNode *node) {
    node->refCount--;
    if (node->refCount <= 0) {
        curryNodeReleaseArguments(node);
    }
}

// Replaces a Curry node in-place. Note that the arguments
// are now SHARED among both nodes.
void curryNodeAssign(struct CurryNode *node, struct CurryNode *src) {
    assert(src != NULL);
    assert(src != node);
    curryNodeReleaseArguments(node);
    curryNodeRetainArguments(src);
    node->tag = src->tag;
    switch (src->tag) {
    case TAG_FUNCTION:
        {
            struct CurryFunction *srcFunction = &src->value.function;
            struct CurryFunction *function = &node->value.function;
            function->argumentCount = srcFunction->argumentCount;
            function->arguments = srcFunction->arguments;
            function->arity = srcFunction->arity;
            function->funcPtr = srcFunction->funcPtr;
        }
        break;
    case TAG_DATA:
        {
            struct CurryData *srcData = &src->value.data;
            struct CurryData *data = &node->value.data;
            data->argumentCount = srcData->argumentCount;
            data->arguments = srcData->arguments;
            data->arity = srcData->arity;
            data->constructor = srcData->constructor;
            data->type = srcData->type;
        }
        break;
    case TAG_CHOICE:
        {
            struct CurryChoice *srcChoice = &src->value.choice;
            struct CurryChoice *choice = &node->value.choice;
            choice->left = srcChoice->left;
            choice->right = srcChoice->right;
        }
        break;
    case TAG_INTEGER:
        node->value.integer = src->value.integer;
        break;
    case TAG_FLOATING:
        node->value.floating = src->value.floating;
        break;
    case TAG_CHARACTER:
        node->value.character = src->value.character;
        break;
    default:
        break;
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

// Evaluates a node to weak head normal form, i.e. so that it becomes
// a constructor, built-in function or partiallly applied function.
void curryNodeEvaluate(struct CurryNode *node) {
    // TODO: Handle nondeterminism & other tags:
    //
    //     Choice  -> Trigger pull-tabbing step
    //     Failure -> Throw an error/exit the program?
    //     Free    -> Instantiate to choice of shallow patterns, e.g.
    //
    //     head x where x free  ->  [] ? (y:ys) where y, ys free
    //
    // See section 5.2 in WFLP_19, ICurry (Antoy et al.)
    if (node->tag == TAG_FUNCTION) {
        struct CurryFunction *function = &node->value.function;
        assert(function->argumentCount <= function->arity);

        // Evaluate if fully applied
        if (function->argumentCount == function->arity) {
            struct CurryNode *evaluated = function->funcPtr(node);
            curryNodeAssign(node, evaluated);
        }
    }
}

// Fetches the nth child of a Curry node.
struct CurryNode *curryNodeAccess(struct CurryNode *node, uint8_t i) {
    switch (node->tag) {
    case TAG_FUNCTION:
        {
            struct CurryFunction *function = &node->value.function;
            assert(i < function->argumentCount);
            return function->arguments[i];
        }
    case TAG_DATA:
        {
            struct CurryData *data = &node->value.data;
            assert(i < data->argumentCount);
            return data->arguments[i];
        }
    case TAG_CHOICE:
        {
            assert(i < 2);
            struct CurryChoice *choice = &node->value.choice;
            switch (i) {
            case 0:
                return choice->left;
            case 1:
                return choice->right;
            default:
                fprintf(stderr, "Invalid choice index %d!\n", i);
                exit(1);
            }
        }
    default:
        fprintf(stderr, "Cannot get child of Curry node with tag %d!", i);
        exit(1);
    }
}

// Fetches a Curry data node's constructor.
uint64_t curryNodeGetConstructor(struct CurryNode *node) {
    assert(node->tag == TAG_DATA);
    return node->value.data.constructor;
}

// Fetches a Curry node's floating-point value.
double curryNodeGetFloating(struct CurryNode *node) {
    assert(node->tag == TAG_FLOATING);
    return node->value.floating;
}

// Fetches a Curry node's integer value.
uint64_t curryNodeGetInteger(struct CurryNode *node) {
    assert(node->tag == TAG_INTEGER);
    return node->value.integer;
}

// Fetches a Curry node's character value.
uint8_t curryNodeGetCharacter(struct CurryNode *node) {
    assert(node->tag == TAG_CHARACTER);
    return node->value.character;
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
    case TAG_FREE:
        printf("FREE");
        break;
    case TAG_FAILURE:
        printf("FAILURE");
        break;
    case TAG_CHOICE:
        printf("CHOICE");
        break;
    default:
        printf("UNKNOWN");
        break;
    }
    printf("\n");
}

// Terminates the program with a generic error message.
void curryExempt(void) {
    fprintf(stderr, "Exempt!\n");
    exit(1);
}
