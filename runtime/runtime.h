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

#ifndef LLCURRY_RUNTIME_H
#define LLCURRY_RUNTIME_H

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

// Allocates a new Curry node. This function is internal to the runtime
// and should not be called by generated code.
struct CurryNode *curryNodeAllocate(uint8_t tag);

// Creates a new data constructor with no arguments applied to it.
struct CurryNode *curryNodeNewData(uint8_t arity, uint64_t type, uint64_t constructor);

// Creates a new function with no arguments applied to it.
struct CurryNode *curryNodeNewFunction(uint8_t arity, struct CurryNode *(*funcPtr)(struct CurryNode *));

// Creates a new choice node from the given two nodes.
struct CurryNode *curryNodeNewChoice(struct CurryNode *left, struct CurryNode *right);

// Creates a new node holding a 64-bit integer.
struct CurryNode *curryNodeNewInteger(uint64_t integer);

// Creates a new node holding a single floating point number.
struct CurryNode *curryNodeNewFloating(double floating);

// Creates a new placeholder node.
struct CurryNode *curryNodeNewPlaceholder(void);

// Creates a new free node.
struct CurryNode *curryNodeNewFree(void);

// Creates a new failure node.
struct CurryNode *curryNodeNewFailure(void);

// Creates a new node holding an 8-bit character.
struct CurryNode *curryNodeNewCharacter(uint8_t character);

// Increments a Curry node's reference count.
void curryNodeRetain(struct CurryNode *node);

// Decrements a Curry node's reference count. Once the reference count
// drops to zero, this function will also _deallocate_ the node, which
// means that every further access is undefined behavior.
void curryNodeRelease(struct CurryNode *node);

// Replaces a Curry node in-place. Note that the arguments
// are now SHARED among both nodes.
void curryNodeAssign(struct CurryNode *node, struct CurryNode *src);

// Applies an argument to an unsatisfied data/constructor node.
void curryNodeDataApply(struct CurryNode *node, struct CurryNode *argument);

// Applies an argument to a function node. Note that this does NOT
// automatically evaluate the result once fully applied.
void curryNodeFunctionApply(struct CurryNode *node, struct CurryNode *argument);

// Evaluates a node to weak head normal form, i.e. so that it becomes
// a constructor, built-in function or partiallly applied function.
void curryNodeEvaluate(struct CurryNode *node);

// Fetches the nth child of a Curry node.
struct CurryNode *curryNodeAccess(struct CurryNode *node, uint8_t i);

// Fetches a Curry data node's constructor.
uint64_t curryNodeGetConstructor(struct CurryNode *node);

// Fetches a Curry node's floating-point value.
double curryNodeGetFloating(struct CurryNode *node);

// Fetches a Curry node's integer value.
uint64_t curryNodeGetInteger(struct CurryNode *node);

// Fetches a Curry node's character value.
uint8_t curryNodeGetCharacter(struct CurryNode *node);

// Prints a Curry node to stdout for debugging purposes.
void curryNodePrint(struct CurryNode *node);

// Terminates the program with a generic error message.
void curryExempt(void);

#endif // LLCURRY_RUNTIME_H
