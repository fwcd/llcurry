/*
 * The implementation of the Curry runtime.
 */

#include "runtime.h"

struct CurryNode *curryNodeAllocate(uint8_t tag) {
    struct CurryNode *node = malloc(sizeof(struct CurryNode));
    node->tag = tag;
    node->refCount = 1;
    return node;
}

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

struct CurryNode *curryNodeNewInteger(uint64_t integer) {
    struct CurryNode *node = curryNodeAllocate(TAG_INTEGER);
    node->value.integer = integer;
    return node;
}

struct CurryNode *curryNodeNewFloating(double floating) {
    struct CurryNode *node = curryNodeAllocate(TAG_FLOATING);
    node->value.floating = floating;
    return node;
}

struct CurryNode *curryNodeNewPlaceholder(void) {
    return curryNodeAllocate(TAG_PLACEHOLDER);
}

struct CurryNode *curryNodeNewFree(void) {
    return curryNodeAllocate(TAG_FREE);
}

struct CurryNode *curryNodeNewFailure(void) {
    return curryNodeAllocate(TAG_FAILURE);
}

struct CurryNode *curryNodeNewCharacter(uint8_t character) {
    struct CurryNode *node = curryNodeAllocate(TAG_CHARACTER);
    node->value.character = character;
    return node;
}

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

void curryNodeRelease(struct CurryNode *node) {
    node->refCount--;
    if (node->refCount <= 0) {
        curryNodeReleaseArguments(node);
    }
}

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

void curryNodeDataApply(struct CurryNode *node, struct CurryNode *argument) {
    assert(node->tag == TAG_DATA);
    struct CurryData *data = &node->value.data;
    assert(data->argumentCount < data->arity);
    data->arguments[data->argumentCount] = argument;
    data->argumentCount++;
    curryNodeRetain(argument);
}

void curryNodeFunctionApply(struct CurryNode *node, struct CurryNode *argument) {
    assert(node->tag == TAG_FUNCTION);
    struct CurryFunction *function = &node->value.function;
    assert(function->argumentCount < function->arity);
    function->arguments[function->argumentCount] = argument;
    function->argumentCount++;
    curryNodeRetain(argument);
}

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

uint64_t curryNodeGetConstructor(struct CurryNode *node) {
    assert(node->tag == TAG_DATA);
    return node->value.data.constructor;
}

double curryNodeGetFloating(struct CurryNode *node) {
    assert(node->tag == TAG_FLOATING);
    return node->value.floating;
}

uint64_t curryNodeGetInteger(struct CurryNode *node) {
    assert(node->tag == TAG_INTEGER);
    return node->value.integer;
}

uint8_t curryNodeGetCharacter(struct CurryNode *node) {
    assert(node->tag == TAG_CHARACTER);
    return node->value.character;
}

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

void curryExempt(void) {
    fprintf(stderr, "Exempt!\n");
    exit(1);
}
