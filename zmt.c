#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#include "zmt.h"

////////////////////////////////////////////////////////////////////////////////
// Chunks / Files //////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

chunk_t *new_chunk(uint8_t *data, size_t len) {
    chunk_t *chunk = (chunk_t *)calloc(1, sizeof(chunk_t));
    chunk->data = data;
    chunk->len = len;
    chunk->used = len;
    chunk->ref_count = 1;
    return chunk;
}

chunk_t *alloc_chunk(size_t len) {
    uint8_t *data = (uint8_t *)calloc(len, 1);
    chunk_t *chunk = new_chunk(data, len);
    chunk->used = 0;
    return chunk;
}

chunk_t *map_file(const char *path) {
    FILE *f = fopen(path, "r");
    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);

    uint8_t *data = (uint8_t *)mmap(NULL, size, PROT_READ,
            MAP_FILE | MAP_PRIVATE, fileno(f), 0);

    fclose(f);

    return new_chunk((uint8_t *)data, size);
}

////////////////////////////////////////////////////////////////////////////////
// Nodes ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

meta_tree_t *new_tree() {
    meta_tree_t *node = (meta_tree_t *)calloc(1, sizeof(meta_tree_t));
    node->ref_count = 1;
    return node;
}

meta_node_t *new_node() {
    meta_node_t *node = (meta_node_t *)calloc(1, sizeof(meta_node_t));
    node->flags = NODE_INNER;
    node->ref_count = 1;
    return node;
}

meta_node_t *new_leaf() {
    meta_node_t *node = new_node();
    node->flags = NODE_LEAF;
    return node;
}

////////////////////////////////////////////////////////////////////////////////
// Editing /////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Set any metadata from a chunk
// Only call this on a new node, as the tree nodes should be immutable
static void set_leaf_meta_data(meta_node_t *node) {
    assert(node->flags == NODE_LEAF);
    for (uint32_t i = node->leaf.start; i < node->leaf.end; i++)
        if (node->leaf.chunk_data[i] == '\n')
            node->nl_count++;
}

static void set_inner_meta_data(meta_node_t *node) {
    assert(node->flags == NODE_INNER);
    node->nl_count = 0;
    for (int32_t x = 0; x < MAX_CHILDREN; x++) {
        if (node->inner.children[x])
            node->nl_count += node->inner.children[x]->nl_count;
    }
}

meta_tree_t *read_data(chunk_t *chunk) {
    // Create tree
    meta_tree_t *tree = new_tree();
    tree->chunks[0] = chunk;
    tree->chunk_count = 1;

    // Create one leaf node with all data
    meta_node_t *node = new_leaf();
    node->leaf.start = 0;
    node->leaf.end = chunk->len;
    node->leaf.chunk_data = chunk->data;
    chunk->ref_count++;

    set_leaf_meta_data(node);

    tree->root = node;
    return tree;
}

// Read data, but create a big dumb tree with lots of nodes for it
meta_node_t *create_dumb_node(chunk_t *chunk, uint32_t start, uint32_t end) {
    if (end - start < 32) {
        meta_node_t *node = new_leaf();
        node->leaf.start = start;
        node->leaf.end = end;
        node->leaf.chunk_data = chunk->data;
        set_leaf_meta_data(node);
        chunk->ref_count++;
        return node;
    } else {
        meta_node_t *parent = new_node();
        uint32_t mid = (start + end) >> 1;
        parent->inner.children[0] = create_dumb_node(chunk, start, mid);
        parent->inner.children[1] = create_dumb_node(chunk, mid, end);
        set_inner_meta_data(parent);
        return parent;
    }
}

meta_tree_t *dumb_read_data(chunk_t *chunk) {
    // Create tree
    meta_tree_t *tree = new_tree();
    tree->chunks[0] = chunk;
    tree->chunk_count = 1;

    meta_node_t *node = create_dumb_node(chunk, 0, chunk->len);

    tree->root = node;
    return tree;
}

////////////////////////////////////////////////////////////////////////////////
// Iteration ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Initialize an iterator object, and return the first node. Does not allocate,
// it should be on the stack
void iter_start(meta_iter_t *iter, meta_tree_t *tree) {
    iter->tree = tree;
    iter->frame[0] = (meta_iter_frame_t) {
        .node = tree->root,
        .idx = 0,
        .state = ITER_START
    };
    iter->depth = 1;
    return iter_next(iter);
}

// Macros for the iterative tree walking, to simulate recursion

#define CALL(n)                                                       \
    do {                                                              \
        assert(iter->depth < ITER_STACK_SIZE);                        \
        iter->frame[iter->depth] = (meta_iter_frame_t) {              \
            .node = (n),                                              \
            .idx = 0,                                                 \
            .state = ITER_START                                       \
        };                                                            \
        iter->depth++;                                                \
        goto next_iter;                                               \
    } while (0)

#define RETURN()    do { iter->depth--; } while (0)

// Advance the iterator to the next leaf node
meta_node_t *iter_next(meta_iter_t *iter) {
    while (iter->depth > 0) {
        meta_iter_frame_t *frame = &iter->frame[iter->depth - 1];

        switch (frame->state) {
            case ITER_START:
                // Found leaf node?
                if (frame->node->flags == NODE_LEAF) {
                    RETURN();
                    return frame->node;
                }
                frame->state = ITER_CHILDREN;
            case ITER_CHILDREN:
                // Inner node. See if there's more children
                // Can recurse deeper?
                while (frame->idx < MAX_CHILDREN) {
                    meta_node_t *child = frame->node->inner
                        .children[frame->idx];
                    frame->idx++;
                    if (child)
                        CALL(child);
                }
                RETURN();
        }
next_iter:
        ;
    }

    return NULL;
}
