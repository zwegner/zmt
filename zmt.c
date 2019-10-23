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
    assert(node->flags & NODE_LEAF);
    node->byte_count = node->leaf.end - node->leaf.start;
    for (uint32_t i = node->leaf.start; i < node->leaf.end; i++)
        if (node->leaf.chunk_data[i] == '\n')
            node->nl_count++;
}

static void set_inner_meta_data(meta_node_t *node) {
    assert(node->flags & NODE_INNER);
    for (int32_t x = 0; x < MAX_CHILDREN; x++) {
        if (node->inner.children[x]) {
            node->byte_count += node->inner.children[x]->byte_count;
            node->nl_count += node->inner.children[x]->nl_count;
        }
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

// Initialize an iterator object. Does not allocate, it should be on the stack
void iter_init(meta_iter_t *iter, meta_tree_t *tree) {
    iter->tree = tree;
    iter->frame[0] = (meta_iter_frame_t) { .node = tree->root, .idx = 0,
        .state = ITER_START };
    iter->start_offset = (offset_t) { 0, 0 };
    iter->end_offset = (offset_t) { 0, 0 };
    iter->desired_offset = (offset_t) { 0, 0 };
    iter->depth = 1;
}

// Macros for the iterative tree walking, to simulate recursion

#define CALL(n, start_state)                                          \
    do {                                                              \
        assert(iter->depth < ITER_STACK_SIZE);                        \
        iter->frame[iter->depth] = (meta_iter_frame_t) {              \
            .node = (n),                                              \
            .idx = 0,                                                 \
            .state = (start_state)                                    \
        };                                                            \
        iter->depth++;                                                \
        goto next_iter;                                               \
    } while (0)

#define YIELD(node)   do { iter->depth--; return (node); } while (0)

#define RETURN()      do { iter->depth--; goto next_iter; } while (0)

// Advance the iterator to the next leaf node
meta_node_t *iter_next(meta_iter_t *iter) {
    while (iter->depth > 0) {
        meta_iter_frame_t *frame = &iter->frame[iter->depth - 1];
        meta_node_t *node = frame->node;

        switch (frame->state) {
            case ITER_START:
                // Found leaf node?
                if (node->flags & NODE_LEAF) {
                    iter->start_offset = iter->end_offset;
                    iter->end_offset.byte += node->byte_count;
                    iter->end_offset.line += node->nl_count;
                    YIELD(node);
                }
                frame->state = ITER_CHILDREN;

                // FALLTHROUGH
            case ITER_CHILDREN:
                // Inner node. See if there's more children
                while (frame->idx < MAX_CHILDREN) {
                    meta_node_t *child = node->inner.children[frame->idx];
                    frame->idx++;
                    if (!child)
                        break;
                    CALL(child, ITER_START);
                }
                RETURN();

            case ITER_JUMP:
                // Check for a leaf node. This is duplicated from above...
                if (node->flags & NODE_LEAF) {
                    iter->start_offset = iter->end_offset;
                    iter->end_offset.byte += node->byte_count;
                    iter->end_offset.line += node->nl_count;
                    YIELD(node);
                }

                // Reset the frame state to ITER_CHILDREN, since in JUMP mode
                // we only affect the first descent into the tree--all the
                // rest of the iteration is normal
                frame->state = ITER_CHILDREN;

                offset_t end = iter->start_offset;
                while (frame->idx < MAX_CHILDREN) {
                    meta_node_t *child = node->inner.children[frame->idx];
                    frame->idx++;
                    if (!child)
                        break;

                    // Check if the desired offset is within this subtree
                    end.byte += child->byte_count;
                    end.line += child->nl_count;

                    // XXX check line offset
                    if (end.byte > iter->desired_offset.byte)
                        CALL(child, ITER_JUMP);
                    else {
                        iter->start_offset = end;
                        iter->end_offset = end;
                    }
                }
                RETURN();
        }
next_iter:
        ;
    }

    return NULL;
}

// Initialize an iterator object, and return the first node
meta_node_t *iter_start(meta_iter_t *iter, meta_tree_t *tree,
        uint64_t byte_offset, uint64_t line_offset) {
    iter_init(iter, tree);

    if (byte_offset || line_offset) {
        // Use the ITER_JUMP machinery to skip all parts of the tree before
        // the desired offset
        iter->frame[0].state = ITER_JUMP;
        iter->desired_offset = (offset_t) { byte_offset, line_offset };

        meta_node_t *node = iter_next(iter);
        if (!node)
            return NULL;

        // XXX check line offset
        assert(iter->start_offset.byte <= byte_offset);
        assert(iter->end_offset.byte > byte_offset);

        // Exact match
        // XXX check line offset
        if (iter->start_offset.byte == byte_offset)
            return node;

        // The desired byte offset is somewhere in the middle. Rather than
        // try to make a weird API to pass this information to the caller,
        // create a dummy node with just the slice we want
        // XXX This is maybe dumb...? We don't get valid metadata for
        // nl_count, etc.
        uint64_t offset = byte_offset - iter->start_offset.byte;
        uint64_t len = iter->end_offset.byte - byte_offset;
        iter->dummy.flags = NODE_LEAF;
        iter->dummy.byte_count = len;
        iter->dummy.nl_count = -1;
        iter->dummy.leaf.start = node->leaf.start + offset;
        iter->dummy.leaf.end = node->leaf.end;
        iter->dummy.leaf.chunk_data = node->leaf.chunk_data;
        return &iter->dummy;
    }

    // Default case: just start the normal iteration
    return iter_next(iter);
}
