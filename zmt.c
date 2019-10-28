#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "zmt.h"

// XXX kinda dumb
chunk_t *current_chunk;

// Read-only singleton, since it can be used everywhere. static const should
// make this actually read only, with any writes causing a bus error.
// We do make non-const pointers to this though, so we have to be careful.
static const meta_node_t HOLE_NODE_SINGLETON = {
    .flags = NODE_HOLE,
    // We use non-zero byte/nl counts here, so that the hole takes up space in
    // the metadata tree, and can thus be jumped to.
    .byte_count = 1,
    .nl_count = 1,
    .leaf = {
        // This should never actually be returned from the iterator protocol,
        // so don't use any data
        .chunk_data = NULL,
        .start = 0,
        .end = 1,
    },
};

////////////////////////////////////////////////////////////////////////////////
// Chunks / Files //////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

chunk_t *create_chunk(uint8_t *data, size_t len) {
    chunk_t *chunk;
    // If no data pointer is passed in, allocate <len> bytes at the end of this
    // chunk and keep it embedded, to save an allocation.
    if (!data) {
        chunk = (chunk_t *)calloc(1, sizeof(chunk_t) + len);
        chunk->data = chunk->embedded_data;
        chunk->flags |= CHUNK_EMBEDDED;
    }
    // Otherwise, allocate just the minimal struct. We mark all bytes as being
    // used here by default.
    else {
        chunk = (chunk_t *)calloc(1, sizeof(chunk_t));
        chunk->data = data;
        chunk->used = len;
    }
    chunk->len = len;
    chunk->ref_count = 1;

    return chunk;
}

chunk_t *map_file(const char *path) {
    FILE *f = fopen(path, "r");
    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);

    uint8_t *data = (uint8_t *)mmap(NULL, size, PROT_READ,
            MAP_FILE | MAP_PRIVATE, fileno(f), 0);
    assert(data);

    fclose(f);

    return create_chunk((uint8_t *)data, size);
}

////////////////////////////////////////////////////////////////////////////////
// Nodes ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

meta_tree_t *create_tree() {
    meta_tree_t *tree = (meta_tree_t *)calloc(1, sizeof(meta_tree_t));
    tree->ref_count = 1;

    // Flag the embedded filler node
    tree->filler_node->flags = NODE_FILLER;
    return tree;
}

meta_node_t *create_node() {
    meta_node_t *node = (meta_node_t *)calloc(1, sizeof(meta_node_t));
    node->flags = NODE_INNER;
    node->ref_count = 1;
    return node;
}

meta_node_t *create_leaf() {
    meta_node_t *node = create_node();
    node->flags = NODE_LEAF;
    return node;
}

static meta_node_t *duplicate_node(meta_node_t *node) {
    meta_node_t *new_node = (meta_node_t *)malloc(sizeof(meta_node_t));
    *new_node = *node;
    return new_node;
}

////////////////////////////////////////////////////////////////////////////////
// Editing /////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Set any metadata from a chunk
// Only call this on a new node, as the tree nodes should be immutable
static void set_leaf_meta_data(meta_node_t *node) {
    assert(node->flags & (NODE_LEAF | NODE_FILLER));
    node->byte_count = node->leaf.end - node->leaf.start;
    node->nl_count = 0;
    for (uint32_t i = node->leaf.start; i < node->leaf.end; i++)
        if (node->leaf.chunk_data[i] == '\n')
            node->nl_count++;
}

static void set_inner_meta_data(meta_node_t *node) {
    assert(node->flags & NODE_INNER);
    node->byte_count = 0;
    node->nl_count = 0;
    for (int32_t x = 0; x < MAX_CHILDREN; x++) {
        if (node->inner.children[x]) {
            node->byte_count += node->inner.children[x]->byte_count;
            node->nl_count += node->inner.children[x]->nl_count;
        }
    }
}

void verify_node(meta_node_t *node) {
    uint32_t old_byte_count = node->byte_count;
    int32_t old_nl_count = node->nl_count;
    if (node->flags & (NODE_LEAF | NODE_FILLER))
        set_leaf_meta_data(node);
    else if (node->flags & NODE_HOLE)
        // XXX no-op
        ;
    else {
        for (int32_t x = 0; x < MAX_CHILDREN; x++)
            if (node->inner.children[x])
                verify_node(node->inner.children[x]);
        set_inner_meta_data(node);
    }
    assert(old_byte_count == node->byte_count);
    assert(old_nl_count == node->nl_count);
}

meta_tree_t *read_data(chunk_t *chunk) {
    // Create tree
    meta_tree_t *tree = create_tree();
    tree->chunks[0] = chunk;
    tree->chunk_count = 1;

    // Create one leaf node with all data
    meta_node_t *node = create_leaf();
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
        meta_node_t *node = create_leaf();
        node->leaf.start = start;
        node->leaf.end = end;
        node->leaf.chunk_data = chunk->data;
        set_leaf_meta_data(node);
        chunk->ref_count++;
        return node;
    } else {
        meta_node_t *parent = create_node();
        uint32_t s = start;
        for (int i = 0; i < MAX_CHILDREN; i++) {
            uint32_t mid = start + (end - start) * (i+1) / MAX_CHILDREN;
            parent->inner.children[i] = create_dumb_node(chunk, s, mid);
            s = mid;
        }
        set_inner_meta_data(parent);
        return parent;
    }
}

meta_tree_t *dumb_read_data(chunk_t *chunk) {
    // Create tree
    meta_tree_t *tree = create_tree();
    tree->chunks[0] = chunk;
    tree->chunk_count = 1;

    meta_node_t *node = create_dumb_node(chunk, 0, chunk->len);

    tree->root = node;
    return tree;
}

meta_node_t *make_slice_node(meta_iter_t *iter, meta_node_t *node,
        uint64_t offset) {
    *iter->slice_node = *node;
    iter->slice_node->leaf.start += offset;
    // Re-initialize metadata. Maybe we should have an option to not
    // recalculate this if it's not needed...?
    set_leaf_meta_data(iter->slice_node);
    return iter->slice_node;
}

// Create a new tree without a hole by replacing the hole node with
// the filler node
meta_tree_t *patch_tree_hole(meta_tree_t *tree) {
    assert(tree->has_hole);

    // Jump an iterator to the start of the hole
    meta_iter_t iter[1];
    meta_node_t *hole = iter_start(iter, tree, tree->hole_offset.byte, 0);
    assert(hole == tree->filler_node);
    assert(hole->flags & NODE_FILLER);

    // Replace the hole with the filler node
    meta_node_t *filler = duplicate_node(tree->filler_node);
    filler->flags = NODE_LEAF;
    tree = replace_current_node(iter, filler, false);
    tree->has_hole = false;

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

#define YIELD(node)                                                   \
    do {                                                              \
        iter->start_offset = iter->end_offset;                        \
        iter->end_offset.byte += (node)->byte_count;                  \
        iter->end_offset.line += (node)->nl_count;                    \
        iter->depth--;                                                \
        return (node);                                                \
    } while (0)

#define RETURN()      do { iter->depth--; goto next_iter; } while (0)

// Advance the iterator to the next leaf node
meta_node_t *iter_next(meta_iter_t *iter) {
    while (iter->depth > 0) {
        meta_iter_frame_t *frame = &iter->frame[iter->depth - 1];
        meta_node_t *node = frame->node;

        switch (frame->state) {
            case ITER_START:
                // Check for a leaf node
                if (node->flags & NODE_LEAF)
                    YIELD(node);

                // Check if this is a hole node. If so, yield the filler node
                if (node->flags & NODE_HOLE)
                    YIELD(iter->tree->filler_node);

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
                // Check for a leaf node or hole node, like ITER_START above
                if (node->flags & NODE_LEAF)
                    YIELD(node);
                if (node->flags & NODE_HOLE)
                    YIELD(iter->tree->filler_node);

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

                    if ((iter->desired_offset.byte &&
                            end.byte > iter->desired_offset.byte) ||
                        (iter->desired_offset.line &&
                             end.line >= iter->desired_offset.line))
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

// Initialize an iterator object, and return the first node, starting at
// either <byte_offset> bytes or <line_offset> lines into the file.
// Only one of byte_offset/line_offset can be non-zero.
meta_node_t *iter_start(meta_iter_t *iter, meta_tree_t *tree,
        uint64_t byte_offset, uint64_t line_offset) {
    iter_init(iter, tree);

    if (byte_offset || line_offset) {
        // Use the ITER_JUMP machinery to skip all parts of the tree before
        // the desired offset
        iter->frame[0].state = ITER_JUMP;
        iter->desired_offset = (offset_t) { byte_offset, line_offset };

        // See if we're jumping inside or past the hole. If so, we fake the offset
        // to the iterator jump, since the size of the hole is not updated in the
        // tree metadata.
        if (tree->has_hole) {
            // Subtract one from the actual size of the filler node, since the
            // hole takes up one byte/newline of space in the metadata tree.
            if (byte_offset > tree->hole_offset.byte)
                iter->desired_offset.byte -= tree->filler_node->byte_count - 1;
            if (line_offset > tree->hole_offset.line)
                iter->desired_offset.line -= tree->filler_node->nl_count - 1;
        }

        meta_node_t *node = iter_next(iter);
        if (!node)
            return NULL;

        uint64_t offset = 0;
        if (byte_offset) {
            assert(!line_offset);
            assert(iter->start_offset.byte <= byte_offset);
            assert(iter->end_offset.byte > byte_offset);

            // Exact match
            if (iter->start_offset.byte == byte_offset)
                return node;

            // Set offset to desired byte
            offset = byte_offset - iter->start_offset.byte;
        } else {
            assert(line_offset);
            assert(iter->start_offset.line <= line_offset);
            assert(iter->end_offset.line >= line_offset);

            // Exact match
            if (iter->start_offset.line == line_offset)
                return node;

            // Jump through newlines in this piece until we reach the
            // desired line
            uint64_t len = node->leaf.end - node->leaf.start;
            uint8_t *nl;
            const uint8_t *start = node->leaf.chunk_data + node->leaf.start;
            while (iter->start_offset.line < line_offset && offset < len &&
                    (nl = memchr(start, '\n', len - offset)) != NULL) {
                offset += nl + 1 - start;
                start = nl + 1;
                iter->start_offset.line++;
            }
        }

        // The desired byte offset is somewhere in the middle. Rather than
        // try to make a weird API to pass this information to the caller,
        // create a slice node with just the slice we want
        meta_node_t *slice = make_slice_node(iter, node, offset);

        // Update the offset. The byte is simple, but for newlines we use
        // a delta between the old and new nodes so we don't have to scan
        // both sides of the split.
        iter->start_offset.byte += offset;
        iter->start_offset.line += node->nl_count - slice->nl_count;

        return slice;
    }

    // Default case: just start the normal iteration
    return iter_next(iter);
}

// From inside an iterator, create a new tree with the current node replaced.
// This is a slightly awkward API perhaps, but we want to use most of the same
// iterator machinery to find a particular node, and keep the iterator stack
// around so we have the full path from root to leaf.
// This modifies the iterator in-place to continue iteration from the new
// tree.
meta_tree_t *replace_current_node(meta_iter_t *iter, meta_node_t *new_node,
        bool create_hole) {
    meta_tree_t *tree = create_tree();
    // Is the new node a hole node (or a parent of one)?
    if (create_hole) {
        // If there is already a hole in this tree, patch it up. We don't
        // bother if we're replacing the filler node.
        if (tree->has_hole &&
                !(iter->frame[iter->depth].node->flags & NODE_HOLE)) {
            // This is recursive, but can only go one deep
            // XXX This tree will be immediately replaced. Any optimization
            // possible here? Or just let gc take care of it?
            tree = patch_tree_hole(tree);
        }
        tree->has_hole = true;
        tree->hole_offset = iter->start_offset;
    }

    // Walk back up the stack, copying each parent node in succession
    for (uint32_t depth = iter->depth; depth > 0; depth--) {
        meta_iter_frame_t *frame = &iter->frame[depth - 1];
        meta_node_t *old_parent = frame->node;
        assert(frame->state == ITER_CHILDREN);
        assert(old_parent->flags & NODE_INNER);

        // Copy this node, replace the current child, and bump refcounts
        meta_node_t *new_parent = duplicate_node(old_parent);
        assert(frame->idx > 0);
        new_parent->inner.children[frame->idx - 1] = new_node;
        for (int32_t x = 0; x < MAX_CHILDREN; x++)
            if (new_parent->inner.children[x])
                new_parent->inner.children[x]->ref_count++;

        // Update metadata
        set_inner_meta_data(new_parent);

        // Replace this frame's current node, and keep it around to replace
        // in the parent frame
        frame->node = new_node = new_parent;
    }

    // XXX update chunk map, when we care about it...

    tree->root = new_node;
    iter->tree = tree;
    return tree;
}

// Like replace_current_node() above, but splits a node into two at the current
// iteration offset, and creates a hole between them. We take the current node
// as a parameter, since we don't generally store the just-yielded node inside
// the iterator, and we frequently jump into the middle of nodes and create
// slice nodes, so we can't easily derive it. We can compare the yielded node
// with the node at the top of the iteration tree, and thus find how far into
// the latter node we need to split.
meta_tree_t *split_current_node(meta_iter_t *iter, meta_node_t *node) {
    meta_node_t *parent = create_node();

    // There are two main cases to handle here, based on whether the jump
    // offset started perfectly on a node or not. Right now we can always
    // determine this based on whether we got the slice node from the iterator.
    if (node == iter->slice_node) {
        assert(iter->depth > 0);
        // Get the base node that the current node is a slice of
        // This is a tiny hack, in that it looks into the stack frame that
        // just got "popped", to see the node that is current
        meta_iter_frame_t *frame = &iter->frame[iter->depth];
        meta_node_t *base_node = frame->node;
        if (base_node->flags & NODE_HOLE) {
            assert(iter->tree->has_hole);
            base_node = iter->tree->filler_node;
        }

        // Sanity check that this is the right base node
        assert(node->flags & (NODE_LEAF | NODE_FILLER));
        assert(base_node->flags & (NODE_LEAF | NODE_FILLER));
        assert(node->leaf.chunk_data == base_node->leaf.chunk_data);
        assert(node->leaf.start > base_node->leaf.start);
        assert(node->leaf.end == base_node->leaf.end);

        // Create two nodes: one with the bytes to the right of the current
        // offset, which we can copy directly from the current node, and one
        // with the bytes to the left, which we create by comparing the
        // current node with its base
        meta_node_t *child_r = duplicate_node(node);
        meta_node_t *child_l = duplicate_node(base_node);
        child_l->leaf.end = child_r->leaf.start;
        child_l->byte_count -= child_r->byte_count;
        child_l->nl_count -= child_r->nl_count;

        // XXX This logic is MAX_CHILDREN==2 specific
        meta_node_t *sub_parent = create_node();
        sub_parent->inner.children[0] = child_l;
        sub_parent->inner.children[1] = (meta_node_t *)&HOLE_NODE_SINGLETON;
        set_inner_meta_data(sub_parent);

        parent->inner.children[0] = sub_parent;
        parent->inner.children[1] = child_r;
        set_inner_meta_data(parent);
    }
    // Not the slice node: we are making a hole at the boundary between
    // two nodes. This case is a lot simpler, we just create one parent
    // with the hole on the left and the current node on the right.
    else {
        assert(node->flags & (NODE_LEAF | NODE_FILLER));
        // XXX This logic is MAX_CHILDREN==2 specific
        parent->inner.children[0] = (meta_node_t *)&HOLE_NODE_SINGLETON;
        parent->inner.children[1] = node;
        set_inner_meta_data(parent);
    }

    return replace_current_node(iter, parent, true);
}

// Helper function: split the tree at the given offset, and insert the
// given bytes into the filler node
meta_tree_t *insert_bytes_at_offset(meta_tree_t *tree, uint64_t offset,
        const uint8_t *data, uint64_t len) {
    // Copy data into chunk
    // XXX chunk management
    if (!current_chunk)
        current_chunk = create_chunk(NULL, 0x10000);
    assert(current_chunk->used + len < current_chunk->len);
    memcpy((uint8_t *)&current_chunk->data[current_chunk->used], data, len);

    // XXX this is suboptimal, we should check for appending to the
    // filler node
    meta_iter_t iter[1];
    meta_node_t *old_node = iter_start(iter, tree, offset, 0);
    tree = split_current_node(iter, old_node);

    meta_node_t *new_node = tree->filler_node;
    new_node->leaf.chunk_data = current_chunk->data;
    new_node->leaf.start = current_chunk->used;
    new_node->leaf.end = current_chunk->used + len;
    set_leaf_meta_data(new_node);
    current_chunk->ref_count++;

    return tree;
}

////////////////////////////////////////////////////////////////////////////////
// Tree sitter integration /////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const char *read_chunk(void *payload, uint32_t byte_offset, TSPoint position,
        uint32_t *bytes_read) {
    meta_iter_t *iter = (meta_iter_t *)payload;
    meta_node_t *node;
    // Tree-sitter apparently likes to backtrack a bunch in the parser, so
    // detect when they ask for a byte offset that isn't where we last left
    // off, and jump to that place in the tree
    if (byte_offset != iter->end_offset.byte)
        node = iter_start(iter, iter->tree, byte_offset, 0);
    else
        node = iter_next(iter);

    if (!node) {
        *bytes_read = 0;
        return NULL;
    }

    // Set number of bytes read, and return a pointer to the raw data
    *bytes_read = node->leaf.end - node->leaf.start;
    return (const char *)(node->leaf.chunk_data + node->leaf.start);
}

// Parse a given source file as C
TSTree *parse_c_tree(meta_tree_t *tree) {
    TSParser *parser = ts_parser_new();
    ts_parser_set_language(parser, tree_sitter_c());

    meta_iter_t iter[1];
    iter_init(iter, tree);

    TSInput input = {
        .payload = (void *)iter,
        .read = read_chunk,
        .encoding = TSInputEncodingUTF8
    };
    return ts_parser_parse(parser, NULL, input);
}
