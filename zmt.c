#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "zmt.h"

// Local prototypes
static const uint8_t *write_data_bytes(const uint8_t *data, uint64_t len,
        uint32_t *start, uint32_t *end);
static meta_tree_t *duplicate_tree(meta_tree_t *tree);
static meta_node_t *duplicate_node(meta_node_t *node);
static void set_leaf_meta_data(meta_node_t *node);
static void set_inner_meta_data(meta_node_t *node);
static meta_node_t *iter_slice_at(meta_iter_t *iter, meta_node_t *node,
        uint64_t line_offset, uint64_t byte_offset, bool forwards);
static meta_tree_t *replace_current_node(meta_iter_t *iter,
        meta_node_t *new_node);

// C -> Lua callbacks
void (*print_node)(meta_node_t *node);
void (*print_tree)(meta_tree_t *tree);
void (*print_iter)(meta_iter_t *iter);

// XXX kinda dumb
chunk_t *current_chunk;

// Read-only singleton, since it can be used everywhere. static const should
// make this actually read only, with any writes causing a bus error.
// We do make non-const pointers to this though, so we have to be careful.
static const meta_node_t HOLE_NODE_SINGLETON = {
    .flags = NODE_HOLE | NODE_HAS_HOLE,
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


// Helper function. This always writes to the same memory, so isn't safe to be
// called multiple times before using the values.
char *ptr_string(void *ptr) {
    static char value[19];
    sprintf(value, "0x%016llX", (uint64_t)ptr);
    return value;
}

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

static const uint8_t *write_data_bytes(const uint8_t *data, uint64_t len,
        uint32_t *start, uint32_t *end) {
    // XXX chunk management
    if (!current_chunk)
        current_chunk = create_chunk(NULL, 0x10000);
    assert(current_chunk->used + len < current_chunk->len);

    memcpy((uint8_t *)&current_chunk->data[current_chunk->used], data, len);

    *start = current_chunk->used;
    *end = current_chunk->used + len;

    current_chunk->used += len;

    // XXX what constitutes a ref? should probably be per-tree not per-node
    current_chunk->ref_count++;

    return current_chunk->data;
}

// Dumb chunk management stuff: just leak the old chunk
chunk_t *write_new_chunk(const uint8_t *data, uint64_t len) {
    uint32_t dummy;
    current_chunk = create_chunk(NULL, 0x10000);
    write_data_bytes(data, len, &dummy, &dummy);
    return current_chunk;
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
// Trees/Nodes /////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Allocate a new tree structure. If make_root is true, we allocate an empty
// leaf node as the root.
meta_tree_t *create_tree(bool make_root) {
    meta_tree_t *tree = (meta_tree_t *)calloc(1, sizeof(meta_tree_t));
    tree->ref_count = 1;

    if (make_root) {
        // Set the tree root as the hole node. This is both so that trees always
        // have a valid state that can be iterated over, even with zero bytes,
        // and also so that we don't need an extra allocation, because the hole
        // node is a singleton, and the filler node is embedded in the tree.
        tree->root = (meta_node_t *)&HOLE_NODE_SINGLETON;
        tree->has_hole = true;
    }

    // Flag the embedded filler node
    tree->filler_node->flags = NODE_FILLER;
    return tree;
}

static meta_tree_t *duplicate_tree(meta_tree_t *tree) {
    meta_tree_t *new_tree = (meta_tree_t *)malloc(sizeof(meta_tree_t));
    *new_tree = *tree;
    return new_tree;
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
    node->flags &= ~NODE_HAS_HOLE;
    node->byte_count = 0;
    node->nl_count = 0;
    for (int32_t x = 0; x < MAX_CHILDREN; x++) {
        meta_node_t *child = node->inner.children[x];
        if (child) {
            node->byte_count += child->byte_count;
            node->nl_count += child->nl_count;
            node->flags |= child->flags & NODE_HAS_HOLE;
        }
    }
}

void verify_node(meta_node_t *node) {
    uint32_t old_flags = node->flags;
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
    assert(old_flags == node->flags);
    assert(old_byte_count == node->byte_count);
    assert(old_nl_count == node->nl_count);
}

meta_tree_t *read_data(chunk_t *chunk) {
    // Create tree
    meta_tree_t *tree = create_tree(false);
    tree->chunks[0] = chunk;
    tree->chunk_count = 1;

    // Fill in root node with all data
    tree->root = create_leaf();
    tree->root->leaf.start = 0;
    tree->root->leaf.end = chunk->len;
    tree->root->leaf.chunk_data = chunk->data;
    chunk->ref_count++;

    set_leaf_meta_data(tree->root);

    return tree;
}

// Read data, but create a big dumb tree with lots of nodes for it
meta_node_t *create_dumb_node(chunk_t *chunk, uint32_t size, uint32_t start,
        uint32_t end) {
    if (end - start <= size) {
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
            parent->inner.children[i] = create_dumb_node(chunk, size, s, mid);
            s = mid;
        }
        set_inner_meta_data(parent);
        return parent;
    }
}

meta_tree_t *dumb_read_data(chunk_t *chunk, uint32_t size) {
    // Create tree
    meta_tree_t *tree = create_tree(false);
    tree->chunks[0] = chunk;
    tree->chunk_count = 1;

    // Set a default size if none given
    size = size ? size : 256;

    meta_node_t *node = create_dumb_node(chunk, size, 0, chunk->used);

    tree->root = node;
    return tree;
}

offset_t get_tree_total_size(meta_tree_t *tree) {
    offset_t off = {
        .line = tree->root->nl_count,
        .byte = tree->root->byte_count
    };
    if (tree->has_hole) {
        off.line += (int64_t)tree->filler_node->nl_count - 1;
        off.byte += (int64_t)tree->filler_node->byte_count - 1;
    }
    return off;
}

// XXX this is dumb
uint64_t get_tree_line_length(meta_tree_t *tree, uint64_t line) {
    meta_iter_t iter[1];
    (void)iter_start_at(iter, tree, line, 0, true);
    uint64_t start_offset = iter->start_offset.byte;
    (void)iter_start_at(iter, tree, line + 1, 0, true);
    return iter->start_offset.byte - start_offset;
}

uint64_t get_abs_byte_offset(meta_tree_t *tree, uint64_t line, uint64_t byte) {
    meta_iter_t iter[1];
    (void)iter_start_at(iter, tree, line, 0, true);
    return iter->start_offset.byte + byte;
}

////////////////////////////////////////////////////////////////////////////////
// Iteration ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Include iter.c with both forwards and backwards specialization

#define DIRECTION   (1)
#include "iter.c"
#define DIRECTION   (-1)
#include "iter.c"

// Initialize an iterator object, and return the first node, starting at
// either <byte_offset> bytes or <line_offset> lines into the file.
// Only one of byte_offset/line_offset can be non-zero.
meta_node_t *iter_start_at(meta_iter_t *iter, meta_tree_t *tree,
        uint64_t line_offset, uint64_t byte_offset, bool forwards) {
    // If line_offset and byte_offset are both non-zero, that needs to be
    // handled specially. Right now this needs to be done with two jumps,
    // since the actual jump destination could be arbitrarily far in the
    // tree from the start of the line. Maybe this could be optimized a bit...
    if (line_offset > 0 && byte_offset > 0) {
        (void)iter_start_at(iter, tree, line_offset, 0, forwards);
        line_offset = 0;
        byte_offset = iter->start_offset.byte + byte_offset;
    }

    if (forwards)
        iter_init(iter, tree, ITER_JUMP);
    else
        iter_init_backwards(iter, tree, ITER_JUMP);

    // Use the ITER_JUMP machinery to skip all parts of the tree before
    // the desired offset
    iter->desired_offset = (offset_t) { line_offset, byte_offset };

    meta_node_t *node;
    // Handle the rootless tree special case here to keep iter_next() simple
    if (!tree->root) {
        node = NULL;
        iter->depth--;
    } else if (forwards)
        node = iter_next(iter);
    else
        node = iter_prev(iter);

    return iter_slice_at(iter, node, line_offset, byte_offset, forwards);
}

// Slice a node in half at a given offset. This fills the slice_node_left/right
// entries in the iterator
static meta_node_t *iter_slice_at(meta_iter_t *iter, meta_node_t *node,
        uint64_t line_offset, uint64_t byte_offset, bool forwards) {
    if (!node)
        return NULL;

    uint64_t offset = 0;
    if (!line_offset) {
        if (forwards) {
            assert(byte_offset >= iter->start_offset.byte);
            assert(byte_offset < iter->end_offset.byte ||
                (byte_offset == iter->end_offset.byte &&
                 byte_offset == iter->start_offset.byte));

            // Exact match
            if (iter->start_offset.byte == byte_offset)
                return node;
        } else {
            assert(byte_offset >= iter->start_offset.byte);
            assert(byte_offset <= iter->end_offset.byte);

            // Exact match
            if (byte_offset == iter->end_offset.byte)
                return node;
        }

        // Set offset to desired byte
        offset = byte_offset - iter->start_offset.byte;
    } else {
        if (forwards) {
            assert(!byte_offset);
            assert(line_offset >= iter->start_offset.line);
            assert(line_offset <= iter->end_offset.line);

            // Exact match
            if (iter->start_offset.line == line_offset)
                return node;
        } else
            assert(!"backwards line slicing not handled yet");

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
    meta_node_t *slice_right = iter->slice_node_right;
    *slice_right = *node;
    slice_right->leaf.start += offset;
    set_leaf_meta_data(slice_right);

    meta_node_t *slice_left = iter->slice_node_left;
    *slice_left = *node;
    slice_left->leaf.end = slice_right->leaf.start;
    slice_left->byte_count -= slice_right->byte_count;
    slice_left->nl_count -= slice_right->nl_count;

    // Update the iteration offsets as necessary given the direction, and
    // return either the left or right slice node.
    if (forwards) {
        iter->start_offset.byte += offset;
        if (line_offset == 0)
            iter->start_offset.line += slice_left->nl_count;

        return slice_right;
    } else {
        // Update the offset. The byte is simple, but for newlines we use
        // a delta between the old and new nodes so we don't have to scan
        // both sides of the split.
        iter->end_offset.byte = iter->start_offset.byte + offset;
        if (line_offset == 0)
            iter->end_offset.line -= slice_right->nl_count;

        return slice_left;
    }
}

////////////////////////////////////////////////////////////////////////////////
// Mutation ////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// From inside an iterator, create a new tree with the current node replaced.
// This is a slightly awkward API perhaps, but we want to use most of the same
// iterator machinery to find a particular node, and keep the iterator stack
// around so we have the full path from root to leaf.
// This modifies the iterator in-place to continue iteration from the new
// tree.
static meta_tree_t *replace_current_node(meta_iter_t *iter,
        meta_node_t *new_node) {
    meta_tree_t *tree = duplicate_tree(iter->tree);

    // Update the hole position if necessary
    // XXX is replaced_node ever actually null? What should we do then?
    meta_node_t *replaced_node = iter->frame[iter->depth].node;
    if (replaced_node && iter->start_offset.byte < tree->hole_offset.byte) {
        offset_t new_size = { 0, 0 };
        if (new_node) {
            new_size.byte = new_node->byte_count;
            new_size.line = new_node->nl_count;
        }

        tree->hole_offset.byte += new_size.byte - replaced_node->byte_count;
        tree->hole_offset.line += new_size.line - replaced_node->nl_count;
    }

    // Walk back up the stack, copying each parent node in succession
    for (uint32_t depth = iter->depth; depth > 0; depth--) {
        meta_iter_frame_t *frame = &iter->frame[depth - 1];
        meta_node_t *old_parent = frame->node;
        assert(old_parent->flags & NODE_INNER);

        // Copy this node, replace the current child, and bump refcounts
        meta_node_t *new_parent = duplicate_node(old_parent);
        assert(frame->idx >= 0 && frame->idx < MAX_CHILDREN);
        new_parent->inner.children[frame->idx] = new_node;
        for (int32_t x = 0; x < MAX_CHILDREN; x++) {
            meta_node_t *child = new_parent->inner.children[x];
            if (child && !(child->flags & NODE_HOLE))
                child->ref_count++;
        }

        // Update metadata
        set_inner_meta_data(new_parent);

        // Replace this frame's current node, and keep it around to replace
        // in the parent frame
        frame->node = new_node = new_parent;
    }

    // XXX update chunk map, when we care about it...

    // Replace the tree root with the new parent that bubbled up here
    tree->root = new_node;

    if (new_node)
        tree->has_hole = (new_node->flags & NODE_HAS_HOLE);

    iter->tree = tree;
    return tree;
}

// Create a new tree without a hole by replacing the hole node with
// the filler node
meta_tree_t *patch_tree_hole(meta_tree_t *tree) {
    assert(tree->has_hole);

    // First, find the hole in the tree
    meta_iter_t iter[1];
    iter_init(iter, tree, ITER_HOLES);
    meta_node_t *hole = iter_next(iter);
    assert(hole == &HOLE_NODE_SINGLETON);
    assert(hole->flags & NODE_HOLE);

    // Replace the hole with the filler node if the filler node has
    // non-zero size, otherwise delete that branch
    meta_node_t *old_filler = tree->filler_node;
    meta_node_t *new_filler = NULL;
    if (old_filler->leaf.chunk_data &&
            old_filler->leaf.end > old_filler->leaf.start) {
        new_filler = duplicate_node(old_filler);
        new_filler->flags = NODE_LEAF;
    }
    meta_tree_t *new_tree = replace_current_node(iter, new_filler);
    new_tree->has_hole = false;

    return new_tree;
}

// Create a new hole at the given offset of the tree, and return the resulting
// tree
meta_tree_t *split_at_offset(meta_tree_t *tree, uint64_t line_offset,
        uint64_t byte_offset) {
    // Patch the existing hole if there's already one
    if (tree->has_hole)
        tree = patch_tree_hole(tree);

    meta_iter_t iter[1];
    meta_node_t *node = iter_start_at(iter, tree, line_offset, byte_offset,
            true);

    assert(!node || node == iter->slice_node_right ||
            node == iter->frame[iter->depth].node);

    // There are two main cases to handle here, based on whether the jump
    // offset started perfectly on a node or not. Right now we can always
    // determine this based on whether we got the slice node from the iterator.
    meta_node_t *parent = create_node();
    if (node == iter->slice_node_right) {
        meta_node_t *left = iter->slice_node_left;

        // Sanity check that this is the right base node
        assert(node->flags & NODE_LEAF);
        assert(left->flags & NODE_LEAF);
        assert(node->leaf.chunk_data == left->leaf.chunk_data);
        assert(node->leaf.start == left->leaf.end);

        // Duplicate the left and right slice nodes
        meta_node_t *child_r = duplicate_node(node);
        meta_node_t *child_l = duplicate_node(left);

        // XXX This logic is MAX_CHILDREN==2 specific
        meta_node_t *sub_parent = create_node();
        sub_parent->inner.children[0] = child_l;
        sub_parent->inner.children[1] = (meta_node_t *)&HOLE_NODE_SINGLETON;
        set_inner_meta_data(sub_parent);

        parent->inner.children[0] = sub_parent;
        parent->inner.children[1] = child_r;
        set_inner_meta_data(parent);
    }
    // No node: this means we're appending the hole at the end.
    else if (!node) {
        assert(iter->depth == 0);
        // XXX This logic is MAX_CHILDREN==2 specific
        parent->inner.children[0] = iter->tree->root;
        parent->inner.children[1] = (meta_node_t *)&HOLE_NODE_SINGLETON;
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

    tree = replace_current_node(iter, parent);

    tree->has_hole = true;
    tree->hole_offset = iter->start_offset;

    // Mark the filler node of the new tree as fresh
    tree->filler_node->leaf.chunk_data = NULL;
    tree->filler_node->leaf.start = 0;
    tree->filler_node->leaf.end = 0;
    tree->filler_node->byte_count = 0;
    tree->filler_node->nl_count = 0;

    return tree;
}

// Add some bytes to the end of the filler node. This can generally be done
// quickly if the filler node is pointing at a chunk with space at the end.
meta_tree_t *append_bytes_to_filler(meta_tree_t *tree, const uint8_t *data,
    uint64_t len) {
    assert(tree->has_hole);
    tree = duplicate_tree(tree);

    meta_node_t *filler = tree->filler_node;

    const uint8_t *chunk_data;
    uint32_t start, end;
    chunk_data = write_data_bytes(data, len, &start, &end);

    if (!filler->leaf.chunk_data) {
        filler->leaf.chunk_data = chunk_data;
        filler->leaf.start = start;
        filler->leaf.end = end;
    } else {
        assert(chunk_data == filler->leaf.chunk_data);
        assert(start == filler->leaf.end);
        filler->leaf.end = end;
    }

    set_leaf_meta_data(filler);

    return tree;
}

// Helper function: split the tree at the given offset, and insert the
// given bytes into the filler node
meta_tree_t *insert_bytes_at_offset(meta_tree_t *tree, uint64_t line_offset,
        uint64_t byte_offset, const uint8_t *data, uint64_t len) {
    tree = split_at_offset(tree, line_offset, byte_offset);
    return append_bytes_to_filler(tree, data, len);
}

meta_tree_t *delete_byte_range(meta_tree_t *tree, uint64_t start,
        uint64_t end) {
    assert(start < end);

    meta_iter_t iter[1];
    meta_node_t *node = iter_start_at(iter, tree, 0, start, true);

    assert(node == iter->slice_node_right ||
            node == iter->frame[iter->depth].node ||
            node == tree->filler_node);

    meta_node_t *left = NULL;

    // Delete leftmost part of this byte range, if we're deleting only part of
    // a node
    if (node == iter->slice_node_right) {
        left = iter->slice_node_left;

        // Sanity check that this is the right base node
        assert(node->flags & (NODE_LEAF | NODE_FILLER));
        assert(left->flags & (NODE_LEAF | NODE_FILLER));
        assert(node->leaf.chunk_data == left->leaf.chunk_data);
        assert(node->leaf.start == left->leaf.end);

        left = duplicate_node(left);
    }

    // Delete the middle part
    while (node && iter->end_offset.byte <= end) {
        tree = replace_current_node(iter, NULL);
        node = iter_next(iter);
    }

    // Delete the right part
    meta_node_t *right = NULL;
    if (node) {
        right = iter_slice_at(iter, node, 0, end, true);
        right = duplicate_node(right);
    }

    meta_node_t *parent = NULL;
    if (left && right) {
        // XXX This logic is MAX_CHILDREN==2 specific
        parent = create_node();
        parent->inner.children[0] = left;
        parent->inner.children[1] = right;
        set_inner_meta_data(parent);
    } else
        parent = left ? left : right;

    tree = replace_current_node(iter, parent);

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
        node = iter_start_at(iter, iter->tree, 0, byte_offset, true);
    else
        node = iter_next(iter);

    if (!node) {
        *bytes_read = 0;
        return NULL;
    }
    assert(node->flags & (NODE_LEAF | NODE_FILLER));

    // Set number of bytes read, and return a pointer to the raw data
    *bytes_read = node->leaf.end - node->leaf.start;
    return (const char *)(node->leaf.chunk_data + node->leaf.start);
}

// Parse a given source file as C
TSTree *parse_c_tree(meta_tree_t *tree) {
    TSParser *parser = ts_parser_new();
    ts_parser_set_language(parser, tree_sitter_c());

    meta_iter_t iter[1];
    iter_init(iter, tree, ITER_START);

    TSInput input = {
        .payload = (void *)iter,
        .read = read_chunk,
        .encoding = TSInputEncodingUTF8
    };
    return ts_parser_parse(parser, NULL, input);
}
