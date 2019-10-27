#include <ncurses.h>
#include <stdint.h>

#include "tree_sitter/api.h"

// Flags
enum {
    NODE_INNER = 1,
    NODE_LEAF = 2
};

#define MAX_CHILDREN            (2)
#define DUMB_MAX_CHUNKS         (16)
#define ITER_STACK_SIZE         (32)

// chunk_t holds big chunks of contiguous backing memory
// XXX make variant with embedded data for non-mmap()ed memory, so we only
// need one allocation per chunk
typedef struct {
    uint32_t ref_count;
    const uint8_t *data;
    size_t len;
    size_t used;
} chunk_t;

// Main tree node for storing buffer state. We use one node type with a union,
// so we can use the same pointer types everywhere
typedef struct meta_node_t {
    uint32_t ref_count;
    uint32_t flags;
    // XXX This size is inadequate
    uint32_t byte_count;
    int32_t nl_count;
    union {
        struct {
            struct meta_node_t *children[MAX_CHILDREN];
        } inner;
        struct {
            uint32_t start;
            uint32_t end;
            // Store a pointer to the raw data, rather than the chunk_t.
            // This is just for efficiency, since we don't want an extra
            // level of indirection for editing. We keep a map of chunks
            // used within each tree in meta_tree_t so we can access the
            // metadata if we need it.
            const uint8_t *chunk_data;
        } leaf;
    };
} meta_node_t;

// Root structure for storing one snapshot of a buffer state, with any extra
// data needed. We store a list of chunks referenced in the tree
typedef struct {
    uint32_t ref_count;
    meta_node_t *root;
    // XXX actual map of chunk data->chunk
    chunk_t *chunks[DUMB_MAX_CHUNKS];
    uint32_t chunk_count;
} meta_tree_t;

////////////////////////////////////////////////////////////////////////////////
// Iterator protocol ///////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// State machine for one frame's iterative tree walking
enum ITER_STATE {
    ITER_START,
    ITER_CHILDREN,
    ITER_JUMP,
};

typedef struct {
    // XXX make these 32-bit for now for luajit compat. This size is inadequate!
    uint32_t byte;
    uint32_t line;
} offset_t;

// Stack frame for iterative tree walking
typedef struct {
    meta_node_t *node;
    int8_t idx;
    enum ITER_STATE state;
} meta_iter_frame_t;

// Main iterator structure
typedef struct {
    meta_tree_t *tree;

    // Tree walking stack and depth counter
    meta_iter_frame_t frame[ITER_STACK_SIZE];
    int32_t depth;

    offset_t start_offset;
    offset_t end_offset;

    offset_t desired_offset;

    meta_node_t dummy;
} meta_iter_t;

// Prototypes
chunk_t *map_file(const char *path);
meta_tree_t *read_data(chunk_t *chunk);
void verify_node(meta_node_t *node);

// Iteration
void iter_init(meta_iter_t *iter, meta_tree_t *tree);
meta_node_t *iter_start(meta_iter_t *iter, meta_tree_t *tree,
        uint64_t byte_offset, uint64_t line_offset);
meta_node_t *iter_next(meta_iter_t *iter);

// Mutation
meta_tree_t *replace_current_node(meta_iter_t *iter, meta_node_t *new_node);

// XXX remove this
meta_tree_t *mangle_tree(meta_tree_t *tree, uint64_t offset,
        const uint8_t *data, uint64_t len);

// XXX remove this
meta_tree_t *dumb_read_data(chunk_t *chunk);

// Tree-sitter stuff
TSTree *parse_c_tree(meta_tree_t *tree);
// Hacky proto
TSLanguage *tree_sitter_c();
