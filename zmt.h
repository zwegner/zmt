// XXX no includes here for easy luajit integration
//#include <stdint.h>

// Flags
enum {
    NODE_INNER = 1,
    NODE_LEAF = 2
};

enum ITER_STATE {
    ITER_START,
    ITER_CHILDREN,
};

// These are basically #defines, but this is the luajit-friendly way to do it
// without needing to run the preprocessor on this file
enum { MAX_CHILDREN = 2 };
enum { DUMB_MAX_CHUNKS = 16 };
enum { ITER_STACK_SIZE = 32 };

// chunk_t holds big chunks of contiguous backing memory
// XXX make variant with embedded data for non-mmap()ed memory, so we only
// need one allocation per chunk
typedef struct {
    uint32_t ref_count;
    uint8_t *data;
    size_t len;
    size_t used;
} chunk_t;

// Main tree node for storing buffer state. We use one node type with a union,
// so we can use the same pointer types everywhere
typedef struct meta_node_t {
    uint32_t ref_count;
    uint32_t flags;
    int32_t nl_count;
    union {
        struct {
            struct meta_node_t *children[MAX_CHILDREN];
        } inner;
        struct {
            uint32_t start;
            uint32_t end;
            int32_t _unused;
            // Store a pointer to the raw data, rather than the chunk_t.
            // This is just for efficiency, since we don't want an extra
            // level of indirection for editing. We keep a map of chunks
            // used within each tree in meta_tree_t so we can access the
            // metadata if we need it.
            uint8_t *chunk_data;
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

typedef struct {
    meta_node_t *node;
    int8_t idx;
    enum ITER_STATE state;
} meta_iter_frame_t;

typedef struct {
    meta_tree_t *tree;
    meta_iter_frame_t frame[ITER_STACK_SIZE];
    int32_t depth;
} meta_iter_t;

// Prototypes
chunk_t *map_file(const char *path);
meta_tree_t *read_data(chunk_t *chunk);
meta_node_t *iter_next(meta_iter_t *iter);
meta_node_t *iter_start(meta_iter_t *iter, meta_tree_t *tree);
// XXX remove this
meta_tree_t *dumb_read_data(chunk_t *chunk);
