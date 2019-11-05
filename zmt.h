#include <ncurses.h>
#include <stdint.h>

#include "tree_sitter/api.h"

// ===META TREES===
// The meta-tree structure is the core of this editor. It's a combination
// of piece tables, persistent trees, and a zipper/one-hole context. Here's
// some quick references for these concepts:
//      https://en.wikipedia.org/wiki/Piece_table
//      https://en.wikipedia.org/wiki/Persistent_data_structure#Trees
//      https://en.wikipedia.org/wiki/Zipper_(data_structure)
//
// What does this mean? First off, all of the actual bytes of data in a buffer
// are stored in append-only chunks of data. When new text is inserted into the
// buffer, that new text is added to a data block, and the tree representing
// the current buffer state will get a new node that references the data block,
// with the appropriate offsets. Deleting text from a buffer only involves meta
// data updates: no new text gets added/removed from data blocks, but some
// nodes of the tree will get modified/removed to reflect the new state of the
// buffer.
//
// So that's the piece table part (or I guess "piece tree"). The next step is
// persistency. Persistency means that the piece tree representing the buffer
// is actually an immutable snapshot: when we make edits to the tree, we return
// a brand new tree, and the old tree is still valid. Since generally most of
// the tree doesn't need to be edited, these trees can share many subtrees. In
// fact, when replacing a node inside a tree at depth D, only D new nodes need
// to be created. Thus if the tree is reasonably balanced, editing operations
// can be completed in O(log N) time, with N being the number of tree nodes,
// and the base of the log determined by the branching factor of the tree (in
// this code, MAX_CHILDREN). Why does persistency matter at all here? One big
// benefit is thread safety: we can do things like pass one version of the
// buffer off to a syntax highlighter in a separate thread, and continue
// editing while the buffer is being parsed. Another big benefit is having
// instant access to all versions of the buffer throughout history, in parallel.
// This will eventually allow wacky features like showing different snapshots
// of a buffer side-by-side or searching for a regular expression through all
// historical versions of a buffer (hopefully without needing to search the
// entire buffer for every snapshot).
//
// So, O(log N) is pretty good, and edits should generally complete in something
// like a microsecond. But that's not quite good enough for me. That's where
// the one-hole context comes in. This isn't used with all the type system
// magic as in functional programming, and thus doesn't have the same
// flexibility or range of benefits, but that's where the idea came from. It's
// pretty much just used for one thing: making lots of sequential edits faster.
// It can also be thought of as an adaptation of the "gap buffer" idea to the
// tree structure. When editing starts at a particular place in the buffer,
// we split the metadata tree at that point, and insert a hole. The hole is
// basically an indirection, a marker to graft a different subtree in at this
// point, called the filler. As editing continues (assuming only inserting
// characters in sequence), the filler node is edited, while the rest of the
// tree can remain untouched. If no other threads hold a reference to the tree
// snapshot, the filler node can be updated in place. Otherwise, just one new
// tree is allocated (which holds the filler node inline), rather than needing
// to allocate tree nodes for the entire path leading down to the edit point.
// Either way, sequential edits are done in O(1) time instead of O(log N).
//
// At the moment, only one hole is supported, and the filler node is always
// a leaf node. It would be fairly straightforward to support multiple holes
// (which would work very nicely for multiple cursor support a la Lapis/Sublime
// Text). Larger filler sub-trees would be feasible as well, but as the size
// of the filler gets larger, the benefit starts to disappear.

typedef struct {
    int64_t line;
    int64_t byte;
} offset_t;

// chunk_t holds big chunks of contiguous backing memory

#define CHUNK_EMBEDDED          (1 << 0)
typedef struct {
    uint32_t ref_count;
    uint32_t flags;
    const uint8_t *data;
    size_t len;
    size_t used;
    const uint8_t embedded_data[];
} chunk_t;

// Main tree node structure for storing buffer state. We use one node type
// with a union, so we can use the same pointer types everywhere. The flags
// field has bits for determining which node type it is, according to the
// enum below.
//
// The overall size of the node involves a few tradeoffs/constraints:
//  * the size of leaf and inner nodes should ideally be the same size, so
//      that neither node type wastes any space.
//  * MAX_CHILDREN is the base of the log in our O(log N). Having more
//      children will reduce the overall depth of the tree, but this isn't
//      a pure win, since all of the re-allocating in creating a new tree
//      involves copying proportional to MAX_CHILDREN.
//  * Using MAX_CHILDREN > 2 might complicate some code paths involved in
//      tree editing
//  * the metadata that is stored only in leaf nodes should only be locally
//      useful (say, for rendering text). We store byte and newline counts
//      for leaf and internal nodes because they are needed for efficiently
//      seeking through the file.
//
// Right now, with no metadata in leaf nodes beyond byte/newline count,
// we can use MAX_CHILDREN==2 and fit a node into a nice 32 bytes.

#define MAX_CHILDREN            (2)

// Flags
enum { node_inner_bit, node_leaf_bit, node_hole_bit, node_filler_bit,
    node_has_hole_bit = 7};
#define NODE_INNER              (1 << node_inner_bit)
#define NODE_LEAF               (1 << node_leaf_bit)
#define NODE_HOLE               (1 << node_hole_bit)
#define NODE_FILLER             (1 << node_filler_bit)

#define NODE_HAS_HOLE           (1 << node_has_hole_bit)

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
            // level of indirection for most read operations. We keep a
            // map of chunks used within each tree in meta_tree_t so we
            // can access the metadata if we need it.
            const uint8_t *chunk_data;
        } leaf;
    };
} meta_node_t;

// Root structure for storing one snapshot of a buffer state, with any extra
// data needed. We store a list of chunks referenced in the tree.
#define DUMB_MAX_CHUNKS         (16)
typedef struct {
    uint32_t ref_count;
    meta_node_t *root;

    // Store whether this tree has a hole. It can only have one.
    bool has_hole;

    // The offset of the hole from the beginning of the file. This is needed
    // because tree metadata doesn't take the filler node into account, and
    // thus we need to know where the hole is, so we can take the size of
    // the filler into account when seeking past it
    offset_t hole_offset;

    // This node fills in the hole, if there is one
    meta_node_t filler_node[1];

    // XXX actual map of chunk data->chunk
    // XXX also need to store a local refcount here--to incrementally maintain
    // this map, and delete items from it when they're unused, we don't want
    // to have to walk the whole tree
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
    ITER_HOLES,
};

// Stack frame for iterative tree walking
typedef struct {
    meta_node_t *node;
    int8_t idx;
    enum ITER_STATE state;
} meta_iter_frame_t;

// Main iterator structure

// XXX right now we have a hardcoded stack limit. 32 is pretty big as far as
// number of edits, MAX_CHILDREN^32 nodes is a lot of editing, if we can keep
// our trees balanced.
#define ITER_STACK_SIZE         (32)

typedef struct {
    meta_tree_t *tree;

    // Tree walking stack and depth counter
    meta_iter_frame_t frame[ITER_STACK_SIZE];
    int32_t depth;

    // These offsets are set during iteration to the start/end points of
    // each leaf node.
    // XXX User code should never touch these... Maybe we should make a
    // function and keep these "private"
    offset_t start_offset;
    offset_t end_offset;

    // The offset we're currently seeking towards
    offset_t desired_offset;

    meta_node_t slice_node_left[1];
    meta_node_t slice_node_right[1];
} meta_iter_t;

////////////////////////////////////////////////////////////////////////////////
// Prototypes //////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// File/chunk management
chunk_t *map_file(const char *path);
meta_tree_t *read_data(chunk_t *chunk);

// Tree creation
meta_tree_t *create_tree(bool make_root);
meta_node_t *create_node();
meta_node_t *create_leaf();

// Metadata functions
offset_t get_tree_total_size(meta_tree_t *tree);
uint64_t get_tree_line_length(meta_tree_t *tree, uint64_t line);

// Iteration
void iter_init(meta_iter_t *iter, meta_tree_t *tree, enum ITER_STATE start);
meta_node_t *iter_next(meta_iter_t *iter);
meta_node_t *iter_start_at(meta_iter_t *iter, meta_tree_t *tree,
        uint64_t line_offset, uint64_t byte_offset);

// Mutation
meta_tree_t *split_at_offset(meta_tree_t *tree, uint64_t line_offset,
        uint64_t byte_offset);
meta_tree_t *append_bytes_to_filler(meta_tree_t *tree, const uint8_t *data,
    uint64_t len);

// Testing functions
void verify_node(meta_node_t *node);

// XXX experimental/test apis
meta_tree_t *insert_bytes_at_offset(meta_tree_t *tree, uint64_t line_offset,
        uint64_t byte_offset, const uint8_t *data, uint64_t len);
meta_tree_t *dumb_read_data(chunk_t *chunk);

// Tree-sitter stuff
TSTree *parse_c_tree(meta_tree_t *tree);
// Hacky proto
TSLanguage *tree_sitter_c();
