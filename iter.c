// iter.c, AKA low-budget C metaprogrammed iteration
//
// This file is #included twice, for forwards and backwards iteration

#if DIRECTION == 1

#   define ITER_INIT_FN     iter_init
#   define ITER_NEXT_FN     iter_next
#   define FIRST_IDX        (0)
#   define ADV_OFFSET(n)                                              \
    do {                                                              \
        iter->start_offset = iter->end_offset;                        \
        iter->end_offset.line += (n)->nl_count;                       \
        iter->end_offset.byte += (n)->byte_count;                     \
    } while (0)

#   define FOR_CHILDREN(c, n, f) \
        for (meta_node_t *(c) = NULL; (f)->idx < MAX_CHILDREN &&      \
                ((c) = (n)->inner.children[(f)->idx], true); (f)->idx++)

#elif DIRECTION == -1

#   define ITER_INIT_FN     iter_init_backwards
#   define ITER_NEXT_FN     iter_prev
#   define FIRST_IDX        (MAX_CHILDREN - 1)
#   define ADV_OFFSET(n)                                              \
    do {                                                              \
        iter->end_offset = iter->start_offset;                        \
        iter->start_offset.line -= (n)->nl_count;                     \
        iter->start_offset.byte -= (n)->byte_count;                   \
    } while (0)

#   define FOR_CHILDREN(c, n, f) \
        for (meta_node_t *(c) = NULL; (f)->idx >= 0 &&      \
                ((c) = (n)->inner.children[(f)->idx], true); (f)->idx--)

#else
#   error "invalid direction"
#endif

// Initialize an iterator object. Does not allocate, it should be on the stack
void ITER_INIT_FN(meta_iter_t *iter, meta_tree_t *tree, enum ITER_STATE start) {
    meta_node_t *root = tree->root;
#if DIRECTION == 1
    offset_t off = {};
#else
    offset_t off = get_tree_total_size(tree);
#endif

    *iter = (meta_iter_t) {
        .tree = tree,
        // XXX this zero-initializes the whole stack, which is probably
        // unnecessary. Possibly worth micro-optimizing this away later
        .frame = { { .node = root, .idx = FIRST_IDX, .state = start } },
        .start_offset = off,
        .end_offset = off,
        .depth = 1,
    };
}

// Macros for the iterative tree walking, to simulate recursion

#define CALL(n, start_state)                                          \
    do {                                                              \
        assert(iter->depth < ITER_STACK_SIZE);                        \
        iter->frame[iter->depth] = (meta_iter_frame_t) {              \
            .node = (n),                                              \
            .idx = FIRST_IDX,                                         \
            .state = (start_state)                                    \
        };                                                            \
        iter->depth++;                                                \
        goto next_iter;                                               \
    } while (0)

#define YIELD(n)                                                      \
    do {                                                              \
        ADV_OFFSET(n);                                                \
        iter->depth--;                                                \
        iter->post_inc = true; \
        return (n);                                                   \
    } while (0)

#define RETURN()      do { iter->depth--; iter->post_inc = true;      \
    goto next_iter; } while (0)

// Advance the iterator to the next leaf node
meta_node_t *ITER_NEXT_FN(meta_iter_t *iter) {
    while (iter->depth > 0) {
        meta_iter_frame_t *frame = &iter->frame[iter->depth - 1];
        meta_node_t *node = frame->node;

        // Kinda hacky: if the last thing we did in the iterator was return or
        // yield, we increment/decrement the child index for the parent node
        // based on the direction. This means, for interior nodes, the index is
        // always pointing at the child we're under in the tree, and we can
        // switch directions in between iterator advancements.
        if (iter->post_inc) {
            frame->idx += DIRECTION;
            iter->post_inc = false;
        }

        switch (frame->state) {
            case ITER_START:
                // Check for a leaf node
                if (node->flags & NODE_LEAF)
                    YIELD(node);

                // Check if this is a hole node. If so, yield the filler node
                if (node->flags & NODE_HOLE) {
                    if (!iter->tree->filler_node->leaf.chunk_data)
                        RETURN();
                    YIELD(iter->tree->filler_node);
                }

                frame->state = ITER_CHILDREN;

                // FALLTHROUGH
            case ITER_CHILDREN:
                // Inner node. See if there's more children
                FOR_CHILDREN(child, node, frame) {
                    if (!child)
                        continue;
                    CALL(child, ITER_START);
                }
                RETURN();

            // Jump mode. Quickly skip over subtrees to get to a desired
            // byte or line offset.
            case ITER_JUMP:
                // Check for a leaf node or hole node, like ITER_START above
                if (node->flags & NODE_LEAF)
                    YIELD(node);
                else if (node->flags & NODE_HOLE) {
                    if (!iter->tree->filler_node->leaf.chunk_data)
                        RETURN();
                    YIELD(iter->tree->filler_node);
                }

                // Reset the frame state to ITER_CHILDREN, since in JUMP mode
                // we only affect the first descent into the tree--all the
                // rest of the iteration is normal
                frame->state = ITER_CHILDREN;

                // Keep track of the absolute offset, which is used to see
                // whether a given node contains the desired jump offset
                offset_t off;
                if (DIRECTION == 1)
                    off = iter->start_offset;
                else
                    off = iter->end_offset;

                FOR_CHILDREN(child, node, frame) {
                    if (!child)
                        continue;

                    // Calculate the offset at the end of this node, making sure
                    // to handle the hole/filler difference as well.
                    off.line += DIRECTION * (int32_t)child->nl_count;
                    off.byte += DIRECTION * (int32_t)child->byte_count;
                    if (child->flags & NODE_HAS_HOLE) {
                        meta_node_t *filler = iter->tree->filler_node;
                        off.line += DIRECTION * (int32_t)(filler->nl_count - 1);
                        off.byte += DIRECTION * (int32_t)(filler->byte_count - 1);
                    }

                    // Check whether this node contains the desired offset.
                    // The condition is flipped when iterating backwards.
                    bool inside = (iter->desired_offset.byte ?
                            (off.byte >  iter->desired_offset.byte) :
                            (off.line >= iter->desired_offset.line));
                    if (DIRECTION == -1)
                        inside = !inside;

                    if (inside)
                        CALL(child, ITER_JUMP);
                    else {
                        iter->start_offset = off;
                        iter->end_offset = off;
                    }
                }
                RETURN();

            // Hole iteration mode
            case ITER_HOLES:
                if (node->flags & NODE_HOLE)
                    YIELD(node);

                if (node->flags & NODE_INNER) {
                    FOR_CHILDREN(child, node, frame) {
                        if (child && child->flags & NODE_HAS_HOLE)
                            CALL(child, ITER_HOLES);
                    }
                }
                RETURN();
        }
next_iter:
        ;
    }

    return NULL;
}

#undef ITER_INIT_FN
#undef ITER_NEXT_FN
#undef FIRST_IDX
#undef ADV_OFFSET
#undef FOR_CHILDREN
#undef CALL
#undef YIELD
#undef RETURN
#undef DIRECTION
