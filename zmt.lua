ffi = require('ffi')

-- Load library and preprocessed source
local zmt = ffi.load('_out/zmt.so')
local defs = io.open('_out/pre.h')
ffi.cdef(defs:read('*all'))
defs:close()

--------------------------------------------------------------------------------
-- Main meta-tree interface ----------------------------------------------------
--------------------------------------------------------------------------------

-- Iterate through the given piece tree, starting at the given offset
function iter_pieces(tree, byte_offset)
    byte_offset = byte_offset or 0
    return coroutine.wrap(function()
        local iter = ffi.new('meta_iter_t[1]')
        local node = zmt.iter_start(iter, tree, byte_offset, 0)
        while node ~= nil do
            local l = node.leaf
            -- end is a keyword. Oh well, it's the proper variable name
            local data = ffi.string(l.chunk_data + l.start, l['end'] - l.start)
            coroutine.yield(data)
            node = zmt.iter_next(iter)
        end
    end)
end

--------------------------------------------------------------------------------
-- Tree-sitter integration -----------------------------------------------------
--------------------------------------------------------------------------------

-- Terminal escape codes for syntax highlighting
HL_TYPE = {
    ['comment']             = 69,
    ['keyword']             = 41,
    ['preproc']             = 69,
    ['keyword.storagecls']  = 41,
    ['number']              = 9,
    ['string']              = 160,
    ['type']                = 41,
    ['type.user']           = 41,
}
HL_START = '\027[38;5;%dm'
HL_END = '\027[0m'

-- Parse a query file for a given language with tree-sitter
function get_query(language, path)
    local query = ffi.string(io.open(path):read('*all'))
    -- We don't care about errors now, just pass an unused array
    local dummy = ffi.new('uint32_t[1]')
    return zmt.ts_query_new(language, query, #query, dummy, dummy)
end

function show_highlight(root, ast, query)
    local cursor = zmt.ts_query_cursor_new()
    zmt.ts_query_cursor_exec(cursor, query, zmt.ts_tree_root_node(ast))
    local match = ffi.new('TSQueryMatch[1]')
    local cap_idx = ffi.new('uint32_t[1]')
    local cn_len = ffi.new('uint32_t[1]')
    local hl_type

    -- Advance the TSQueryCursor to the next capture
    function next_capture()
        local ok = zmt.ts_query_cursor_next_capture(cursor, match, cap_idx)
        if not ok then
            return 1e100, 1e100
        end
        local capture = match[0].captures[cap_idx[0]]

        -- Get capture type and its highlight
        local cn = zmt.ts_query_capture_name_for_id(query,
                capture.index, cn_len)
        cn = ffi.string(cn, cn_len[0])
        hl_type = HL_TYPE[cn]

        -- No highlighting for this capture: skip it
        if not hl_type then
            return next_capture()
        end

        -- Return start/end byte offsets
        local s = zmt.ts_node_start_byte(capture.node)
        local e = zmt.ts_node_end_byte(capture.node)
        return s, e
    end

    local q_start = -1
    local q_end = -1
    local offset = 0
    -- Iterate through the pieces of the tree, adding highlighting according
    -- to the tree-sitter captures and the HL_TYPE table above
    for piece in iter_pieces(root, 0) do
        -- Chop up this piece into parts that share the same highlight
        while #piece > 0 do
            -- Jump the cursor forward until we know the next match
            if offset > q_end then
                q_start, q_end = next_capture()
            -- This piece starts or ends highlight. Output the escape code,
            -- and one character of source so we make progress.
            elseif offset == q_start or offset == q_end then
                local hl = (offset == q_start and HL_START:format(hl_type)
                        or HL_END)
                local part = piece:sub(1, 1)
                io.stdout:write(hl .. part)
                piece = piece:sub(2)
                offset = offset + 1
            -- Default case: output up until the next highlight change
            else
                local bound = (offset < q_start and q_start or q_end)
                bound = math.min(bound - offset, #piece)
                local part = piece:sub(1, bound)
                io.stdout:write(part)
                piece = piece:sub(bound + 1)
                offset = offset + #part
            end
        end
    end

    zmt.ts_query_cursor_delete(cursor)
end

-- Read input file
local chunk = zmt.map_file(arg[1])
local root = zmt.dumb_read_data(chunk)

-- Highlight that shit
local ast = zmt.parse_c_tree(root)
local query = get_query(zmt.tree_sitter_c(), 'ts-query/c.txt')
show_highlight(root, ast, query)
