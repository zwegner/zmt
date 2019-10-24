ffi = require('ffi')

-- Load library and preprocessed source
local zmt = ffi.load('_out/rel/zmt.so')
local defs = io.open('_out/pre.h')
ffi.cdef(defs:read('*all'))
defs:close()

-- Alias for tree-sitter
ts = zmt

--------------------------------------------------------------------------------
-- Main meta-tree interface ----------------------------------------------------
--------------------------------------------------------------------------------

-- Iterate through the given piece tree, starting at the given offset
function iter_nodes(tree, byte_offset, line_offset)
    byte_offset = byte_offset or 0
    line_offset = line_offset or 0
    return coroutine.wrap(function()
        local iter = ffi.new('meta_iter_t[1]')
        local node = zmt.iter_start(iter, tree, byte_offset, line_offset)
        while node ~= nil do
            local l = node.leaf
            -- end is a keyword. Oh well, it's the proper variable name
            local data = ffi.string(l.chunk_data + l.start, l['end'] - l.start)
            coroutine.yield(iter[0], node, data)
            node = zmt.iter_next(iter)
        end
    end)
end

-- Iterate through pieces, broken up by lines. We yield successive tuples of
-- the form (line_number, byte_offset, is_end, piece), where is_end marks
-- pieces that are at the end of their respective lines
function iter_lines(tree, line_start, line_end)
    return coroutine.wrap(function()
        -- Start a piece iterator at the start line
        local line = line_start
        for iter, node, piece in iter_nodes(tree, 0, line_start) do
            -- Chop up this piece into lines. We only do the scan the exact
            -- number of times needed
            local offset = iter.start_offset.byte
            for i = 0, node.nl_count - 1 do
                -- Cut off any part after a newline
                local idx = piece:find('\n')
                --assert idx ~= nil
                part = piece:sub(1, idx - 1)
                piece = piece:sub(idx + 1)
                coroutine.yield(line, offset, true, part)
                offset = offset + idx
                line = line + 1
                if line > line_end then
                    break
                end
            end
            if line > line_end then
                break
            end

            -- Yield the last part of this piece
            coroutine.yield(line, offset, false, piece)
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
HL_LINE_NB_FMT = '\027[48;5;238m\027[38;5;3m%4d ' .. HL_END

-- Parse a query file for a given language with tree-sitter
function get_query(language, path)
    local query = ffi.string(io.open(path):read('*all'))
    -- We don't care about errors now, just pass an unused array
    local dummy = ffi.new('uint32_t[1]')
    return ts.ts_query_new(language, query, #query, dummy, dummy)
end

function show_highlight(tree, ast, query)
    local cursor = ts.ts_query_cursor_new()
    ts.ts_query_cursor_exec(cursor, query, ts.ts_tree_root_node(ast))
    local match = ffi.new('TSQueryMatch[1]')
    local cap_idx = ffi.new('uint32_t[1]')
    local cn_len = ffi.new('uint32_t[1]')
    local hl_type

    -- Advance the TSQueryCursor to the next capture
    function next_capture()
        local ok = ts.ts_query_cursor_next_capture(cursor, match, cap_idx)
        if not ok then
            return 1e100, 1e100
        end
        local capture = match[0].captures[cap_idx[0]]

        -- Get capture type and its highlight
        local cn = ts.ts_query_capture_name_for_id(query,
                capture.index, cn_len)
        cn = ffi.string(cn, cn_len[0])
        hl_type = HL_TYPE[cn]

        -- No highlighting for this capture: skip it
        if not hl_type then
            return next_capture()
        end

        -- Return start/end byte offsets
        local s = ts.ts_node_start_byte(capture.node)
        local e = ts.ts_node_end_byte(capture.node)
        return s, e
    end

    -- The current highlight value, so we can have syntax regions that
    -- span multiple lines
    local cur_hl = HL_END

    -- The current tree-sitter query capture start and end byte offsets
    -- Start at -1 so we grab the next capture immediately
    local q_start = -1
    local q_end = -1

    -- Iterate through all lines in the file
    local last_line = -1
    for line, offset, is_end, piece in iter_lines(tree, 0,
            tree.root.nl_count - 1) do
        -- Line number display
        if line ~= last_line then
            io.stdout:write(HL_LINE_NB_FMT:format(line + 1) .. cur_hl)
            last_line = line
        end

        -- Chop up this piece into parts that share the same highlight
        while #piece > 0 do
            -- Jump the cursor forward until we know the next match
            if offset > q_end then
                q_start, q_end = next_capture()
            -- This piece starts or ends highlight. Output the escape code,
            -- and one character of source so we make progress.
            elseif offset == q_start or offset == q_end then
                cur_hl = (offset == q_start and HL_START:format(hl_type)
                        or HL_END)
                local part = piece:sub(1, 1)
                io.stdout:write(cur_hl .. part)
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

        -- Break out if this piece had a newline
        if is_end then
            -- Check for a highlight that ends on the newline
            if offset == q_end then
                cur_hl = HL_END
            end
            io.stdout:write('\n')
        end
    end

    ts.ts_query_cursor_delete(cursor)
end

-- Read input file
local chunk = zmt.map_file(arg[1])
local tree = zmt.dumb_read_data(chunk)

-- Highlight that shit
local ast = zmt.parse_c_tree(tree)
local query = get_query(ts.tree_sitter_c(), 'ts-query/c.txt')
show_highlight(tree, ast, query)
