ffi = require('ffi')

-- Load library and preprocessed source
local zmt = ffi.load('_out/rel/zmt.so')
local defs = io.open('_out/pre.h')
ffi.cdef(defs:read('*all'))
defs:close()

-- Alias for tree-sitter
ts = zmt
-- Alias for ncurses
nc = zmt

--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------
--------------------------------------------------------------------------------

function log(fmt, ...)
    io.stderr:write(fmt:format(...) .. '\n')
end

function range(start, stop, step)
    local result = {}
    for i = start, stop, step do
        result[#result + 1] = i
    end
    return result
end

function enum(stop)
    return unpack(range(1, stop, 1))
end

function iter_bytes(str)
    return ipairs({str:byte(1, -1)})
end

function str(value)
    if type(value) == 'table' then
        local s = ''
        for k, v in pairs(value) do
            s = s .. ('[%s] = %s, '):format(str(k), str(v))
        end
        return '{' .. s .. '}'
    else
        return tostring(value)
    end
end

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
                assert(idx ~= nil)
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
    ['default']             = {15,   0},
    ['comment']             = {69,   0},
    ['keyword']             = {41,   0},
    ['preproc']             = {69,   0},
    ['keyword.storagecls']  = {41,   0},
    ['number']              = {9,    0},
    ['string']              = {160,  0},
    ['type']                = {41,   0},
    ['type.user']           = {41,   0},
    ['line_nb']             = {11, 238},
}

function init_color()
    nc.start_color()
    local idx = 1
    for k, v in pairs(HL_TYPE) do
        local fg, bg = unpack(v)
        nc.init_pair(idx, fg, bg)
        HL_TYPE[k] = idx
        idx = idx + 1
    end
end

LINE_NB_FMT = '%4d '
LINE_NB_WIDTH = 5

-- Parse a query file for a given language with tree-sitter
function get_query(language, path)
    local query = ffi.string(io.open(path):read('*all'))
    -- We don't care about errors now, just pass an unused array
    local dummy = ffi.new('uint32_t[1]')
    return ts.ts_query_new(language, query, #query, dummy, dummy)
end

function draw_lines(window, tree, ast, query, start_line)
    local cursor = ts.ts_query_cursor_new()
    ts.ts_query_cursor_exec(cursor, query, ts.ts_tree_root_node(ast))
    local match = ffi.new('TSQueryMatch[1]')
    local cap_idx = ffi.new('uint32_t[1]')
    local cn_len = ffi.new('uint32_t[1]')
    local hl_type

    nc.clear()

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
    local cur_hl = 0

    -- The current tree-sitter query capture start and end byte offsets
    -- Start at -1 so we grab the next capture immediately
    local q_start = -1
    local q_end = -1

    local row = 0
    local col = 0

    function set_color(color)
        color = color > 0 and color or HL_TYPE['default']
        nc.wcolor_set(window, color, nil)
    end

    -- Iterate through all lines in the file
    local last_line = -1
    for line, offset, is_end, piece in iter_lines(tree, start_line,
            tree.root.nl_count - 1) do
        -- Line number display
        if line ~= last_line then
            line_nb_str = LINE_NB_FMT:format(line + 1):sub(1, LINE_NB_WIDTH)
            set_color(HL_TYPE['line_nb'])
            nc.mvwaddnstr(window, row, 0, line_nb_str, LINE_NB_WIDTH)
            set_color(cur_hl)
            col = LINE_NB_WIDTH
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
                cur_hl = (offset == q_start and hl_type or 0)
                set_color(cur_hl)
                local part = piece:sub(1, 1)
                nc.mvwaddnstr(window, row, col, part, #part)
                piece = piece:sub(2)
                offset = offset + 1
                col = col + 1
            -- Default case: output up until the next highlight change
            else
                local bound = (offset < q_start and q_start or q_end)
                bound = math.min(bound - offset, #piece)
                local part = piece:sub(1, bound)
                nc.mvwaddnstr(window, row, col, part, #part)
                piece = piece:sub(bound + 1)
                offset = offset + #part
                col = col + #part
            end
        end

        -- Break out if this piece had a newline
        if is_end then
            -- Check for a highlight that ends on the newline
            if offset == q_end then
                cur_hl = 0
            end
            row = row + 1
            col = 0
            if row >= nc.LINES then
                break
            end
        end
    end

    nc.wrefresh(window)

    ts.ts_query_cursor_delete(cursor)
end

--------------------------------------------------------------------------------
-- Input handling --------------------------------------------------------------
--------------------------------------------------------------------------------

-- Action enum
local QUIT, SCROLL_UP, SCROLL_DOWN, SCROLL_HALFPAGE_UP,
    SCROLL_HALFPAGE_DOWN, check = enum(100)
assert(check ~= nil)

function mouse_input(seq)
    local code = seq[4] - 0x20
    local col = seq[5] - 0x21
    local row = seq[6] - 0x21
    if code == 64 then
        return SCROLL_UP
    elseif code == 65 then
        return SCROLL_DOWN
    end
end

function action_is_scroll(action)
    if action == SCROLL_UP then
        return -1
    elseif action == SCROLL_DOWN then
        return 1
    elseif action == SCROLL_HALFPAGE_UP then
        return -bit.rshift(nc.LINES, 1)
    elseif action == SCROLL_HALFPAGE_DOWN then
        return bit.rshift(nc.LINES, 1)
    end
    return nil
end

-- Input sequences
local MAIN_INPUT_TABLE = {
    ['QQ']              = QUIT,
    ['j']               = SCROLL_DOWN,
    ['k']               = SCROLL_UP,
    ['d']               = SCROLL_HALFPAGE_DOWN,
    ['u']               = SCROLL_HALFPAGE_UP,
    ['\027[M...']       = mouse_input,
}

-- Make a big tree for matching the inputs in input_table. The leaves are
-- actions, which is either an enum value from above or a function
function parse_input_table(input_table)
    local input_tree = {}
    for seq, action in pairs(input_table) do
        local node = input_tree
        for i, byte in iter_bytes(seq) do
            -- '.' == 46 is a wildcard
            if byte == 46 then
                byte = 0
            end

            if i == #seq then
                assert(node[byte] == nil)
                node[byte] = action
            else
                if node[byte] == nil then
                    node[byte] = {}
                end
                assert(type(node[byte]) == 'table')
                node = node[byte]
            end
        end
    end
    return input_tree
end

-- Read input until we parse a command
function get_next_input(input_tree)
    -- Store all current input sequence matches
    local matches = {}

    while true do
        local c = ffi.C.getchar()
        local buffer, action = nil, nil

        -- Look through in-progress matches and advance them if this character
        -- is in the given match sub-table
        for idx, m in ipairs(matches) do
            local prefix, match_table = unpack(m)
            local lookup = match_table[c] or match_table[0]
            if lookup ~= nil then
                prefix[#prefix + 1] = c

                if type(lookup) == 'table' then
                    -- This is a sub-table: add the character to the buffer
                    -- and match on the sub-table
                    matches[idx] = {prefix, lookup}
                elseif lookup ~= nil then
                    -- Leaf node: set the action and break
                    buffer, action = prefix, lookup
                    matches[idx] = nil
                    break
                end
            else
                -- Non-matching character: delete this match
                matches[idx] = nil
            end
        end

        -- Also check for starting a new match. We do this only after
        -- advancing previous matches so we don't advance this one again
        local lookup = input_tree[c] or input_tree[0]
        if type(lookup) == 'table' then
            -- New match
            matches[#matches + 1] = {{c}, lookup}
        elseif lookup ~= nil then
            -- Immediate match
            buffer, action = {c}, lookup
        end

        -- Check for a successful input match
        if type(action) == 'function' then
            action = action(buffer)
        end

        if action ~= nil then
            return action
        end
    end
end

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

function run_tui(tree, ast, query)
    local window = nc.initscr()

    nc.scrollok(window, true)
    nc.cbreak()
    nc.noecho()

    -- HACK: #defines aren't available, use -1
    nc.mousemask(ffi.new('int', -1), nil)

    -- Make a prefix tree out of all defined input sequences
    local input_tree = parse_input_table(MAIN_INPUT_TABLE)

    init_color()
    local start_line = 0
    while true do
        -- Draw screen
        draw_lines(window, tree, ast, query, start_line)

        -- Handle input
        local action = get_next_input(input_tree)

        if action == QUIT then
            break
        else
            local scroll = action_is_scroll(action)
            if scroll then
                local old_start_line = start_line
                start_line = start_line + scroll
                start_line = math.min(start_line, tree.root.nl_count - 1)
                start_line = math.max(start_line, 0)
                -- Scroll screen contents with ncurses. This isn't really
                -- necessary, since we're going to update the whole screen,
                -- but should speed things up a bit
                nc.scrl(start_line - old_start_line)
            end
        end
    end
end

function main()
    -- Read input file
    local chunk = zmt.map_file(arg[1])
    local tree = zmt.dumb_read_data(chunk)

    -- Set up highlighting
    local ast = zmt.parse_c_tree(tree)
    local query = get_query(ts.tree_sitter_c(), 'ts-query/c.txt')

    local res = {pcall(run_tui, tree, ast, query)}
    nc.endwin()
    print(unpack(res))
end

main()
