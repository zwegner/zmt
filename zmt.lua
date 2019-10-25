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

TS_LANGS = {
    ['c'] = {zmt.parse_c_tree, ts.tree_sitter_c, 'ts-query/c.txt'},
}

-- TSContext holds all global tree-sitter context
function TSContext()
    local self = {}
    self.langs = {}

    -- Parse a query file for a given language with tree-sitter
    self.get_lang = function(ftype)
        -- Fill in cached entry if it's not there
        if not self.langs[ftype] then
            if not TS_LANGS[ftype] then return nil end

            local zmt_parse, ts_lang, query_path = unpack(TS_LANGS[ftype])
            local lang = ts_lang()
            local q_text = io.open(query_path):read('*all')
            -- We don't care about errors now, just pass an unused array
            local dummy = ffi.new('uint32_t[1]')
            local query = ts.ts_query_new(lang, q_text, #q_text, dummy, dummy)
            self.langs[ftype] = {zmt_parse, lang, query}
        end
        return unpack(self.langs[ftype])
    end

    self.parse_buf = function(buf)
        -- XXX get actual extension/filetype
        local ext = buf.path:sub(-1, -1)
        local parse, lang, query = self.get_lang(ext)

        -- XXX sticking stuff into buffer object--maybe not the best
        -- abstraction...?
        buf.ast = parse(buf.tree)
        buf.query = TSQuery(query, buf)
    end

    return self
end

-- TSQuery holds information for a given buffer in a particular language
function TSQuery(query, buf)
    local self = {}
    local cursor = ts.ts_query_cursor_new()
    local match = ffi.new('TSQueryMatch[1]')
    local cap_idx = ffi.new('uint32_t[1]')
    local cn_len = ffi.new('uint32_t[1]')
    local cap_names = {}
    local events = {}
    local event_start, event_end = 0, 0

    self.free = function()
        ts.ts_query_cursor_delete(cursor)
    end

    self.reset = function(offset)
        ts.ts_query_cursor_exec(cursor, query, ts.ts_tree_root_node(buf.ast))
        ts.ts_query_cursor_set_byte_range(cursor, offset, ffi.cast('int32_t', -1))
        events = {}
        event_start, event_end = 0, 0
    end

    function get_capture_name(id)
        if not cap_names[id] then
            local cn = ts.ts_query_capture_name_for_id(query, id, cn_len)
            cap_names[id] = ffi.string(cn, cn_len[0])
        end
        return cap_names[id]
    end

    -- Advance the TSQueryCursor to the next capture
    function next_capture()
        local ok = ts.ts_query_cursor_next_capture(cursor, match,
                cap_idx)
        if not ok then return nil end
        local capture = match[0].captures[cap_idx[0]]

        -- Return capture name, and start/end byte offsets
        local cn = get_capture_name(capture.index)
        local s = ts.ts_node_start_byte(capture.node)
        local e = ts.ts_node_end_byte(capture.node)
        return cn, s, e
    end

    -- Advance the event iterator
    self.next_event = function()
        if event_start >= event_end then
            local cn, s, e = next_capture()
            if cn == nil then
                return 1e100, nil
            end
            events = {{s, cn}, {e, 'default'}}
            event_start, event_end = 0, 2
        end
        event_start = event_start + 1
        return unpack(events[event_start])
    end
    return self
end

--------------------------------------------------------------------------------
-- Drawing ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Color codes for syntax highlighting. Each value is a (fg, bg) pair
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

LINE_NB_FMT = '%4d '
LINE_NB_WIDTH = 5

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

function draw_lines(window, buf, start_line)
    nc.clear()

    -- The current highlight value, so we can have syntax regions that
    -- span multiple lines
    local cur_hl = 0

    -- The next highlight event
    -- Start at -1 so we grab the next capture immediately
    local event_offset = -1
    local event_hl = -1
    local q_hl

    local row = 0
    local col = 0

    function set_color(color)
        color = color > 0 and color or HL_TYPE['default']
        nc.wcolor_set(window, color, nil)
    end

    -- Iterate through all lines in the file
    local last_line = -1
    for line, offset, is_end, piece in iter_lines(buf.tree, start_line,
            buf.tree.root.nl_count - 1) do
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
            while offset > event_offset do
                -- Start the query over from the given byte offset
                if event_offset == -1 then
                    buf.query.reset(offset)
                end

                event_offset, event_hl = buf.query.next_event()
                if event_hl ~= nil and HL_TYPE[event_hl] then
                    event_hl = HL_TYPE[event_hl]
                else
                    event_hl = 0
                end
            end

            -- This byte starts or ends a highlight. Change the color output,
            -- and output one character of source so we make progress.
            if offset == event_offset then
                cur_hl = event_hl
                set_color(cur_hl)
                local part = piece:sub(1, 1)
                nc.mvwaddnstr(window, row, col, part, #part)
                piece = piece:sub(2)
                offset = offset + 1
                col = col + 1
            -- Default case: output up until the next highlight change
            elseif event_offset > offset then
                local bound = math.min(event_offset - offset, #piece)
                local part = piece:sub(1, bound)
                nc.mvwaddnstr(window, row, col, part, #part)
                piece = piece:sub(bound + 1)
                offset = offset + #part
                col = col + #part
            end
        end

        -- Break out if this piece had a newline
        if is_end then
            -- Check for a highlight that starts/ends on the newline
            if offset == event_offset then
                cur_hl = event_hl
            end
            row = row + 1
            col = 0
            if row >= nc.LINES then
                break
            end
        end
    end

    nc.wrefresh(window)
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

function read_buffer(path)
    local buf = {}
    buf.path = path
    buf.chunk = zmt.map_file(path)
    buf.tree = zmt.dumb_read_data(buf.chunk)
    return buf
end

function run_tui(buf)
    -- ncurses setup
    local window = nc.initscr()
    nc.scrollok(window, true)
    nc.cbreak()
    nc.noecho()
    init_color()
    -- HACK: #defines aren't available, use -1
    nc.mousemask(ffi.new('int', -1), nil)

    -- Make a prefix tree out of all defined input sequences
    local input_tree = parse_input_table(MAIN_INPUT_TABLE)

    local start_line = 0
    while true do
        -- Draw screen
        draw_lines(window, buf, start_line)

        -- Handle input
        local action = get_next_input(input_tree)

        if action == QUIT then
            break
        else
            local scroll = action_is_scroll(action)
            if scroll then
                local old_start_line = start_line
                start_line = start_line + scroll
                start_line = math.min(start_line, buf.tree.root.nl_count - 1)
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
    -- Set up highlighting
    local ts_ctx = TSContext()

    -- Read input file and parse it
    local buf = read_buffer(arg[1])
    ts_ctx.parse_buf(buf)

    -- Run the TUI, catching and printing any errors
    local res = {pcall(run_tui, buf)}
    nc.endwin()
    print(unpack(res))
end

main()
