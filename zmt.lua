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

function iter(...)
    return ipairs({...})
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
    elseif type(value) == 'string' then
        -- %q annoyingly replaces '\n' with '\\\n', that is, a backslash and
        -- then an actual newline. Replace the newline with an 'n'.
        return ('%q'):format(value):gsub('\n', 'n')
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
                local part = piece:sub(1, idx - 1)
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
    function self.get_lang(ftype)
        -- Fill in cached entry if it's not there
        if not self.langs[ftype] then
            if not TS_LANGS[ftype] then return nil end

            local zmt_parse, ts_lang, query_path = unpack(TS_LANGS[ftype])
            local lang = ts_lang()
            local q_text = io.open(query_path):read('*all')
            -- We don't care about errors now, just pass an unused array
            local dummy = ffi.new('uint32_t[1]')
            local query = ts.ts_query_new(lang, q_text, #q_text, dummy, dummy)
            self.langs[ftype] = {zmt_parse, query}
        end
        return unpack(self.langs[ftype])
    end

    function self.parse_buf(buf)
        -- XXX get actual extension/filetype
        local ext = buf.path:sub(-1, -1)
        local parse, query = self.get_lang(ext)

        if not parse then
            buf.query = TSNullQuery()
        else
            -- XXX sticking stuff into buffer object--maybe not the best
            -- abstraction...?
            buf.ast = parse(buf.tree)
            buf.query = TSQuery(query, buf)
        end
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
    local capture_pushback = nil

    function self.free()
        ts.ts_query_cursor_delete(cursor)
    end

    function self.reset(offset)
        ts.ts_query_cursor_exec(cursor, query, ts.ts_tree_root_node(buf.ast))
        ts.ts_query_cursor_set_byte_range(cursor, offset, -1)
        events = {}
        event_start, event_end = 0, 0
        capture_pushback = nil
    end

    local function get_capture_name(id)
        if not cap_names[id] then
            local cn = ts.ts_query_capture_name_for_id(query, id, cn_len)
            cap_names[id] = ffi.string(cn, cn_len[0])
        end
        return cap_names[id]
    end

    -- Advance the TSQueryCursor to the next capture
    local function next_capture()
        if capture_pushback then
            local c = capture_pushback
            capture_pushback = nil
            return unpack(c)
        end
        local ok = ts.ts_query_cursor_next_capture(cursor, match,
                cap_idx)
        if not ok then return end
        local capture = match[0].captures[cap_idx[0]]

        -- Return capture name, and start/end byte offsets
        local cn = get_capture_name(capture.index)
        local s = ts.ts_node_start_byte(capture.node)
        local e = ts.ts_node_end_byte(capture.node)
        return cn, s, e
    end

    -- Advance the event iterator
    function self.next_event()
        if event_start >= event_end then
            local cn, cap_start, cap_end = next_capture()
            if cn == nil then return end
            events = {{cap_start, cn}, {cap_end, nil}}

            -- Fill in any nested captures that happen within this one
            while true do
                local cn, s, e = next_capture()
                if cn == nil or s > cap_end then
                    assert(capture_pushback == nil)
                    capture_pushback = {cn, s, e}
                    break
                end
                events[#events+1] = {s, cn}
                events[#events+1] = {e, nil}
            end
            table.sort(events, function(a, b) return a[1] < b[1] end)
            event_start, event_end = 0, #events
        end
        event_start = event_start + 1
        return unpack(events[event_start])
    end
    return self
end

-- TSNullQuery conforms to the TSQuery interface but doesn't return any
-- highlight events. Used for unknown syntax, etc.
function TSNullQuery()
    local self = {}
    function self.reset(offset) end
    function self.next_event() end
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
    ['status']              = {15,  21},
    ['status-unfocused']    = {0,   15},
}
HL_COLORS = {}

LINE_NB_FMT = '%4d '
LINE_NB_WIDTH = 5

function init_color()
    nc.start_color()
    local idx = 1
    for k, v in pairs(HL_TYPE) do
        local fg, bg = unpack(v)
        nc.init_pair(idx, fg, bg)
        HL_TYPE[k] = idx
        HL_COLORS[idx] = {fg, bg}
        idx = idx + 1
    end
end

function draw_lines(window, is_focused)
    local buf = window.buf
    window.clear()

    -- The current highlight value, so we can have syntax regions that
    -- span multiple lines
    local cur_hl = HL_TYPE['default']

    -- The next highlight event
    -- Start at -1 so we grab the next capture immediately
    local event_offset = -1
    local event_hl = -1
    -- Keep a stack of highlights
    local hl_stack = {HL_TYPE['default']}

    local row = 0
    local col = 0

    -- Iterate through all lines in the file
    local last_line = -1
    local first = true
    for line, offset, is_end, piece in iter_lines(buf.tree, window.start_line,
            buf.tree.root.nl_count - 1) do
        -- Line number display
        if line ~= last_line then
            -- Start the query over once we get the first byte offset
            if first then
                first = false
                buf.query.reset(offset)
            end

            line_nb_str = LINE_NB_FMT:format(line + 1):sub(1, LINE_NB_WIDTH)
            local color = HL_TYPE['line_nb']
            window.write_at(row, 0, color, line_nb_str, LINE_NB_WIDTH)
            col = LINE_NB_WIDTH
            last_line = line
        end

        -- Chop up this piece into parts that share the same highlight
        while #piece > 0 do
            -- Jump the cursor forward until we know the next match
            while offset > event_offset do
                local off, hl = buf.query.next_event()
                event_offset = off
                if off == nil then
                    event_offset = 1e100
                    break
                end
                if hl == nil then
                    -- End of capture, pop the highlight stack
                    assert(#hl_stack > 1)
                    hl_stack[#hl_stack] = nil
                    event_hl = hl_stack[#hl_stack]
                else
                    -- Push this highlight. Duplicate the top stack if this is
                    -- an ignored capture, so we can still pop it off later
                    event_hl = HL_TYPE[hl] or hl_stack[#hl_stack]
                    hl_stack[#hl_stack + 1] = event_hl
                end
            end

            -- This byte starts or ends a highlight. Change the color output,
            -- and output one character of source so we make progress.
            if offset == event_offset then
                cur_hl = event_hl
                local part = piece:sub(1, 1)
                window.write_at(row, col, cur_hl, part, #part)
                piece = piece:sub(2)
                offset = offset + 1
                col = col + 1
            -- Default case: output up until the next highlight change
            elseif event_offset > offset then
                local bound = math.min(event_offset - offset, #piece)
                local part = piece:sub(1, bound)
                window.write_at(row, col, cur_hl, part, #part)
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
            if row >= window.rows - 1 then
                break
            end
        end
    end

    -- Draw status line
    local color = is_focused and HL_TYPE['status'] or
            HL_TYPE['status-unfocused']
    local status_line = buf.path .. (' '):rep(window.cols)
    status_line = status_line:sub(1, window.cols)
    window.write_at(window.rows - 1, 0, color, status_line, #status_line)

    window.refresh()
end

--------------------------------------------------------------------------------
-- Input handling --------------------------------------------------------------
--------------------------------------------------------------------------------

-- Action enum
local QUIT, SCROLL_UP, SCROLL_DOWN, SCROLL_HALFPAGE_UP,
    SCROLL_HALFPAGE_DOWN, WINDOW_SWITCH, check = enum(100)
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

function action_is_scroll(action, window)
    if action == SCROLL_UP then
        return -1
    elseif action == SCROLL_DOWN then
        return 1
    elseif action == SCROLL_HALFPAGE_UP then
        return -bit.rshift(window.rows, 1)
    elseif action == SCROLL_HALFPAGE_DOWN then
        return bit.rshift(window.rows, 1)
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
    ['\023']            = WINDOW_SWITCH,
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

function Window(buf, rows, cols, y, x)
    local self = {}
    self.buf = buf
    self.win = nc.newwin(rows, cols, y, x)
    self.rows, self.cols, self.y, self.x = rows, cols, y, x
    self.color = nil
    self.start_line = 0

    function self.clear() nc.wclear(self.win) end
    function self.refresh() nc.wrefresh(self.win) end

    function self.write_at(row, col, color, str, len)
        if color ~= self.color then
            nc.wcolor_set(self.win, color, nil)
            self.color = color
        end
        nc.mvwaddnstr(self.win, row, col, str, len)
    end

    function self.handle_scroll(scroll)
        local old_start_line = self.start_line
        self.start_line = self.start_line + scroll
        self.start_line = math.min(self.start_line, buf.tree.root.nl_count - 1)
        self.start_line = math.max(self.start_line, 0)
        -- Scroll screen contents with ncurses. This isn't really
        -- necessary, since we're going to update the whole screen,
        -- but should speed things up a bit
        nc.wscrl(self.win, self.start_line - old_start_line)
    end

    return self
end

function NullWindow(buf, rows, cols, y, x)
    local self = {}
    self.buf = buf
    self.rows, self.cols, self.y, self.x = rows, cols, y, x
    self.color = nil
    self.start_line = 0
    self.row = 0

    function self.clear()
        self.row = 0
    end
    function self.refresh()
    end

    function self.write_at(row, col, color, str, len)
        if row ~= self.row then
            io.stdout:write('\n')
        end
        if color ~= self.color then
            local fg, bg = unpack(HL_COLORS[color])
            io.stdout:write(('\027[38;5;%dm\027[48;5;%dm'):format(fg, bg))
            self.color = color
        end
        io.stdout:write(str)
        self.row = row
    end

    function self.handle_scroll(scroll)
        self.start_line = self.start_line + scroll
        self.start_line = math.min(self.start_line, buf.tree.root.nl_count - 1)
        self.start_line = math.max(self.start_line, 0)
    end

    return self
end

function run_tui(buffers, dumb_tui)
    local Window = Window
    if dumb_tui then
        -- Fake out 
        nc.LINES = 25
        nc.COLS = 80
        Window = NullWindow
    else
        -- ncurses setup
        local stdscr = nc.initscr()
        nc.scrollok(stdscr, true)
        nc.cbreak()
        nc.noecho()
        -- HACK: #defines aren't available, use -1
        nc.mousemask(ffi.cast('int', -1), nil)
    end
    init_color()

    -- Make a prefix tree out of all defined input sequences
    local input_tree = parse_input_table(MAIN_INPUT_TABLE)

    -- Split window up to show all buffers
    local windows = {}
    local first = 0
    for i = 1, #buffers do
        local last = math.floor(nc.LINES * i / #buffers)
        windows[#windows + 1] = Window(buffers[i], last - first,
                nc.COLS, first, 0)
        first = last
    end
    local cur_win = 1
    local window = windows[cur_win]

    -- Draw all windows
    function draw_all()
        for i = 1, #windows do
            draw_lines(windows[i], i == cur_win)
        end
    end

    draw_all()

    local start_line = 0
    while true do
        -- Draw screen
        if dumb_tui then
            draw_all()
        else
            draw_lines(window, true)
        end

        -- Handle input
        local action = get_next_input(input_tree)

        if action == QUIT then
            break
        elseif action == WINDOW_SWITCH then
            cur_win = cur_win % #windows + 1
            window = windows[cur_win]
            -- Draw all windows to refresh out-of-focus status lines
            draw_all()
        else
            local scroll = action_is_scroll(action, window)
            if scroll then
                window.handle_scroll(scroll)
            end
        end
    end
end

function main()
    -- Set up highlighting
    local ts_ctx = TSContext()

    if #arg < 1 then
        error('no files')
    end

    -- Read input files and parse it
    local buffers = {}
    for _, path in ipairs(arg) do
        local buf = read_buffer(path)
        ts_ctx.parse_buf(buf)
        buffers[#buffers + 1] = buf
    end

    -- Run the TUI, catching and printing any errors
    local res = {pcall(run_tui, buffers)}
    nc.endwin()
    print(unpack(res))
end

main()
