require('stdlib')
local ffi = require('ffi')

local module = {}

-- Build the library. Not great for separation of concerns right now,
-- but oh well.
local conf = 'rel'
if arg[1] == '-d' then
    table.remove(arg, 1)
    conf = 'deb'
end
local so_path = ('_out/%s/zmt.so'):format(conf)
local h_path = '_out/pre.h'
ffi.cdef('int system(const char *command);')
if ffi.C.system(('make.py %s %s'):format(so_path, h_path)) ~= 0 then
    error('failed build')
end

-- Load library and preprocessed source
local zmt = ffi.load(so_path)
local defs = io.open(h_path)
ffi.cdef(defs:read('*all'))
defs:close()

-- Make C library accessible elsewhere
module.zmt = zmt

-- Alias for libc
local C = ffi.C
-- Alias for tree-sitter
local ts = zmt
-- Alias for ncurses
local nc = zmt

-- Editor definitions

-- Mode enum
local NORMAL_MODE, INSERT_MODE, VISUAL_MODE, check = enum(5)
assert(check ~= nil)
local MODE_STR = {
    [NORMAL_MODE] = '',
    [INSERT_MODE] = '-- INSERT --',
    [VISUAL_MODE] = '-- VISUAL --',
}

-- Action enum
local
    -- Generic/mode switching commands
    ENTER_INSERT, EXIT_INSERT, ENTER_VISUAL, EXIT_VISUAL, QUIT,
    -- Insert mode
    INSERT_CHAR,
    -- Cursor
    _CURSOR_MIN, CURSOR_UP, CURSOR_DOWN, CURSOR_LEFT, CURSOR_RIGHT,
    CURSOR_HOME, CURSOR_END, CURSOR_NL, _CURSOR_MAX,
    -- Scrolling
    SCROLL_UP, SCROLL_DOWN, SCROLL_HALFPAGE_UP, SCROLL_HALFPAGE_DOWN,
    -- Mouse events
    MOUSE_DOWN,
    -- Window movement
    WINDOW_NEXT, WINDOW_PREV,
    check = enum(100)
assert(check ~= nil)

-- Render event enum
local EV_HL_BEGIN, EV_HL_END, EV_WRAP, EV_CURSOR, EV_EOF,
        EV_VISUAL_BEGIN, EV_VISUAL_END, check = enum(50)
assert(check ~= nil)

-- Color codes for syntax highlighting. Each value is a (fg, bg) pair
local HL_TYPE = {
    ['default']             = {15,   0},
    ['comment']             = {69,   0},
    ['keyword']             = {28,   0},
    ['preproc']             = {69,   0},
    ['keyword.storagecls']  = {41,   0},
    ['number']              = {9,    0},
    ['string']              = {160,  0},
    ['type']                = {41,   0},
    ['type.user']           = {41,   0},
    ['error']               = {15,   9},

    ['visual']              = {15,   0, 'inv'},
    ['line_nb']             = {11, 238},
    ['status']              = {15,  21, 'bold'},
    ['status-unfocused']    = {0,   15},
    ['mode_line']           = {15,   0, 'bold'},
}

local LINE_NB_FMT = '%4d '
local LINE_NB_WIDTH = 5

--------------------------------------------------------------------------------
-- Main meta-tree interface ----------------------------------------------------
--------------------------------------------------------------------------------

-- HACK
local NODE_INNER  = bit.lshift(1, zmt.node_inner_bit)
local NODE_LEAF   = bit.lshift(1, zmt.node_leaf_bit)
local NODE_HOLE   = bit.lshift(1, zmt.node_hole_bit)
local NODE_FILLER = bit.lshift(1, zmt.node_filler_bit)
local MAX_CHILDREN = 2

function module.iter_start(tree, line_offset, byte_offset)
    byte_offset = byte_offset or 0
    line_offset = line_offset or 0
    local iter = ffi.new('meta_iter_t[1]')
    local node = zmt.iter_start_at(iter, tree, line_offset, byte_offset)
    return iter, node
end

-- Iterate through the given piece tree, starting at the given offset
function module.iter_nodes(tree, line_offset, byte_offset)
    return coroutine.wrap(function()
        local iter, node = module.iter_start(tree, line_offset, byte_offset)
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
function module.iter_lines(tree, line_start, line_end)
    return coroutine.wrap(function()
        -- Start a piece iterator at the start line
        local line = line_start
        for iter, node, piece in module.iter_nodes(tree, line_start, 0) do
            -- Chop up this piece into lines. We only do the scan the exact
            -- number of times needed
            local offset = tonumber(iter.start_offset.byte)
            for i = 0, node.nl_count - 1 do
                -- Cut off any part after a newline
                local idx = piece:find('\n')
                assert(idx ~= nil)
                local part = piece:sub(1, idx)
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

-- Recursive coroutine that formats tree printing
local function co_tree_print(tree, node, depth)
    local prefix = ('%s %s(%s) b=%s n=%s'):format(
            ('  '):rep(depth), node, node.flags, node.byte_count, node.nl_count)

    if bit.band(node.flags, NODE_LEAF + NODE_FILLER) ~= 0 then
        if bit.band(node.flags, NODE_FILLER) ~= 0 then
            prefix = prefix .. ' FILLER'
        end
        local l = node.leaf
        local data = ffi.string(l.chunk_data + l.start, l['end'] - l.start)
        coroutine.yield(fmt(prefix, str(data)))
    elseif bit.band(node.flags, NODE_HOLE) ~= 0 then
        coroutine.yield(fmt(prefix, 'HOLE'))
        co_tree_print(tree, tree.filler_node[0], depth+1)
    else
        coroutine.yield(prefix)
        for i = 0, MAX_CHILDREN-1 do
            if node.inner.children[i] ~= nil then
                co_tree_print(tree, node.inner.children[i], depth+1)
            end
        end
    end
end

local function iter_tree_print(tree)
    return coroutine.wrap(function ()
        co_tree_print(tree, tree.root, 0)
    end)
end

function module.print_tree(tree)
    log('\n\nTREE')
    for line in iter_tree_print(tree) do
        log(line)
    end
end

--------------------------------------------------------------------------------
-- Tree-sitter integration -----------------------------------------------------
--------------------------------------------------------------------------------

local TS_LANGS = {
    ['c'] = {zmt.parse_c_tree, ts.tree_sitter_c, 'ts-query/c.txt'},
    ['h'] = {zmt.parse_c_tree, ts.tree_sitter_c, 'ts-query/c.txt'},
}

-- TSQuery holds information for a given buffer in a particular language
local function TSQuery(query, buf)
    local self = {}
    local cursor = ts.ts_query_cursor_new()
    local match = ffi.new('TSQueryMatch[1]')
    local cap_idx = ffi.new('uint32_t[1]')
    local cn_len = ffi.new('uint32_t[1]')
    local cap_names = {}
    local current_capture = nil

    function self.free()
        ts.ts_query_cursor_delete(cursor)
    end

    function self.reset(offset)
        ts.ts_query_cursor_exec(cursor, query, ts.ts_tree_root_node(buf.ast))
        ts.ts_query_cursor_set_byte_range(cursor, offset, -1)
        self.next_capture()
    end

    local function get_capture_name(id)
        if not cap_names[id] then
            local cn = ts.ts_query_capture_name_for_id(query, id, cn_len)
            cap_names[id] = ffi.string(cn, cn_len[0])
        end
        return cap_names[id]
    end

    -- Advance the TSQueryCursor to the next capture
    function self.next_capture()
        local ok = ts.ts_query_cursor_next_capture(cursor, match, cap_idx)
        if not ok then
            current_capture = {}
            return
        end
        local capture = match[0].captures[cap_idx[0]]

        -- Return capture name, and start/end byte offsets
        local cn = get_capture_name(capture.index)
        local s = ts.ts_node_start_byte(capture.node)
        local e = ts.ts_node_end_byte(capture.node)
        current_capture = {cn, s, e}
    end

    function self.current_capture()
        return unpack(current_capture)
    end

    return self
end

-- TSNullQuery conforms to the TSQuery interface but doesn't return any
-- highlight events. Used for unknown syntax, etc.
local function TSNullQuery()
    local self = {}
    function self.reset(offset) end
    function self.next_capture() end
    function self.current_capture() end
    return self
end

-- TSContext holds all global tree-sitter context
local function TSContext()
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

            local offset = ffi.new('uint32_t[1]')
            local error_type = ffi.new('uint32_t[1]')
            local query = ts.ts_query_new(lang, q_text, #q_text, offset, error_type)
            if query == nil then
                error(('error parsing query file %q, error %d at offset %d')
                        :format(query_path, error_type[0], offset[0]))
            end
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

--------------------------------------------------------------------------------
-- Drawing ---------------------------------------------------------------------
--------------------------------------------------------------------------------

local HL_ATTRS = {}
local HL_ATTR_IDS = {}

local function init_color()
    local idx = 1
    for k, v in pairs(HL_TYPE) do
        local fg, bg, ext = unpack(v)
        nc.init_pair(idx, fg, bg)
        -- HACK: hardcoded bit offsets, since we don't have the #defines here
        attr = bit.lshift(idx, 8)
        if ext == 'bold' then
            attr = attr + bit.lshift(1, 8+13)
        elseif ext == 'inv' then
            attr = attr + bit.lshift(1, 8+10)
        end
        HL_TYPE[k] = idx
        HL_ATTR_IDS[idx] = attr
        HL_ATTRS[idx] = {fg, bg, ext}
        idx = idx + 1
    end
end

-- EventContext manages rendering events, which are byte offsets in which a
-- special action needs to take place during rendering, such as changing the
-- highlight, wrapping the line, or displaying the cursor. We keep a priority
-- queue of sorts that will return the next event, but this simple model is
-- complicated by events like line wrapping which are dynamic: we don't know
-- what byte offset a line wrap happens on until we start the line, and we
-- don't know when a line ends until we hit the next one.
local function EventContext(window, query, width)
    local self = {}

    -- Dynamic events: line wrap and cursor
    local next_wrap_offset
    local cursor_offset = nil
    local visual_start, visual_end

    -- Event buffer, for managing nested highlights
    local event_buf = {}
    local event_start, event_end = 0, 0
    local next_hl_event = {-1, nil, nil}

    local first = true

    function self.mark_line_start(line, offset)
        -- Set next potential line wrap event
        next_wrap_offset = offset + width
        -- If this is the line with the cursor, set up an event
        cursor_offset = (line == window.curs_line and offset + window.curs_byte)
        -- Same, but for visual mode
        if window.visual_start_line then
            visual_start = (line == window.visual_start_line and
                    offset + window.visual_start_byte)
            visual_end = (line == window.curs_line and offset + window.curs_byte)
            if window.visual_start_line > window.curs_line or
                (window.visual_start_line == window.curs_line and
                window.visual_start_byte > window.curs_byte) then
                visual_start, visual_end = visual_end, visual_start
            end
        else
            visual_start, visual_end = nil, nil
        end

        -- Start the tree-sitter query over once we get the first byte offset
        if first then
            first = false
            query.reset(offset)
            if window.visual_start_line and window.visual_start_line < line then
                visual_start = offset
            end
        end
    end

    function self.next_capture_event()
        -- If the buffer is exhausted, grab a new capture, and fill in any
        -- nested captures
        if event_start >= event_end then
            local cap_name, cap_start, cap_end = query.current_capture()
            if cap_name == nil then
                next_hl_event = {1e100, EV_EOF, nil}
                return
            end

            event_buf = {{cap_start, EV_HL_BEGIN, cap_name},
                    {cap_end, EV_HL_END, nil}}

            -- Fill in any nested captures that happen within this one
            while true do
                query.next_capture()
                local n, s, e = query.current_capture()
                if n == nil or s >= cap_end then
                    break
                end
                event_buf[#event_buf+1] = {s, EV_HL_BEGIN, n}
                event_buf[#event_buf+1] = {e, EV_HL_END, nil}
            end
            table.sort(event_buf, function(a, b) return a[1] < b[1] end)
            event_start, event_end = 0, #event_buf
        end

        event_start = event_start + 1
        next_hl_event = event_buf[event_start]
    end

    -- Peek at the current event. We generally do this without consuming
    -- an event, since due to dynamic events the next event might change
    -- as we move forward through the buffer
    function self.current_event()
        -- Build a table of candidate events, and find the nearest one
        local events = {
            {next_wrap_offset, EV_WRAP, nil},
            {visual_start, EV_VISUAL_BEGIN, 'visual'},
            {visual_end, EV_VISUAL_END, 'visual'},
            {cursor_offset, EV_CURSOR, nil},
            {next_wrap_offset, EV_WRAP, nil},
        }
        local event = next_hl_event
        for _, e in ipairs(events) do
            if e[1] and e[1] < event[1] then event = e end
        end
        return event
    end

    -- Advance the event queue
    function self.next_event(event_type)
        if event_type == EV_CURSOR then
            cursor_offset = nil
        elseif event_type == EV_VISUAL_BEGIN then
            visual_start = nil
        elseif event_type == EV_VISUAL_END then
            visual_end = nil
        elseif event_type == EV_WRAP then
            next_wrap_offset = next_wrap_offset + width
        else
            self.next_capture_event()
        end
    end

    return self
end

local function draw_number_column(window, row, line, wrap)
    local line_nb_str
    if wrap then
        line_nb_str = (' '):rep(LINE_NB_WIDTH)
    else
        line_nb_str = LINE_NB_FMT:format(line + 1):sub(1, LINE_NB_WIDTH)
    end
    window.write_at(row, 0, HL_TYPE['line_nb'], line_nb_str, LINE_NB_WIDTH)
end

local function draw_status_line(window, is_focused)
    local attr = is_focused and HL_TYPE['status'] or
            HL_TYPE['status-unfocused']
    local status_line = right_pad(window.buf.path, window.cols)
    window.write_at(window.rows - 1, 0, attr, status_line, #status_line)
    window.end_row()
end

local function draw_mode_line(window, mode)
    local attr = HL_TYPE['mode_line']
    local mode_line = MODE_STR[mode]
    window.write_at(window.rows - 1, 0, attr, mode_line, #mode_line)
    window.end_row()
    window.refresh()
end

local function draw_lines(window, is_focused)
    local buf = window.buf
    window.clear()

    -- Render event handling
    local event_ctx = EventContext(window, buf.query, window.cols - LINE_NB_WIDTH)
    -- The current render event we're waiting for
    local event_offset, event_type, event_hl = -1, nil, nil
    -- A stack of highlights, so highlights can nest
    local hl_stack = {HL_TYPE['default']}
    -- The current highlight value
    local cur_hl = HL_TYPE['default']
    local visual_hl

    local last_line = -1
    local row = 0
    local col = 0

    local function set_event()
        event_offset, event_type, event_hl = unpack(event_ctx.current_event())
    end

    local function next_event()
        event_ctx.next_event(event_type)
        set_event()
    end

    -- Update state based on the current event. Generally called at the exact
    -- offset of the event, except when handling highlights that start before
    -- the start of the screen. This also advances the event iterator.
    local function handle_event()
        -- Start highlight
        if event_type == EV_HL_BEGIN then
            -- Push this highlight. Duplicate the top stack if this is
            -- an ignored capture, so we can still pop it off later
            cur_hl = HL_TYPE[event_hl] or hl_stack[#hl_stack]
            hl_stack[#hl_stack + 1] = cur_hl
        -- End highlight
        elseif event_type == EV_HL_END then
            -- End of capture, pop the highlight stack
            assert(#hl_stack > 1)
            hl_stack[#hl_stack] = nil
            cur_hl = hl_stack[#hl_stack]
        elseif event_type == EV_VISUAL_BEGIN then
            visual_hl = HL_TYPE['visual']
        elseif event_type == EV_VISUAL_END then
            visual_hl = nil
        -- Line wrap. HACK: make sure to not wrap if this is the last
        -- character of a line
        elseif event_type == EV_WRAP then
        --elseif event_type == EV_WRAP and (not is_end or #piece > 0) then
            row = row + 1
            col = LINE_NB_WIDTH
            draw_number_column(window, row, line, true)
        -- Mark cursor
        elseif event_type == EV_CURSOR then
            window.mark_cursor(row, col - LINE_NB_WIDTH)
        end
        next_event()
    end

    -- Iterate through all lines in the file
    for line, offset, is_end, piece in buf.iter_lines_from(window.start_line) do
        -- Line number display
        if line ~= last_line then
            event_ctx.mark_line_start(line, offset)
            set_event()

            -- When drawing the very first line, read through events to get
            -- the current highlight state, which might start above the screen
            if last_line == -1 then
                while offset > event_offset do
                    handle_event()
                end
                cur_hl = hl_stack[#hl_stack]
            end

            draw_number_column(window, row, line, false)
            col = LINE_NB_WIDTH
            last_line = line
        end

        -- Chop up this piece into parts separated by events from event_ctx
        while (#piece > 0 or offset == event_offset) and
                row < window.rows - 1 do
            -- Skip over any events we've already gone past
            while offset > event_offset do
                next_event()
            end

            -- Output any characters up until the next event
            if offset < event_offset then
                local bound = math.min(event_offset - offset, #piece)
                local part = piece:sub(1, bound)
                window.write_at(row, col, visual_hl or cur_hl, part, #part)
                piece = piece:sub(bound + 1)
                offset = offset + #part
                col = col + #part
            end

            -- We've reached an event, update the current state
            if offset == event_offset then
                handle_event()
            end
        end

        -- Move to the next line if this piece had a newline
        if is_end then
            row = row + 1
        end
        -- Check row count
        if row >= window.rows - 1 then break end
    end

    draw_status_line(window, is_focused)

    window.refresh()
end

--------------------------------------------------------------------------------
-- Input handling --------------------------------------------------------------
--------------------------------------------------------------------------------

local function handle_mouse_input(buf)
    -- XXX hardcoded values
    local code = buf[4] - 0x20
    local col = buf[5] - 0x21
    local row = buf[6] - 0x21
    if code >= 0 and code <= 2 then
        return MOUSE_DOWN, {row, col}
    elseif code == 64 then
        return SCROLL_UP, {row, col}
    elseif code == 65 then
        return SCROLL_DOWN, {row, col}
    end
end

local function handle_insert_char(buf)
    assert(#buf == 1)
    -- Ignore all non-ASCII and control characters except \n
    if buf[1] >= 32 and buf[1] < 127 then
        return INSERT_CHAR, buf[1]
    elseif buf[1] == 13 then
        return INSERT_CHAR, 10
    end
end

-- HACKy helper functions that rely on action enum ordering

local function action_is_scroll(action)
    return action >= SCROLL_UP and action <= SCROLL_HALFPAGE_DOWN
end
local function action_is_cursor(action)
    return action >= _CURSOR_MIN and action <= _CURSOR_MAX
end
local function action_is_window_switch(action)
    return action >= WINDOW_NEXT and action <= WINDOW_PREV
end

local function CTRL(x) return string.char(bit.band(string.byte(x), 0x1f)) end

-- Input sequences, per mode

local INPUT_TABLES = {
    [NORMAL_MODE] = {
        ['QQ']              = QUIT,

        ['h']               = CURSOR_LEFT,
        ['j']               = CURSOR_DOWN,
        ['k']               = CURSOR_UP,
        ['l']               = CURSOR_RIGHT,
        ['0']               = CURSOR_HOME,
        ['$']               = CURSOR_END,

        ['i']               = ENTER_INSERT,
        ['v']               = ENTER_VISUAL,

        [CTRL('E')]         = SCROLL_DOWN,
        [CTRL('Y')]         = SCROLL_UP,
        [CTRL('D')]         = SCROLL_HALFPAGE_DOWN,
        [CTRL('U')]         = SCROLL_HALFPAGE_UP,
        ['\027[M...']       = handle_mouse_input,
        [CTRL('N')]         = WINDOW_NEXT,
        [CTRL('P')]         = WINDOW_PREV,
    },
    [INSERT_MODE] = {
        ['\027']            = EXIT_INSERT,
        ['.']               = handle_insert_char,
    },
    [VISUAL_MODE] = {
        ['\027']            = EXIT_VISUAL,
        ['h']               = CURSOR_LEFT,
        ['j']               = CURSOR_DOWN,
        ['k']               = CURSOR_UP,
        ['l']               = CURSOR_RIGHT,
        ['0']               = CURSOR_HOME,
        ['$']               = CURSOR_END,
    }
}

-- Make a big tree for matching the inputs in input_table. The leaves are
-- actions, which is either an enum value from above or a function
local function parse_input_table(input_table)
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

-- Make a prefix tree out of all defined input sequences, one for each mode
local INPUT_TREES = {}
for mode, table in pairs(INPUT_TABLES) do
    INPUT_TREES[mode] = parse_input_table(table)
end

-- Read input until we parse a command
local function get_next_input(input_tree)
    -- Store all current input sequence matches
    local matches = {}

    while true do
        local c = C.getchar()
        local buffer, action, data = nil, nil, nil

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
            action, data = action(buffer)
        end

        if action ~= nil then
            return action, buffer, data
        end
    end
end

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

local function Buffer(path)
    local self = {}
    self.path = path
    self.chunk = zmt.map_file(path)
    self.tree = zmt.dumb_read_data(self.chunk)

    function self.get_line_count()
        return tonumber(zmt.get_tree_total_size(self.tree).line) + 1
    end

    function self.iter_lines_from(start_line)
        return module.iter_lines(self.tree,
            start_line, self.get_line_count())
    end

    return self
end

local function TreeDebugBuffer(buf)
    local self = {}
    self.query = TSNullQuery()
    self.path = '[tree-debug]'
    self.lines = 0

    function self.iter_lines_from(start_line)
        return coroutine.wrap(function ()
            local line_nb = 0
            self.lines = 1
            for line in iter_tree_print(buf.tree) do
                self.lines = line_nb + 1
                if line_nb >= start_line then
                    coroutine.yield(line_nb, 0, true, line)
                end
                line_nb = line_nb + 1
            end
        end)
    end

    function self.get_line_count()
        -- HACK
        return self.lines
    end
    return self
end

local function get_scroll_amount(action, window)
    if action == SCROLL_UP then
        return -1
    elseif action == SCROLL_DOWN then
        return 1
    elseif action == SCROLL_HALFPAGE_UP then
        return -bit.rshift(window.rows, 1)
    elseif action == SCROLL_HALFPAGE_DOWN then
        return bit.rshift(window.rows, 1)
    end
end

local function get_cursor_movement(action)
    if action == CURSOR_UP then
        return -1, 0
    elseif action == CURSOR_DOWN then
        return 1, 0
    elseif action == CURSOR_LEFT then
        return 0, -1
    elseif action == CURSOR_RIGHT then
        return 0, 1
    elseif action == CURSOR_HOME then
        return 0, -1e100
    elseif action == CURSOR_END then
        return 0, 1e100
    elseif action == CURSOR_NL then
        return 1, -1e100
    end
end

local function Window(buf, rows, cols, y, x)
    local self = {}
    self.buf = buf
    self.win = nc.newwin(rows, cols, y, x)
    self.rows, self.cols, self.y, self.x = rows, cols, y, x
    self.curs_line, self.curs_byte = 0, 0
    self.curs_row, self.curs_col = 0, 0
    self.attr = nil
    self.start_line = 0
    self.visual_start_line, self.visual_start_byte = nil

    function self.clear()
        self.curs_row, self.curs_col = nil, nil
        nc.werase(self.win)
    end
    function self.end_row()
        nc.wclrtoeol(self.win)
    end
    function self.refresh()
        if self.curs_row and self.curs_col then
            nc.wmove(self.win, self.curs_row, self.curs_col + LINE_NB_WIDTH)
        end
        nc.wrefresh(self.win)
    end

    function self.handle_cursor(action)
        local dy, dx = get_cursor_movement(action)
        self.curs_line = math.max(0, math.min(self.curs_line + dy,
                tonumber(buf.get_line_count() - 1)))
        -- XXX multibyte
        local len = tonumber(zmt.get_tree_line_length(self.buf.tree,
                    self.curs_line)) - 2
        self.curs_byte = math.max(0, math.min(self.curs_byte + dx,
                len))
    end

    function self.mark_cursor(row, col)
        self.curs_row, self.curs_col = row, col
    end

    function self.get_cursor()
        return self.curs_line, self.curs_byte
    end

    function self.write_at(row, col, attr, str, len)
        if attr ~= self.attr then
            nc.wattrset(self.win, HL_ATTR_IDS[attr])
            self.attr = attr
        end
        nc.mvwaddnstr(self.win, row, col, str, len)
    end

    function self.handle_scroll(action)
        local scroll = get_scroll_amount(action, self)
        local start_line = self.start_line
        start_line = start_line + scroll
        start_line = math.min(start_line, buf.tree.root.nl_count - 1)
        start_line = math.max(start_line, 0)
        -- Scroll screen contents with ncurses. This isn't really
        -- necessary, since we're going to update the whole screen,
        -- but should speed things up a bit
        nc.wscrl(self.win, start_line - self.start_line)
        self.start_line = start_line
    end

    return self
end

local function NullWindow(buf, rows, cols, y, x)
    local self = {}
    self.buf = buf
    self.rows, self.cols, self.y, self.x = rows, cols, y, x
    self.attr = nil
    self.start_line = 0
    self.row = 0

    function self.clear()
        self.row = 0
    end
    function self.end_row()
        io.stdout:write('\n')
    end
    function self.refresh() end

    function self.write_at(row, col, attr, s, len)
        if row ~= self.row then
            io.stdout:write('\n')
            self.row = row
        end
        if attr ~= self.attr then
            local fg, bg, ext = unpack(HL_ATTRS[attr])
            io.stdout:write(('\027[38;5;%dm\027[48;5;%dm'):format(fg, bg))
            if ext == 'bold' then
                io.stdout:write('\027[1m')
            elseif ext == 'inv' then
                io.stdout:write('\027[7m')
            end
            self.attr = attr
        end
        io.stdout:write(s)
    end

    function self.handle_scroll(action)
        local scroll = get_scroll_amount(action, self)
        self.start_line = self.start_line + scroll
        self.start_line = math.min(self.start_line, buf.tree.root.nl_count - 1)
        self.start_line = math.max(self.start_line, 0)
    end

    return self
end

local function run_tui(paths, debug, dumb_tui)
    local Window = Window
    local rows, cols
    if dumb_tui then
        -- Fake lines/cols information
        rows = 25 - 1
        cols = 80
        Window = NullWindow
    else
        -- ncurses setup
        local stdscr = nc.initscr()
        nc.scrollok(stdscr, true)
        nc.raw()
        nc.noecho()
        nc.nonl()
        nc.start_color()
        -- HACK: #defines aren't available, use -1
        nc.mousemask(ffi.cast('int', -1), nil)
        rows = nc.LINES - 1
        cols = nc.COLS
    end
    init_color()

    -- Set up highlighting
    local ts_ctx = TSContext()

    -- Read input files and parse them
    local buffers = {}
    for _, path in ipairs(paths) do
        local buf = Buffer(path)
        ts_ctx.parse_buf(buf)
        buffers[#buffers + 1] = buf

        if debug then
            local td_buf = TreeDebugBuffer(buf)
            buffers[#buffers + 1] = td_buf 
        end
    end

    -- Split window up to show all buffers
    local windows = {}
    local first = 0
    for i = 1, #buffers do
        local last = math.floor(rows * i / #buffers)
        windows[#windows + 1] = Window(buffers[i], last - first,
                cols, first, 0)
        first = last
    end
    local cur_win = 1
    local window = windows[cur_win]

    -- Kind of a hack: make a separate window just for the modeline
    local mode_window = Window(nil, 1, cols, rows, 0)

    local function find_window_target(row, col)
        for i, window in ipairs(windows) do
            if row >= window.y and row < window.y + window.rows and
                col >= window.x and col < window.x + window.cols then
                return i, window
            end
        end
    end

    local current_mode = NORMAL_MODE

    -- Draw all windows
    -- XXX actually track dirty status
    function draw_all()
        for i = 1, #windows do
            draw_lines(windows[i], i == cur_win)
        end
    end

    while true do
        -- Draw screen
        draw_mode_line(mode_window, current_mode)
        draw_all()
        -- Also, refresh the current window again, to make sure the cursor is
        -- in the right place
        window.refresh()

        -- Handle input
        local action, buffer, data = get_next_input(INPUT_TREES[current_mode])

        if action == QUIT then
            break
        elseif action_is_window_switch(action) then
            local offset = (action == WINDOW_NEXT and 1 or -1)
            cur_win = (cur_win + offset - 1) % #windows + 1
            window = windows[cur_win]
        elseif action == MOUSE_DOWN then
            local i, target = find_window_target(unpack(data))
            if i ~= nil then
                cur_win, window = i, target
            end
        elseif action_is_scroll(action) then
            if data then
                local _, target = find_window_target(unpack(data))
                if target then
                    target.handle_scroll(action)
                end
            else
                window.handle_scroll(action)
            end
        elseif action_is_cursor(action) then
            window.handle_cursor(action)

        ------------------------------------------------------------------------
        -- Insert Mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif action == ENTER_INSERT then
            current_mode = INSERT_MODE
            -- Split the tree at the cursor offset
            local line, byte = window.get_cursor()
            window.buf.tree = zmt.split_at_offset(window.buf.tree, line, byte)
        elseif action == EXIT_INSERT then
            current_mode = NORMAL_MODE
        elseif action == INSERT_CHAR then
            assert(current_mode == INSERT_MODE)

            local char = string.char(data)

            window.buf.tree = zmt.append_bytes_to_filler(window.buf.tree,
                    char, #char)

            zmt.verify_node(window.buf.tree.root)
            -- HACK: just re-parse the whole buffer, and leak the old ast
            ts_ctx.parse_buf(window.buf)
            -- XXX dumb?
            window.handle_cursor(data == 10 and CURSOR_NL or CURSOR_RIGHT)

        ------------------------------------------------------------------------
        -- Visual Mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif action == ENTER_VISUAL then
            current_mode = VISUAL_MODE
            window.visual_start_line, window.visual_start_byte = window.get_cursor()
        elseif action == EXIT_VISUAL then
            current_mode = NORMAL_MODE
            window.visual_start_line, window.visual_start_byte = nil

        else
            --error('unknown action', action)
        end
    end
end

function module.main(args)
    -- Argument "parsing"
    local debug_tree = false
    if arg[1] == '--debug' then
        table.remove(arg, 1)
        debug_tree = true
    end

    if #arg < 1 then
        error('no files')
    end

    -- Run the TUI, catching and printing any errors
    local res = {xpcall(run_tui, debug.traceback, args, debug_tree)}
    nc.endwin()
    print(unpack(res))
end

return module
