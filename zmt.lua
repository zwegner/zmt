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

-- Modes
local NORMAL_MODE, INSERT_MODE, VISUAL_CHAR_MODE, VISUAL_LINE_MODE,
        OPERATOR_MODE, check = enum(50)
assert(check ~= nil)
local MODE_STR = {
    [NORMAL_MODE] = '',
    [INSERT_MODE] = '-- INSERT --',
    [VISUAL_CHAR_MODE] = '-- VISUAL --',
    [VISUAL_LINE_MODE] = '-- VISUAL LINE --',
}

local function mode_is_visual(mode)
    return (mode == VISUAL_CHAR_MODE or mode == VISUAL_LINE_MODE)
end

-- Action enum
local
    -- Generic/mode switching commands
    ENTER_INSERT, EXIT_INSERT, ENTER_VISUAL_CHAR, ENTER_VISUAL_LINE,
    EXIT_VISUAL, QUIT,
    -- Operators
    _OP_MIN, OP_CHANGE, OP_DELETE, _OP_MAX,
    -- Motions
    _MOTION_MIN, MOTION_UP, MOTION_DOWN, MOTION_LEFT, MOTION_RIGHT,
    MOTION_HOME, MOTION_END, MOTION_FIRST, MOTION_LAST, MOTION_NL, _MOTION_MAX,
    -- Insert mode
    INSERT_CHAR,
    -- Scrolling
    _SCROLL_MIN, SCROLL_UP, SCROLL_DOWN, SCROLL_HALFPAGE_UP,
    SCROLL_HALFPAGE_DOWN, _SCROLL_MAX,
    -- Mouse events
    MOUSE_DOWN,
    -- Window movement
    _WINDOW_SWITCH_MIN, WINDOW_NEXT, WINDOW_PREV, _WINDOW_SWITCH_MAX,
    check = enum(100)
assert(check ~= nil)

-- Motion properties
local M_LINEWISE, M_CHARWISE, M_INC, M_EXC, check = enum(50)
assert(check ~= nil)

-- Render event enum
local EV_HL_BEGIN, EV_HL_END, EV_WRAP, EV_CURSOR, EV_EOF,
        EV_VISUAL_BEGIN, EV_VISUAL_END, check = enum(50)
assert(check ~= nil)

-- Color codes for syntax highlighting. Each value is a (fg, bg) pair
local ATTR_INFO = {
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
local ATTR_ID = {}
local idx = 1
for k, v in pairs(ATTR_INFO) do
    ATTR_ID[k] = idx
    idx = idx + 1
end

local LINE_NB_FMT = '%4d '
local LINE_NB_WIDTH = 5

local POS_META = {
    __eq = function (a, b)
        return a.line == b.line and a.byte == b.byte
    end,
    __lt = function (a, b)
        return a.line < b.line or (a.line == b.line and a.byte < b.byte)
    end
}

local function Pos(line, byte)
    local self = {}
    setmetatable(self, POS_META)
    self.line, self.byte = line, byte
    function self.copy()
        return Pos(self.line, self.byte)
    end
    function self.delta(d_l, d_b)
        return Pos(self.line + d_l, self.byte + d_b)
    end
    return self
end

--------------------------------------------------------------------------------
-- Main meta-tree interface ----------------------------------------------------
--------------------------------------------------------------------------------

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
function module.iter_lines(tree, start_line, start_byte)
    return coroutine.wrap(function()
        -- Start a piece iterator at the start line
        local line = start_line
        for iter, node, piece in module.iter_nodes(tree,
                start_line, start_byte) do
            -- Chop up this piece into lines. We only do the scan the exact
            -- number of times needed
            local offset = tonumber(iter.start_offset.byte)
            for i = 0, node.nl_count - 1 do
                -- Cut off any part after a newline
                local idx = piece:find('\n')
                assert(idx ~= nil)
                local part = piece:sub(1, idx - 1)
                piece = piece:sub(idx + 1)
                coroutine.yield(line, offset, true, part)
                offset = offset + idx
                line = line + 1
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

    if bit.band(node.flags, zmt.NODE_LEAF + zmt.NODE_FILLER) ~= 0 then
        if bit.band(node.flags, zmt.NODE_FILLER) ~= 0 then
            prefix = prefix .. ' FILLER'
        end
        local l = node.leaf
        local data = ffi.string(l.chunk_data + l.start, l['end'] - l.start)
        coroutine.yield(fmt(prefix, str(data)))
    elseif bit.band(node.flags, zmt.NODE_HOLE) ~= 0 then
        coroutine.yield(fmt(prefix, 'HOLE'))
        co_tree_print(tree, tree.filler_node[0], depth+1)
    else
        coroutine.yield(prefix)
        for i = 0, zmt.MAX_CHILDREN-1 do
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
            local query = ts.ts_query_new(lang, q_text, #q_text, offset,
                    error_type)
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
    local visual_start, visual_end = nil, nil
    local visual_start_off, visual_end_off = nil, nil

    -- Event buffer, for managing nested highlights
    local event_buf = {}
    local event_start, event_end = 0, 0
    local next_hl_event = {-1, nil, nil}

    function self.reset(line, offset)
        -- Start the tree-sitter query over
        query.reset(offset)
        visual_start, visual_end = window.get_visual_range()
        if visual_start and visual_start.line < line then
            visual_start = Pos(line, offset)
        end
    end

    function self.mark_line_start(line, offset)
        -- Set next potential line wrap event
        next_wrap_offset = offset + width
        -- If this is the line with the cursor, set up an event
        cursor_offset = (line == window.cursor.line and
                offset + window.cursor.byte)
        -- Same, but for visual mode
        visual_start_off = (visual_start and line == visual_start.line and
                offset + visual_start.byte)
        visual_end_off = (visual_end and line == visual_end.line and
                offset + visual_end.byte)
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
            {visual_start_off, EV_VISUAL_BEGIN, 'visual'},
            {visual_end_off, EV_VISUAL_END, 'visual'},
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
            visual_start_off = nil
        elseif event_type == EV_VISUAL_END then
            visual_end_off = nil
        elseif event_type == EV_WRAP then
            next_wrap_offset = next_wrap_offset + width
        else
            self.next_capture_event()
        end
    end

    return self
end

local function draw_number_column(window, row, line, wrap, skip)
    local line_nb_str
    if wrap then
        line_nb_str = (' '):rep(LINE_NB_WIDTH)
    else
        line_nb_str = LINE_NB_FMT:format(line + 1):sub(1, LINE_NB_WIDTH)
        if skip then
            line_nb_str = line_nb_str:gsub(' ', '-')
        end
    end
    window.write_at(row, 0, ATTR_ID['line_nb'], line_nb_str, LINE_NB_WIDTH)
end

local function draw_status_line(window, is_focused)
    local attr = is_focused and ATTR_ID['status'] or ATTR_ID['status-unfocused']
    local status_line = right_pad(window.buf.path, window.cols)
    window.write_at(window.rows - 1, 0, attr, status_line, #status_line)
    window.clear_line(window.rows - 1, #status_line, attr)
end

local function draw_mode_line(window, mode)
    local attr = ATTR_ID['mode_line']
    local mode_line = MODE_STR[mode]
    window.write_at(window.rows - 1, 0, attr, mode_line, #mode_line)
    window.clear_line(window.rows - 1, #mode_line, attr)
end

function module.draw_lines(window, is_focused)
    local buf = window.buf
    window.clear()

    -- Render event handling
    local event_ctx = EventContext(window, buf.query,
            window.cols - LINE_NB_WIDTH)
    -- The current render event we're waiting for
    local event_offset, event_type, event_hl = -1, nil, nil
    -- A stack of highlights, so highlights can nest
    local hl_stack = {ATTR_ID['default']}
    -- The current highlight value
    local cur_hl = ATTR_ID['default']
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
    local function handle_event(is_end, piece)
        -- Start highlight
        if event_type == EV_HL_BEGIN then
            -- Push this highlight. Duplicate the top stack if this is
            -- an ignored capture, so we can still pop it off later
            cur_hl = ATTR_ID[event_hl] or hl_stack[#hl_stack]
            hl_stack[#hl_stack + 1] = cur_hl
        -- End highlight
        elseif event_type == EV_HL_END then
            -- End of capture, pop the highlight stack
            assert(#hl_stack > 1)
            hl_stack[#hl_stack] = nil
            cur_hl = hl_stack[#hl_stack]
        elseif event_type == EV_VISUAL_BEGIN then
            visual_hl = ATTR_ID['visual']
        elseif event_type == EV_VISUAL_END then
            visual_hl = nil
        -- Line wrap. HACK: make sure to not wrap if this is the last
        -- character of a line
        elseif event_type == EV_WRAP and (not is_end or #piece > 0) then
            row = row + 1
            col = LINE_NB_WIDTH
            draw_number_column(window, row, line, true, false)
        -- Mark cursor
        elseif event_type == EV_CURSOR then
            window.mark_cursor(row, col - LINE_NB_WIDTH)
        end
        next_event()
    end

    -- Iterate through all lines in the file
    local first = true
    for line, offset, is_end, piece in buf.iter_lines_from(window.start_line,
            window.start_byte) do
        -- On the very first byte offset, update EventContext
        if first then
            first = false
            event_ctx.reset(line, offset)
        end
        -- Line number display
        if line ~= last_line then
            event_ctx.mark_line_start(line, offset)
            set_event()

            local skip = false
            -- When drawing the very first line, read through events to get
            -- the current highlight state, which might start above the screen
            if last_line == -1 then
                while offset > event_offset do
                    handle_event(false, '')
                end
                cur_hl = hl_stack[#hl_stack]
                skip = window.start_byte > 0
            end

            draw_number_column(window, row, line, false, skip)
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
                handle_event(is_end, piece)
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

-- Input helper functions

local function action_is_operator(action)
    return action >= _OP_MIN and action <= _OP_MAX
end
local function action_is_scroll(action)
    return action >= _SCROLL_MIN and action <= _SCROLL_MAX
end
local function action_is_motion(action)
    return action >= _MOTION_MIN and action <= _MOTION_MAX
end
local function action_is_window_switch(action)
    return action >= _WINDOW_SWITCH_MIN and action <= _WINDOW_SWITCH_MAX
end

local function CTRL(x) return string.char(bit.band(string.byte(x), 0x1f)) end

local function byte_in_range(c, a, b)
    return (c >= string.byte(a) and c <= string.byte(b))
end

-- Input sequences, per mode

local MOTION_TABLE = {
    ['h']               = MOTION_LEFT,
    ['j']               = MOTION_DOWN,
    ['k']               = MOTION_UP,
    ['l']               = MOTION_RIGHT,
    ['0']               = MOTION_HOME,
    ['$']               = MOTION_END,
    ['gg']              = MOTION_FIRST,
    ['G']               = MOTION_LAST,
}

local OPERATOR_TABLE = {
    ['c']               = OP_CHANGE,
    ['d']               = OP_DELETE,
}

local INPUT_TABLES = {
    [NORMAL_MODE] = {
        ['QQ']              = QUIT,

        ['i']               = ENTER_INSERT,
        ['v']               = ENTER_VISUAL_CHAR,
        ['V']               = ENTER_VISUAL_LINE,

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
    [VISUAL_CHAR_MODE] = {
        ['\027']            = EXIT_VISUAL,
        ['v']               = EXIT_VISUAL,
        ['V']               = ENTER_VISUAL_LINE,
    },
    [VISUAL_LINE_MODE] = {
        ['\027']            = EXIT_VISUAL,
        ['v']               = ENTER_VISUAL_CHAR,
        ['V']               = EXIT_VISUAL,
    },
    [OPERATOR_MODE] = {
        ['.']               = EXIT_OPERATOR,
    },
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

-- Post-processing: add operators and motions
local op_tree = parse_input_table(OPERATOR_TABLE)
local motion_tree = parse_input_table(MOTION_TABLE)
for _, mode in ipairs{NORMAL_MODE, VISUAL_CHAR_MODE, VISUAL_LINE_MODE,
        OPERATOR_MODE} do
    INPUT_TREES[mode] = merge(INPUT_TREES[mode], op_tree)
    INPUT_TREES[mode] = merge(INPUT_TREES[mode], motion_tree)
end

-- Read input until we parse a command
local function InputHandler()
    local self = {}
    local matches = nil
    local enable_count, done_count, count = nil, nil, nil
    local input, action, data = nil, nil, nil

    function self.reset(mode)
        input_tree = INPUT_TREES[mode]
        -- Store all current input sequence matches
        matches = {}
        -- HACK
        enable_count = (mode == NORMAL_MODE or mode == OPERATOR_MODE or
                mode_is_visual(mode))
        done_count = false
        count = nil
        input, action, data = nil, nil, nil
    end

    function self.feed(c)
        -- Parse count
        if enable_count and not done_count then
            if count == nil and byte_in_range(c, '1', '9') then
                count = c - string.byte('0')
                return nil
            elseif count ~= nil and byte_in_range(c, '0', '9') then
                count = count * 10 + (c - string.byte('0'))
                return nil
            end
            done_count = true
            matches = {}
        end

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
                    input, action = prefix, lookup
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
            input, action = {c}, lookup
        end

        -- Check for a successful input match
        if type(action) == 'function' then
            action, data = action(input)
        end

        if action ~= nil then
            return action, count, input, data
        -- Clear out count if no matches are in progress
        elseif #matches == 0 then
            count = nil
            done_count = false
        end
        return nil
    end
    return self
end

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

local function Buffer(path)
    local self = {}
    self.path = path
    self.chunk = zmt.map_file(path)
    self.tree = zmt.dumb_read_data(self.chunk, 0)

    function self.get_line_count()
        return tonumber(zmt.get_tree_total_size(self.tree).line) + 1
    end

    function self.get_line_len(line)
        -- XXX multibyte
        return tonumber(zmt.get_tree_line_length(self.tree, line))
    end

    function self.iter_lines_from(start_line, start_byte)
        return module.iter_lines(self.tree, start_line, start_byte)
    end

    return self
end

local function TreeDebugBuffer(buf)
    local self = {}
    self.query = TSNullQuery()
    self.path = '[tree-debug]'
    self.lines = 0

    function self.iter_lines_from(start_line, start_byte)
        -- XXX ignore start_byte
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

local function get_motion_props(buf, raw_count, action, start)
    local count = raw_count or 1
    if action == MOTION_UP then
        return start.delta(-count, 0), M_LINEWISE, M_INC
    elseif action == MOTION_DOWN then
        return start.delta(count, 0), M_LINEWISE, M_INC
    elseif action == MOTION_LEFT then
        return start.delta(0, -count), M_CHARWISE, M_EXC
    elseif action == MOTION_RIGHT then
        return start.delta(0, count), M_CHARWISE, M_EXC
    elseif action == MOTION_HOME then
        return Pos(start.line, 0), M_CHARWISE, M_EXC
    elseif action == MOTION_END then
        local len = buf.get_line_len(start.line) - 1
        -- Sorta hacky: use exclusive motion here instead of inclusive like vim,
        -- since right now we're counting the final newline in get_line_len()
        return Pos(start.line, len), M_CHARWISE, M_EXC
    elseif action == MOTION_FIRST then
        return Pos(raw_count and (raw_count - 1) or 0, 0), M_LINEWISE, M_INC
    elseif action == MOTION_LAST then
        local last = buf.get_line_count() - 1
        return Pos(raw_count and (raw_count - 1) or last, 0), M_LINEWISE, M_INC
    elseif action == MOTION_NL then
        return Pos(start.line + 1, 0), M_LINEWISE, M_INC
    end
end

function module.Window(buf, rows, cols)
    local self = {}
    self.buf = buf
    self.rows, self.cols = rows, cols
    self.cursor = Pos(0, 0)
    self.curs_row, self.curs_col = 0, 0
    self.attr = nil
    self.start_line, self.start_byte = 0, 0
    self.visual_mode = nil
    self.visual_start, self.visual_end = nil, nil
    -- XXX multibyte
    ffi.cdef([[
        typedef struct {
            uint8_t ch;
            uint8_t attr;
        } grid_cell_t;]])
    -- Meh, luajit doesn't allow multidimensional variable arrays
    self.grid = ffi.new('grid_cell_t['..rows..']['..cols..']')
    local default_attr = ATTR_ID['default']

    function self.clear()
        self.curs_row, self.curs_col = nil, nil
        for r = 0, self.rows - 1 do
            for c = 0, self.cols - 1 do
                self.grid[r][c].ch = 32
                self.grid[r][c].attr = default_attr
            end
        end
    end

    function self.clear_line(row, start_col, attr)
        for c = start_col, self.cols - 1 do
            self.grid[row][c].ch = 32
            self.grid[row][c].attr = attr
        end
    end

    function self.refresh()
        if self.curs_row and self.curs_col then
            nc.wmove(self.win, self.curs_row, self.curs_col + LINE_NB_WIDTH)
        end
        nc.wrefresh(self.win)
    end

    function self.get_motion_props(count, action)
        local cursor, mtype, inc = get_motion_props(self.buf, count, action,
                self.cursor)
        return self.clip_cursor(cursor), mtype, inc
    end

    function self.handle_cursor(count, action)
        self.cursor = self.get_motion_props(count, action)

        -- XXX handle start_byte
        if self.cursor.line < self.start_line then
            self.start_line = self.cursor.line
        -- XXX line/row size. also, # of rows with status line
        elseif self.cursor.line > self.start_line + self.rows - 2 then
            self.start_line = self.cursor.line - self.rows + 2
        end
        self.clip_view()
    end

    function self.mark_cursor(row, col)
        self.curs_row, self.curs_col = row, col
    end

    function self.write_at(row, col, attr, str, len)
        local c = 0
        while col + c < self.cols and c < len do
            self.grid[row][col + c].ch = str:byte(c+1, c+1)
            self.grid[row][col + c].attr = attr
            c = c + 1
        end
    end

    function self.clip_cursor(cursor)
        cursor.line = math.max(0, math.min(cursor.line,
                self.buf.get_line_count() - 1))
        local len = self.buf.get_line_len(cursor.line) - 2
        cursor.byte = math.max(0, math.min(cursor.byte, len))
        return cursor
    end

    function self.clip_view()
        self.start_line = math.max(0, math.min(self.start_line,
                self.buf.get_line_count() - 1))
    end

    function self.handle_scroll(action)
        local scroll = get_scroll_amount(action, self)
        local width = self.cols - LINE_NB_WIDTH
        -- Scroll down
        if scroll > 0 then
            local len = self.buf.get_line_len(self.start_line) - 1
            local line_count = self.buf.get_line_count()
            while scroll > 0 and self.start_line < line_count do
                if self.start_byte + width < len then
                    self.start_byte = self.start_byte + width
                else
                    self.start_line = self.start_line + 1
                    self.start_byte = 0
                    len = self.buf.get_line_len(self.start_line) - 1
                end
                scroll = scroll - 1
            end
        -- Scroll up
        else
            while scroll < 0 and (self.start_line > 0 or self.start_byte > 0) do
                if self.start_byte > 0 then
                    self.start_byte = self.start_byte - width
                else
                    self.start_line = self.start_line - 1
                    local len = self.buf.get_line_len(self.start_line) - 1
                    self.start_byte = len - (len % width)
                end
                scroll = scroll + 1
            end
            self.start_byte = math.max(self.start_byte, 0)
        end

        self.clip_view()

        -- XXX update cursor
    end

    -- Visual mode
    function self.start_visual(mode, cursor)
        self.visual_start = cursor.copy()
        self.update_visual(mode, cursor)
    end
    function self.update_visual(mode, cursor)
        self.visual_mode = mode
        self.visual_end = cursor.copy()
    end
    function self.get_visual_range()
        if not self.visual_mode then
            return nil, nil
        end
        local start, stop = self.visual_start.copy(), self.visual_end.copy()
        -- Ensure start < end
        if start > stop then
            start, stop = stop, start
        end
        -- Update for line-wise visual
        if self.visual_mode == VISUAL_LINE_MODE then
            start.byte = 0
            stop.byte = self.buf.get_line_len(stop.line) - 2
        end
        return start, stop.delta(0, 1)
    end
    function self.end_visual()
        self.visual_mode = nil
        self.visual_start, self.visual_end = nil, nil
    end

    return self
end

local function NCursesUI()
    local self = {}

    -- ncurses setup
    local stdscr = nc.initscr()
    nc.scrollok(stdscr, true)
    nc.raw()
    nc.noecho()
    nc.nonl()
    nc.start_color()
    -- HACK: #defines aren't available, use -1
    nc.mousemask(ffi.cast('int', -1), nil)
    self.rows = nc.LINES - 1
    self.cols = nc.COLS
    self.windows = {}

    -- Initialize attributes
    local attr_value = {}
    for k, idx in pairs(ATTR_ID) do
        local fg, bg, ext = unpack(ATTR_INFO[k])
        nc.init_pair(idx, fg, bg)
        -- HACK: hardcoded bit offsets, since we don't have the #defines here
        local attr = bit.lshift(idx, 8)
        if ext == 'bold' then
            attr = attr + bit.lshift(1, 8+13)
        elseif ext == 'inv' then
            attr = attr + bit.lshift(1, 8+10)
        end
        attr_value[idx] = attr
    end

    -- Sort of weird UI: insert a window at the given index, so window IDs match
    function self.add_window(idx, window, rows, cols, y, x)
        self.windows[idx] = {
            rows=rows, cols=cols, y=y, x=x,
            window=window,
            nc_win=nc.newwin(rows, cols, y, x),
        }
    end

    -- Kind of a hack: make a separate window just for the modeline
    self.mode_window = module.Window(nil, 1, self.cols)
    self.add_window('mode', self.mode_window, 1, self.cols, self.rows, 0)

    -- Draw screen
    function self.draw(cur_win, current_mode)
        for win_idx, window in pairs(self.windows) do
            local win = window.window
            local nc_win = window.nc_win

            -- meh
            if win == self.mode_window then
                draw_mode_line(win, current_mode)
            else
                -- XXX actually track dirty status
                module.draw_lines(win, win_idx == cur_win)
            end

            nc.werase(nc_win)

            -- Draw the grid
            for r = 0, win.rows - 1 do
                local cur_attr = nil
                for c = 0, win.cols - 1 do
                    local attr = win.grid[r][c].attr
                    if attr > 0 and attr ~= cur_attr then
                        nc.wattrset(nc_win, attr_value[attr])
                        cur_attr = attr
                    end

                    nc.mvwaddch(nc_win, r, c, win.grid[r][c].ch)
                end
            end
            if win_idx == cur_win and win.curs_row and win.curs_col then
                nc.wmove(nc_win, win.curs_row, win.curs_col + LINE_NB_WIDTH)
            end
            nc.wrefresh(nc_win)
        end
        -- Refresh the current window again, to make sure the cursor is
        -- in the right place
        nc.wrefresh(self.windows[cur_win].nc_win)
    end

    function self.find_window_target(row, col)
        for i, window in ipairs(self.windows) do
            if row >= window.y and row < window.y + window.rows and
                col >= window.x and col < window.x + window.cols then
                return i, window.window
            end
        end
    end

    function self.stop()
        nc.endwin()
    end
    return self
end

local function run_ui(ui, paths, debug)
    local rows, cols

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
        local last = math.floor(ui.rows * i / #buffers)
        local rows, cols = last - first, ui.cols
        local window = module.Window(buffers[i], rows, cols)
        local idx = #windows + 1
        windows[idx] = window
        ui.add_window(idx, window, rows, cols, first, 0)
        first = last
    end
    local cur_win = 1
    local window = windows[cur_win]

    local current_mode = NORMAL_MODE
    -- Action and count for operator pending mode
    local op_action, op_count = nil, nil

    -- Handle an operator on a given range
    local function operate(window, start, stop, action, mtype, inc)
        if start > stop then
            start, stop = stop, start
        end

        -- Handle linewise/charwise and inclusive/exclusive
        if mtype == M_LINEWISE then
            -- XXX ignore inclusive/exclusive
            start.byte = 0
            -- Sorta hacky: linewise change operator expects to get a new
            -- line when starting the insert, so cut off before the last byte
            if action == OP_CHANGE then
                stop.byte = window.buf.get_line_len(stop.line) - 1
            else
                stop.line = stop.line + 1
                stop.byte = 0
            end
        elseif inc == M_INC then
            stop.byte = stop.byte + 1
        end

        local start_off = zmt.get_abs_byte_offset(window.buf.tree,
                start.line, start.byte)
        local stop_off = zmt.get_abs_byte_offset(window.buf.tree,
                stop.line, stop.byte)

        if start_off < stop_off then
            window.buf.tree = zmt.delete_byte_range(window.buf.tree,
                    start_off, stop_off)
            -- HACK: just re-parse the whole buffer, and leak the old ast
            ts_ctx.parse_buf(window.buf)
        end
        window.cursor = start
        if action == OP_CHANGE then
            window.buf.tree = zmt.split_at_offset(window.buf.tree,
                    window.cursor.line, window.cursor.byte)
            return INSERT_MODE
        end
        return NORMAL_MODE
    end

    local input_handler = InputHandler()
    -- Input buffer
    local buf_size = 64
    local in_buf = ffi.new('uint8_t[?]', buf_size)
    local n_read, n_used = 0, 0
    -- XXX for some reason just #include-ing unistd.h makes this function not found...
    ffi.cdef('ssize_t read(int, void *, size_t);')

    while true do
        -- Only draw the screen if we've processed the whole input buffer
        if n_used >= n_read then
            ui.draw(cur_win, current_mode)
        end

        -- Handle input
        local action, count, input, data
        input_handler.reset(current_mode)
        while true do
            -- Read a chunk of input from stdin if our buffer is empty
            while n_used >= n_read do
                n_read = ffi.C.read(0, in_buf, buf_size)
                n_used = 0
            end
            local char = in_buf[n_used]
            n_used = n_used + 1
            action, count, input, data = input_handler.feed(char)
            if action ~= nil then break end
        end

        -- Clear out any pending operator
        if current_mode ~= OPERATOR_MODE then
            op_action, op_count = nil, nil
        elseif action_is_operator(action) and op_action ~= action then
            op_action, op_count = nil, nil
            current_mode = NORMAL_MODE
        end

        ------------------------------------------------------------------------
        -- Universal commands --------------------------------------------------
        ------------------------------------------------------------------------
        if action == QUIT then
            break
        elseif action_is_window_switch(action) then
            local offset = (action == WINDOW_NEXT and 1 or -1)
            cur_win = (cur_win + offset - 1) % #windows + 1
            window = windows[cur_win]
        elseif action == MOUSE_DOWN then
            local i, target = ui.find_window_target(unpack(data))
            if i ~= nil then
                cur_win, window = i, target
            end
        elseif action_is_scroll(action) then
            if data then
                local _, target = ui.find_window_target(unpack(data))
                if target then
                    target.handle_scroll(action)
                end
            else
                window.handle_scroll(action)
            end

        elseif action_is_motion(action) then
            if op_action then
                -- Multiply counts unless both are nil, then pass nil
                local count = op_count and (op_count * (count or 1)) or count
                local stop, mtype, inc = window.get_motion_props(
                        count, action)
                current_mode = operate(window, window.cursor, stop,
                        op_action, mtype, inc)
            else
                window.handle_cursor(count, action)
                if mode_is_visual(current_mode) then
                    window.update_visual(current_mode, window.cursor)
                end
            end

        ------------------------------------------------------------------------
        -- Normal mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif current_mode == NORMAL_MODE and action_is_operator(action) then
            op_action, op_count = action, count
            current_mode = OPERATOR_MODE

        ------------------------------------------------------------------------
        -- Operator mode -------------------------------------------------------
        ------------------------------------------------------------------------

        -- Handle double operators: cc, dd, etc.
        elseif current_mode == OPERATOR_MODE and action_is_operator(action) then
            assert(action == op_action)
            -- Multiply counts unless both are nil, then pass 1 (unlike default
            -- counts for motions, which pass nil)
            local count = op_count and (op_count * (count or 1)) or count or 1
            -- Double operator with count works like [count - 1]j
            local stop, mtype, inc = window.get_motion_props(
                    count - 1, MOTION_DOWN)
            current_mode = operate(window, window.cursor, stop,
                    op_action, mtype, inc)

        ------------------------------------------------------------------------
        -- Insert Mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif action == ENTER_INSERT then
            current_mode = INSERT_MODE
            -- Split the tree at the cursor offset
            window.buf.tree = zmt.split_at_offset(window.buf.tree,
                    window.cursor.line, window.cursor.byte)
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
            window.handle_cursor(1, data == 10 and MOTION_NL or MOTION_RIGHT)

        ------------------------------------------------------------------------
        -- Visual Mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif action == ENTER_VISUAL_CHAR or action == ENTER_VISUAL_LINE then
            local next_mode = (action == ENTER_VISUAL_CHAR) and VISUAL_CHAR_MODE
                    or VISUAL_LINE_MODE
            if mode_is_visual(current_mode) then
                window.update_visual(next_mode, window.cursor)
            else
                window.start_visual(next_mode, window.cursor)
            end
            current_mode = next_mode
        elseif action == EXIT_VISUAL then
            current_mode = NORMAL_MODE
            window.end_visual()
        elseif mode_is_visual(current_mode) and action_is_operator(action) then
            mtype = (current_mode == VISUAL_CHAR_MODE) and M_CHARWISE or
                    M_LINEWISE
            local start, stop = window.get_visual_range()
            current_mode = operate(window, start, stop, action, mtype, M_EXC)
            window.end_visual()
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

    local ui = NCursesUI()

    -- Run the TUI, catching and printing any errors
    local res = {xpcall(run_ui, debug.traceback, ui, args, debug_tree)}
    ui.stop()
    print(unpack(res))
end

return module
