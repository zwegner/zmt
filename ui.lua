require('stdlib')
local ffi = require('ffi')
local zmt = require('zmt')
local ed = require('ed')
local ACT, MODE, Pos = ed.ACT, ed.MODE, ed.Pos
local input = require('input')
local ts = require('ts')

local module = {}

local lib = zmt.lib
-- Alias for ncurses
local nc = lib

-- Render events
local EV = enum('EV', [[HL_BEGIN HL_END WRAP CURSOR EOF
        VISUAL_BEGIN VISUAL_END]])

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
ATTR_NAME = {}
local idx = 1
for k, v in pairs(ATTR_INFO) do
    ATTR_ID[k] = idx
    ATTR_NAME[idx] = k
    idx = idx + 1
end

local LINE_NB_FMT = '%4d '
local LINE_NB_WIDTH = 5

local function WPos(row, col)
    local self = {}
    self.row, self.col = row, col
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
        if visual_start and visual_start < window.win_start then
            visual_start = Pos(line, 0)
        end
        if visual_end and visual_end < window.win_start then
            visual_end = Pos(line, 0)
        end
    end

    function self.mark_line_start(line, offset, abs_off)
        -- Set next potential line wrap event
        next_wrap_offset = offset + width
        -- If this is the line with the cursor, set up an event
        cursor_offset = (line == window.cursor.line and
                abs_off + window.cursor.byte)
        -- Same, but for visual mode
        visual_start_off = (visual_start and line == visual_start.line and
                abs_off + visual_start.byte)
        visual_end_off = (visual_end and line == visual_end.line and
                abs_off + visual_end.byte)
    end

    function self.next_capture_event()
        -- If the buffer is exhausted, grab a new capture, and fill in any
        -- nested captures
        if event_start >= event_end then
            local cap_name, cap_start, cap_end = query.current_capture()
            if cap_name == nil then
                next_hl_event = {1e100, EV.EOF, nil}
                return
            end

            event_buf = {{cap_start, EV.HL_BEGIN, cap_name},
                    {cap_end, EV.HL_END, nil}}

            -- Fill in any nested captures that happen within this one
            while true do
                query.next_capture()
                local n, s, e = query.current_capture()
                if n == nil or s >= cap_end then
                    break
                end
                event_buf[#event_buf+1] = {s, EV.HL_BEGIN, n}
                event_buf[#event_buf+1] = {e, EV.HL_END, nil}
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
            {next_wrap_offset, EV.WRAP, nil},
            {visual_start_off, EV.VISUAL_BEGIN, 'visual'},
            {visual_end_off, EV.VISUAL_END, 'visual'},
            {cursor_offset, EV.CURSOR, nil},
            {next_wrap_offset, EV.WRAP, nil},
        }
        local event = next_hl_event
        for _, e in ipairs(events) do
            if e[1] and e[1] < event[1] then event = e end
        end
        return event
    end

    -- Advance the event queue
    function self.next_event(event_type)
        if event_type == EV.CURSOR then
            cursor_offset = nil
        elseif event_type == EV.VISUAL_BEGIN then
            visual_start_off = nil
        elseif event_type == EV.VISUAL_END then
            visual_end_off = nil
        elseif event_type == EV.WRAP then
            next_wrap_offset = next_wrap_offset + width
        else
            self.next_capture_event()
        end
    end

    return self
end

local function draw_number_column(window, row, line, wrap, skip)
    local line_nb_str
    if not window.show_line_numbers then
        return 0
    end
    if wrap then
        line_nb_str = (' '):rep(LINE_NB_WIDTH)
    else
        line_nb_str = LINE_NB_FMT:format(line + 1):sub(1, LINE_NB_WIDTH)
        if skip then
            line_nb_str = line_nb_str:gsub(' ', '-')
        end
    end
    window.write_at(row, 0, ATTR_ID['line_nb'], line_nb_str, LINE_NB_WIDTH)
    return LINE_NB_WIDTH
end

local function draw_status_line(window, is_focused)
    local attr = is_focused and ATTR_ID['status'] or ATTR_ID['status-unfocused']
    local status_line = right_pad(window.buf.path, window.cols)
    window.write_at(window.rows - 1, 0, attr, status_line, #status_line)
    window.clear_line(window.rows - 1, #status_line, attr)
end

local function draw_mode_line(window, mode)
    local attr = ATTR_ID['mode_line']
    local mode_line = ed.MODE_STR[mode]
    window.write_at(window.rows - 1, 0, attr, mode_line, #mode_line)
    window.clear_line(window.rows - 1, #mode_line, attr)
end

function module.draw_lines(window, is_focused)
    local buf = window.buf
    window.clear()

    -- Render event handling
    local event_ctx = EventContext(window, buf.query, window.inner_width())
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
        if event_type == EV.HL_BEGIN then
            -- Push this highlight. Duplicate the top stack if this is
            -- an ignored capture, so we can still pop it off later
            cur_hl = ATTR_ID[event_hl] or hl_stack[#hl_stack]
            hl_stack[#hl_stack + 1] = cur_hl
        -- End highlight
        elseif event_type == EV.HL_END then
            -- End of capture, pop the highlight stack
            assert(#hl_stack > 1)
            hl_stack[#hl_stack] = nil
            cur_hl = hl_stack[#hl_stack]
        elseif event_type == EV.VISUAL_BEGIN then
            visual_hl = ATTR_ID['visual']
        elseif event_type == EV.VISUAL_END then
            visual_hl = nil
        -- Line wrap. HACK: make sure to not wrap if this is the last
        -- character of a line
        elseif event_type == EV.WRAP and (not is_end or #piece > 0) then
            row = row + 1
            col = draw_number_column(window, row, line, true, false)
        -- Mark cursor
        elseif event_type == EV.CURSOR then
            window.mark_cursor(row, col)
        end
        next_event()
    end

    -- Iterate through all lines in the file
    local first = true
    for line, offset, is_end, piece in buf.iter_lines_from(window.win_start.line,
            window.win_start.byte) do
        -- On the very first byte offset, update EventContext
        if first then
            first = false
            event_ctx.reset(line, offset)
        end
        -- Line number display
        if line ~= last_line then
            local skip = (last_line == -1) and window.win_start.byte or 0
            local abs_off = offset - skip
            event_ctx.mark_line_start(line, offset, abs_off)
            set_event()

            -- When drawing the very first line, read through events to get
            -- the current highlight state, which might start above the screen
            if last_line == -1 then
                while offset > event_offset do
                    handle_event(false, '')
                end
                cur_hl = hl_stack[#hl_stack]
            end

            col = draw_number_column(window, row, line, false, skip > 0)
            last_line = line

            -- HACK: add a space on empty lines
            if piece == '\n' then
                piece = ' '
            end
        end
        -- HACK: trim off final newline
        if is_end and piece:sub(-1, -1) == '\n' then
            piece = piece:sub(1, -2)
        end

        -- Chop up this piece into parts separated by events from event_ctx
        while #piece > 0 and row < window.rows - 1 do
            -- Skip over any events we've already gone past
            while offset > event_offset do
                next_event()
            end

            -- We've reached an event, update the current state
            while offset == event_offset do
                handle_event(is_end, piece)
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
        end

        -- Move to the next line if this piece had a newline
        if is_end then
            -- Handle any remaining events for this line
            while offset == event_offset do
                handle_event(true, '')
            end
            row = row + 1
        end
        -- Check row count
        if row >= window.rows - 1 then break end
    end

    draw_status_line(window, is_focused)
end

-- Create a friendly table-of-strings representation of a window grid
function module.get_grid_lines(win, use_attrs)
    local grid = {}
    local cur_attr = 'default'
    assert(win.w_cursor ~= nil)
    local c_row, c_col = win.w_cursor.row, win.w_cursor.col
    for r = 0, win.rows - 1 do
        local line = ''
        for c = 0, win.cols - 1 do
            -- Deal with attributes
            local attr = ATTR_NAME[win.grid[r][c].attr]
            if use_attrs and attr ~= cur_attr then
                if cur_attr ~= 'default' then
                    line = line .. '}'
                end
                cur_attr = attr
                if attr ~= 'default' then
                    line = line .. '{' .. attr .. ':'
                end
            end
            -- Deal with cursor
            if r == c_row and c == c_col then
                line = line .. '^'
            end

            line = line .. string.char(win.grid[r][c].ch)
        end
        -- Finish any attributes on the last row
        if r == win.rows - 1 and use_attrs and cur_attr ~= 'default' then
            line = line .. '}'
        end
        grid[#grid + 1] = line
    end
    return grid
end

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

local function get_scroll_amount(action, window)
    if action == ACT.SCROLL_UP then
        return -1
    elseif action == ACT.SCROLL_DOWN then
        return 1
    elseif action == ACT.SCROLL_HALFPAGE_UP then
        return -bit.rshift(window.rows, 1)
    elseif action == ACT.SCROLL_HALFPAGE_DOWN then
        return bit.rshift(window.rows, 1)
    end
end

function module.Window(buf, rows, cols)
    local self = {}
    self.buf = buf
    self.rows, self.cols = rows, cols
    self.cursor = Pos(0, 0)
    self.w_cursor = nil
    self.win_start = Pos(0, 0)
    self.visual_mode = nil
    self.visual_start, self.visual_end = nil, nil
    -- Change tracking
    local dirty, last_focused, last_state = true, nil, nil

    self.show_line_numbers = true

    -- XXX multibyte
    ffi.cdef([[
        typedef struct {
            uint8_t ch;
            uint8_t attr;
        } grid_cell_t;]])
    -- Meh, luajit doesn't allow multidimensional variable arrays
    self.grid = ffi.new('grid_cell_t['..rows..']['..cols..']')
    local default_attr = ATTR_ID['default']

    -- Drawing functions

    function self.clear()
        self.w_cursor = nil
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
    function self.write_at(row, col, attr, str, len)
        local c = 0
        while col + c < self.cols and c < len do
            self.grid[row][col + c].ch = str:byte(c+1, c+1)
            self.grid[row][col + c].attr = attr
            c = c + 1
        end
    end

    function self.render(is_focused)
        -- Check if we actually need to render
        local buf_state = buf.refresh(last_state)
        if dirty or last_state ~= buf_state or last_focused ~= is_focused then
            module.draw_lines(self, is_focused)
        end

        dirty = false
        last_state = buf_state
        last_focused = is_focused
    end

    function self.mark_dirty()
        dirty = true
        self.w_cursor = nil
    end

    function self.mark_cursor(row, col)
        self.w_cursor = WPos(row, col)
    end

    function self.inner_width()
        local left_margin = self.show_line_numbers and LINE_NB_WIDTH or 0
        return self.cols - left_margin
    end

    function self.get_motion_props(count, action)
        local cursor, mtype, inc = ed.get_motion_props(self.buf, self, count,
                action, self.cursor)
        return self.clip_cursor(cursor), mtype, inc
    end

    function self.handle_cursor(count, action, can_scroll)
        self.cursor = self.get_motion_props(count, action)

        if can_scroll then
            -- XXX handle win_start.byte
            if self.cursor.line < self.win_start.line then
                self.win_start.line = self.cursor.line
            -- XXX line/row size. also, # of rows with status line
            elseif self.cursor.line > self.win_start.line + self.rows - 2 then
                self.win_start.line = self.cursor.line - self.rows + 2
            end
            self.clip_view()
        end
        if self.visual_mode then
            self.update_visual(self.visual_mode, self.cursor)
        end
        self.mark_dirty()
    end

    function self.clip_cursor(cursor)
        cursor.line = math.max(0, math.min(cursor.line,
                self.buf.get_line_count() - 1))
        local len = self.buf.get_line_len(cursor.line) - 2
        cursor.byte = math.max(0, math.min(cursor.byte, len))
        return cursor
    end

    function self.clip_view()
        self.win_start.line = math.max(0, math.min(self.win_start.line,
                self.buf.get_line_count() - 2))
    end

    function self.handle_scroll(action, count)
        -- XXX For now, just require that we already know the cursor position
        assert(self.w_cursor)
        local w_cursor, w_row = self.w_cursor, self.w_cursor.row
        count = count or 1
        local scroll = get_scroll_amount(action, self) * count
        local width = self.inner_width()
        -- Scroll down
        local scrolled = 0
        if scroll > 0 then
            local len = self.buf.get_line_len(self.win_start.line) - 1
            local line_count = self.buf.get_line_count() - 1
            while scrolled < scroll and self.win_start.line < line_count do
                if self.win_start.byte + width < len then
                    self.win_start.byte = self.win_start.byte + width
                else
                    self.win_start.line = self.win_start.line + 1
                    self.win_start.byte = 0
                    len = self.buf.get_line_len(self.win_start.line) - 1
                end
                scrolled = scrolled + 1
            end
            w_row = w_row - scrolled
            if w_row < 0 then
                self.handle_cursor(-w_row, ACT.MOTION_ROWS_DOWN, false)
                w_row = 0
            end
        -- Scroll up
        else
            scroll = -scroll
            while scrolled < scroll and (self.win_start.line > 0 or
                    self.win_start.byte > 0) do
                if self.win_start.byte > 0 then
                    self.win_start.byte = self.win_start.byte - width
                else
                    self.win_start.line = self.win_start.line - 1
                    local len = self.buf.get_line_len(self.win_start.line) - 1
                    self.win_start.byte = len - (len % width)
                end
                scrolled = scrolled + 1
            end
            self.win_start.byte = math.max(self.win_start.byte, 0)
            w_row = w_row + scrolled
            local last_row = self.rows - 2
            if w_row > last_row then
                self.handle_cursor(w_row - last_row, ACT.MOTION_ROWS_UP, false)
                w_row = last_row
            end
        end
        -- Kinda hacky: manually set the cursor to where we think it is after
        -- marking the window as dirty (which clears w_cursor)
        self.mark_dirty()
        self.w_cursor = w_cursor
        self.w_cursor.row = w_row
    end

    -- Visual mode

    function self.start_visual(mode, cursor)
        self.visual_start = cursor.copy()
        self.update_visual(mode, cursor)
    end
    function self.update_visual(mode, cursor)
        self.visual_mode = mode
        self.visual_end = cursor.copy()
        self.mark_dirty()
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
        if self.visual_mode == MODE.VISUAL_LINE then
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
                win.render(win_idx == cur_win)
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
            if win_idx == cur_win and win.w_cursor then
                nc.wmove(nc_win, win.w_cursor.row, win.w_cursor.col)
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
    local ts_ctx = ts.TSContext()

    -- Read input files and parse them
    local buffers = {}
    for _, path in ipairs(paths) do
        local buf = zmt.open_buffer(path)
        ts_ctx.parse_buf(buf)
        buffers[#buffers + 1] = buf

        if debug then
            local td_buf = zmt.TreeDebugBuffer(buf)
            td_buf.query = ts.TS_NULL_QUERY
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

    local state = ed.EdState(windows, ts_ctx)

    local input_handler = input.InputHandler()
    -- Input buffer
    local buf_size = 64
    local in_buf = ffi.new('uint8_t[?]', buf_size)
    local n_read, n_used = 0, 0
    -- XXX for some reason just #include-ing unistd.h makes this function not found...
    ffi.cdef('ssize_t read(int, void *, size_t);')

    while true do
        -- Only draw the screen if we've processed the whole input buffer
        if n_used >= n_read then
            ui.draw(state.cur_win, state.mode)
        end

        -- Handle input
        local action, count, input, data
        input_handler.reset(state.mode)
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

        -- If we got a mouse event, interpret the window target
        if data and (action == ACT.MOUSE_DOWN or
                ed.action_is_scroll(action)) then
            local idx, target = ui.find_window_target(unpack(data))
            data = {idx=idx, target=target}
        end

        if state.handle_action(action, count, data) then
            break
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
