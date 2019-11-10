--------------------------------------------------------------------------------
-- Editor definitions and operations -------------------------------------------
--------------------------------------------------------------------------------

require('stdlib')
local zmt = require('zmt')
local lib = zmt.lib

-- Modes
local MODE = enum('MODE', 'NORMAL INSERT VISUAL_CHAR VISUAL_LINE OPERATOR')

local MODE_STR = {
    [MODE.NORMAL] = '',
    [MODE.INSERT] = '-- INSERT --',
    [MODE.VISUAL_CHAR] = '-- VISUAL --',
    [MODE.VISUAL_LINE] = '-- VISUAL LINE --',
    [MODE.OPERATOR] = '-- OPERATOR --',
}

function mode_is_visual(mode)
    return (mode == MODE.VISUAL_CHAR or mode == MODE.VISUAL_LINE)
end

-- Action enum
local ACT = enum('ACT', [[
    -- Generic/mode switching commands
    ENTER_INSERT EXIT_INSERT ENTER_VISUAL_CHAR ENTER_VISUAL_LINE
    EXIT_VISUAL QUIT
    -- Operators
    OPERATOR:{ OP_CHANGE OP_DELETE }
    -- Motions
    MOTION:{ MOTION_UP MOTION_DOWN MOTION_LEFT MOTION_RIGHT MOTION_HOME
        MOTION_END MOTION_FIRST MOTION_LAST MOTION_NL }
    -- Insert mode
    INSERT_CHAR
    -- Scrolling
    SCROLL:{ SCROLL_UP SCROLL_DOWN SCROLL_HALFPAGE_UP SCROLL_HALFPAGE_DOWN }
    -- Mouse events
    MOUSE_DOWN
    -- Window movement
    WINDOW_SWITCH:{ WINDOW_NEXT WINDOW_PREV }
]])

local function action_is_operator(action)
    return action.group == 'OPERATOR'
end
local function action_is_scroll(action)
    return action.group == 'SCROLL'
end
local function action_is_motion(action)
    return action.group == 'MOTION'
end
local function action_is_window_switch(action)
    return action.group == 'WINDOW_SWITCH'
end

-- Motion properties
local MP = enum('MP', 'LINEWISE CHARWISE INC EXC')

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

function get_motion_props(buf, raw_count, action, start)
    local count = raw_count or 1
    if action == ACT.MOTION_UP then
        return start.delta(-count, 0), MP.LINEWISE, MP.INC
    elseif action == ACT.MOTION_DOWN then
        return start.delta(count, 0), MP.LINEWISE, MP.INC
    elseif action == ACT.MOTION_LEFT then
        return start.delta(0, -count), MP.CHARWISE, MP.EXC
    elseif action == ACT.MOTION_RIGHT then
        return start.delta(0, count), MP.CHARWISE, MP.EXC
    elseif action == ACT.MOTION_HOME then
        return Pos(start.line, 0), MP.CHARWISE, MP.EXC
    elseif action == ACT.MOTION_END then
        local len = buf.get_line_len(start.line) - 1
        return Pos(start.line, len), MP.CHARWISE, MP.INC
    elseif action == ACT.MOTION_FIRST then
        return Pos(raw_count and (raw_count - 1) or 0, 0), MP.LINEWISE, MP.INC
    elseif action == ACT.MOTION_LAST then
        local last = buf.get_line_count() - 1
        return Pos(raw_count and (raw_count - 1) or last, 0), MP.LINEWISE, MP.INC
    elseif action == ACT.MOTION_NL then
        return Pos(start.line + 1, 0), MP.LINEWISE, MP.INC
    end
end

function EdState(windows, ts_ctx)
    local self = {}
    self.mode = MODE.NORMAL
    self.cur_win = 1
    local window = windows[self.cur_win]

    -- Action and count for operator pending mode
    local op_action, op_count = nil, nil

    -- Handle an operator on a given range
    local function operate(window, start, stop, action, mtype, inc)
        if start > stop then
            start, stop = stop, start
        end

        -- Handle linewise/charwise and inclusive/exclusive
        if mtype == MP.LINEWISE then
            -- XXX ignore inclusive/exclusive
            start.byte = 0
            -- Sorta hacky: linewise change operator expects to get a new
            -- line when starting the insert, so cut off before the last byte
            if action == ACT.OP_CHANGE then
                stop.byte = window.buf.get_line_len(stop.line) - 1
            else
                stop.line = stop.line + 1
                stop.byte = 0
            end
        elseif inc == MP.INC then
            stop.byte = stop.byte + 1
        end

        local start_off = lib.get_abs_byte_offset(window.buf.tree,
                start.line, start.byte)
        local stop_off = lib.get_abs_byte_offset(window.buf.tree,
                stop.line, stop.byte)

        if start_off < stop_off then
            window.buf.tree = lib.delete_byte_range(window.buf.tree,
                    start_off, stop_off)
            -- HACK: just re-parse the whole buffer, and leak the old ast
            ts_ctx.parse_buf(window.buf)
        end
        window.cursor = start
        if action == ACT.OP_CHANGE then
            window.buf.tree = lib.split_at_offset(window.buf.tree,
                    window.cursor.line, window.cursor.byte)
            return MODE.INSERT
        end
        return MODE.NORMAL
    end

    function self.handle_action(action, count, data)
        -- Clear out any pending operator
        if self.mode ~= MODE.OPERATOR then
            op_action, op_count = nil, nil
        elseif action_is_operator(action) and op_action ~= action then
            op_action, op_count = nil, nil
            self.mode = MODE.NORMAL
        end

        ------------------------------------------------------------------------
        -- Universal commands --------------------------------------------------
        ------------------------------------------------------------------------
        if action == ACT.QUIT then
            return true
        elseif action_is_window_switch(action) then
            local offset = (action == ACT.WINDOW_NEXT and 1 or -1)
            self.cur_win = (self.cur_win + offset - 1) % #windows + 1
            window = windows[self.cur_win]
        elseif action == ACT.MOUSE_DOWN then
            if data.target ~= nil then
                self.cur_win, window = data.idx, data.target
            end
        elseif action_is_scroll(action) then
            if data and data.target then
                data.target.handle_scroll(action, count)
            else
                window.handle_scroll(action, count)
            end

        elseif action_is_motion(action) then
            if op_action then
                -- Multiply counts unless both are nil, then pass nil
                local count = op_count and (op_count * (count or 1)) or count
                local stop, mtype, inc = window.get_motion_props(count, action)
                self.mode = operate(window, window.cursor, stop,
                        op_action, mtype, inc)
            else
                window.handle_cursor(count, action)
                if mode_is_visual(self.mode) then
                    window.update_visual(self.mode, window.cursor)
                end
            end

        ------------------------------------------------------------------------
        -- Normal mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif self.mode == MODE.NORMAL and action_is_operator(action) then
            op_action, op_count = action, count
            self.mode = MODE.OPERATOR

        ------------------------------------------------------------------------
        -- Operator mode -------------------------------------------------------
        ------------------------------------------------------------------------

        -- Handle double operators: cc, dd, etc.
        elseif self.mode == MODE.OPERATOR and action_is_operator(action) then
            assert(action == op_action)
            -- Multiply counts unless both are nil, then pass 1 (unlike default
            -- counts for motions, which pass nil)
            local count = op_count and (op_count * (count or 1)) or count or 1
            -- Double operator with count works like [count - 1]j
            local stop, mtype, inc = window.get_motion_props(
                    count - 1, ACT.MOTION_DOWN)
            self.mode = operate(window, window.cursor, stop,
                    op_action, mtype, inc)

        ------------------------------------------------------------------------
        -- Insert Mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif action == ACT.ENTER_INSERT then
            self.mode = MODE.INSERT
            -- Split the tree at the cursor offset
            window.buf.tree = lib.split_at_offset(window.buf.tree,
                    window.cursor.line, window.cursor.byte)
        elseif action == ACT.EXIT_INSERT then
            self.mode = MODE.NORMAL
        elseif action == ACT.INSERT_CHAR then
            assert(self.mode == MODE.INSERT)

            local char = string.char(data)

            window.buf.tree = lib.append_bytes_to_filler(window.buf.tree,
                    char, #char)

            lib.verify_node(window.buf.tree.root)
            -- HACK: just re-parse the whole buffer, and leak the old ast
            ts_ctx.parse_buf(window.buf)
            -- XXX dumb?
            window.handle_cursor(1, data == 10 and ACT.MOTION_NL or
                ACT.MOTION_RIGHT)

        ------------------------------------------------------------------------
        -- Visual Mode ---------------------------------------------------------
        ------------------------------------------------------------------------
        elseif action == ACT.ENTER_VISUAL_CHAR or
                action == ACT.ENTER_VISUAL_LINE then
            local next_mode = ((action == ACT.ENTER_VISUAL_CHAR) and
                    MODE.VISUAL_CHAR or MODE.VISUAL_LINE)
            if mode_is_visual(self.mode) then
                window.update_visual(next_mode, window.cursor)
            else
                window.start_visual(next_mode, window.cursor)
            end
            self.mode = next_mode
        elseif action == ACT.EXIT_VISUAL then
            self.mode = MODE.NORMAL
            window.end_visual()
        elseif mode_is_visual(self.mode) and
                action_is_operator(action) then
            mtype = (self.mode == MODE.VISUAL_CHAR) and MP.CHARWISE or
                    MP.LINEWISE
            local start, stop = window.get_visual_range()
            self.mode = operate(window, start, stop, action, mtype, MP.EXC)
            window.end_visual()
        else
            --error('unknown action', action)
        end

        return false
    end

    return self
end

return {
    ACT = ACT,
    action_is_scroll = action_is_scroll,
    MODE = MODE,
    MODE_STR = MODE_STR,
    mode_is_visual = mode_is_visual,
    Pos = Pos,
    get_motion_props = get_motion_props,
    EdState = EdState,
}