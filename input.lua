require('stdlib')
local ed = require('ed')
local ACT, MODE = ed.ACT, ed.MODE

--------------------------------------------------------------------------------
-- Input handling --------------------------------------------------------------
--------------------------------------------------------------------------------

-- Input helper functions

local function handle_mouse_input(buf)
    -- XXX hardcoded values
    local code = buf[4] - 0x20
    local col = buf[5] - 0x21
    local row = buf[6] - 0x21
    if code >= 0 and code <= 2 then
        return ACT.MOUSE_DOWN, {row, col}
    elseif code == 64 then
        return ACT.SCROLL_UP, {row, col}
    elseif code == 65 then
        return ACT.SCROLL_DOWN, {row, col}
    end
end

local function handle_insert_char(buf)
    assert(#buf == 1)
    -- Ignore all non-ASCII and control characters except \n
    if buf[1] >= 32 and buf[1] < 127 then
        return ACT.INSERT_CHAR, buf[1]
    elseif buf[1] == 13 then
        return ACT.INSERT_CHAR, 10
    end
end

local function CTRL(x) return string.char(bit.band(string.byte(x), 0x1f)) end
local ESC = CTRL('[')

local function byte_in_range(c, a, b)
    return (c >= string.byte(a) and c <= string.byte(b))
end

-- Input sequences, per mode

local MOTION_TABLE = {
    ['h']               = ACT.MOTION_LEFT,
    ['j']               = ACT.MOTION_DOWN,
    ['k']               = ACT.MOTION_UP,
    ['l']               = ACT.MOTION_RIGHT,
    ['gj']              = ACT.MOTION_ROWS_DOWN,
    ['gk']              = ACT.MOTION_ROWS_UP,
    ['0']               = ACT.MOTION_HOME,
    ['$']               = ACT.MOTION_END,
    ['gg']              = ACT.MOTION_FIRST,
    ['G']               = ACT.MOTION_LAST,
}

local OPERATOR_TABLE = {
    ['c']               = ACT.OP_CHANGE,
    ['d']               = ACT.OP_DELETE,
}

local INPUT_TABLES = {
    [MODE.NORMAL] = {
        ['QQ']              = ACT.QUIT,

        ['i']               = ACT.ENTER_INSERT,
        ['v']               = ACT.ENTER_VISUAL_CHAR,
        ['V']               = ACT.ENTER_VISUAL_LINE,

        [CTRL('E')]         = ACT.SCROLL_DOWN,
        [CTRL('Y')]         = ACT.SCROLL_UP,
        [CTRL('D')]         = ACT.SCROLL_HALFPAGE_DOWN,
        [CTRL('U')]         = ACT.SCROLL_HALFPAGE_UP,
        [ESC..'[M...']      = handle_mouse_input,
        [CTRL('N')]         = ACT.WINDOW_NEXT,
        [CTRL('P')]         = ACT.WINDOW_PREV,
    },
    [MODE.INSERT] = {
        [ESC]               = ACT.EXIT_INSERT,
        ['.']               = handle_insert_char,
    },
    [MODE.VISUAL_CHAR] = {
        [ESC]               = ACT.EXIT_VISUAL,
        ['v']               = ACT.EXIT_VISUAL,
        ['V']               = ACT.ENTER_VISUAL_LINE,
    },
    [MODE.VISUAL_LINE] = {
        [ESC]               = ACT.EXIT_VISUAL,
        ['v']               = ACT.ENTER_VISUAL_CHAR,
        ['V']               = ACT.EXIT_VISUAL,
    },
    [MODE.OPERATOR] = {
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
for _, mode in ipairs{MODE.NORMAL, MODE.VISUAL_CHAR, MODE.VISUAL_LINE,
        MODE.OPERATOR} do
    INPUT_TREES[mode] = merge(INPUT_TREES[mode], op_tree)
    INPUT_TREES[mode] = merge(INPUT_TREES[mode], motion_tree)
end

-- Read input until we parse a command
local function InputHandler()
    local self = {}
    local matches = {}
    local enable_count, done_count, count = nil, nil, nil

    function self.reset(mode)
        input_tree = INPUT_TREES[mode]
        -- Store all current input sequence matches
        matches = {}
        -- HACK
        enable_count = (mode == MODE.NORMAL or mode == MODE.OPERATOR or
                ed.mode_is_visual(mode))
        done_count = false
        count = nil
    end

    function self.matches() return matches end

    function self.feed(c)
        local input, action, data = nil, nil, nil

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
                if type(lookup) == 'table' and lookup.enum ~= ACT then
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
        if action == nil then
            local lookup = input_tree[c] or input_tree[0]
            if type(lookup) == 'table' and lookup.enum ~= ACT then
                -- New match
                matches[#matches + 1] = {{c}, lookup}
            elseif lookup ~= nil and #matches == 0 then
                -- Immediate match
                input, action = {c}, lookup
            end
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

return {
    InputHandler = InputHandler,
    CTRL = CTRL,
    ESC = ESC,
}
