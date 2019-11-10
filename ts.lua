--------------------------------------------------------------------------------
-- Tree-sitter integration -----------------------------------------------------
--------------------------------------------------------------------------------

require('stdlib')
local ffi = require('ffi')
local zmt = require('zmt')

-- Alias of the main C library for tree-sitter
local ts = zmt.lib

local module = {}

local TS_LANGS = {
    ['c'] = {zmt.lib.parse_c_tree, ts.tree_sitter_c, 'ts-query/c.txt'},
    ['h'] = {zmt.lib.parse_c_tree, ts.tree_sitter_c, 'ts-query/c.txt'},
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

-- TS_NULL_QUERY is a singleton that conforms to the TSQuery interface but
-- doesn't return any highlight events. Used for unknown syntax, etc.
local TS_NULL_QUERY = {}
function TS_NULL_QUERY.reset(offset) end
function TS_NULL_QUERY.next_capture() end
function TS_NULL_QUERY.current_capture() end

-- TSContext holds all global tree-sitter context
local function TSContext()
    local self = {}
    local langs = {}

    -- Parse a query file for a given language with tree-sitter
    local function get_lang(ftype)
        -- Fill in cached entry if it's not there
        if not langs[ftype] then
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
            langs[ftype] = {zmt_parse, query}
        end
        return unpack(langs[ftype])
    end

    function self.parse_buf(buf)
        -- XXX get actual extension/filetype
        local ext = buf.path:sub(-1, -1)
        local parse, query = get_lang(ext)

        if not parse then
            buf.query = TS_NULL_QUERY
        else
            -- XXX sticking stuff into buffer object--maybe not the best
            -- abstraction...?
            buf.ast = parse(buf.tree)
            buf.query = TSQuery(query, buf)
        end
    end

    return self
end

-- Same but for TSContext
local TS_NULL_CONTEXT = {}
function TS_NULL_CONTEXT.parse_buf(buf) buf.query = TS_NULL_QUERY end

return {
    TSContext = TSContext,
    TS_NULL_QUERY = TS_NULL_QUERY,
    TS_NULL_CONTEXT = TS_NULL_CONTEXT,
}
