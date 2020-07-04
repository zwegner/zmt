--------------------------------------------------------------------------------
-- Main meta-tree interface ----------------------------------------------------
--------------------------------------------------------------------------------

require('stdlib')
local bit = require('bit')
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
local lib = ffi.load(so_path)
local defs = io.open(h_path)
ffi.cdef(defs:read('*all'))
defs:close()

-- Make C library accessible elsewhere
module.lib = lib

function module.iter_start(tree, line_offset, byte_offset)
    byte_offset = byte_offset or 0
    line_offset = line_offset or 0
    local iter = ffi.new('meta_iter_t[1]')
    local node = lib.iter_start_at(iter, tree, line_offset, byte_offset)
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
            node = lib.iter_next(iter)
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
                local part = piece:sub(1, idx)
                piece = piece:sub(idx + 1)
                if #part > 0 then
                    coroutine.yield(line, offset, true, part)
                end
                offset = offset + idx
                line = line + 1
            end

            -- Yield the last part of this piece
            if #piece > 0 then
                coroutine.yield(line, offset, false, piece)
            end
        end
    end)
end

local function ptr_string(cdata)
    return ffi.string(lib.ptr_string(cdata))
end

local function fmt_node(node)
    if node == nil then
        return '[null]'
    end

    local prefix = ('%s(%s) b=%s n=%s'):format(
            ptr_string(node), node.flags, node.byte_count, node.nl_count)

    if bit.band(node.flags, lib.NODE_LEAF + lib.NODE_FILLER) ~= 0 then
        if bit.band(node.flags, lib.NODE_FILLER) ~= 0 then
            prefix = prefix .. ' FILLER'
        end
        local l = node.leaf
        local data = ffi.string(l.chunk_data + l.start, l['end'] - l.start)
        return fmt(prefix, str(data))
    elseif bit.band(node.flags, lib.NODE_HOLE) ~= 0 then
        return fmt(prefix, 'HOLE')
    end
    return prefix
end

function module.print_node(node)
    log(fmt_node(node))
end

-- Recursive coroutine that formats tree printing
local function co_fmt_tree(tree, node, depth)
    coroutine.yield(('  '):rep(depth) .. fmt_node(node))
    if node ~= nil and bit.band(node.flags, lib.NODE_INNER) ~= 0 then
        for i = 0, lib.MAX_CHILDREN-1 do
            if node.inner.children[i] ~= nil then
                co_fmt_tree(tree, node.inner.children[i], depth+1)
            end
        end
    end
end

local function iter_fmt_tree(tree)
    return coroutine.wrap(function ()
        co_fmt_tree(tree, tree.root, 0)
    end)
end

function module.print_tree(tree)
    log('\nTREE')
    for line in iter_fmt_tree(tree) do
        log(line)
    end
end

local ITER_STATE = c_enum_table(lib, [[ITER_START ITER_CHILDREN ITER_JUMP
        ITER_HOLES]])

local function fmt_offset(offset)
    return ('{l=%s b=%s}'):format(tonumber(offset.line), tonumber(offset.byte))
end

function module.print_iter(iter)
    log('\nITER')
    log('  start: ' .. fmt_offset(iter.start_offset))
    log('    end: ' .. fmt_offset(iter.end_offset))
    log(' desire: ' .. fmt_offset(iter.desired_offset))
    log('slice l: ' .. fmt_node(iter.slice_node_left[0]))
    log('slice r: ' .. fmt_node(iter.slice_node_right[0]))
    for depth = 0, iter.depth-1 do
        local frame = iter.frame[depth]
        logf('%2d: %13s %s %s\n', depth, ITER_STATE[tonumber(frame.state)],
                frame.idx, fmt_node(frame.node))
        if frame.node ~= nil and
                bit.band(frame.node.flags, lib.NODE_INNER) ~= 0 then
            for i = 0, lib.MAX_CHILDREN-1 do
                local child = frame.node.inner.children[i]
                logf('    %s\n', child ~= nil and fmt_node(child) or nil)
            end
        end
    end
end

--------------------------------------------------------------------------------
-- Buffers ---------------------------------------------------------------------
--------------------------------------------------------------------------------

function module.Buffer(path, tree)
    local self = {}
    self.path = path
    self.tree = tree

    -- no-op
    function self.refresh()
        return self.tree
    end

    function self.get_line_count()
        return tonumber(lib.get_tree_total_size(self.tree).line) + 1
    end

    function self.get_line_len(line)
        -- XXX multibyte
        return tonumber(lib.get_tree_line_length(self.tree, line))
    end

    function self.iter_lines_from(start_line, start_byte)
        return module.iter_lines(self.tree, start_line, start_byte)
    end

    return self
end

function module.open_buffer(path)
    local chunk = lib.map_file(path)
    local tree = lib.dumb_read_data(chunk, 0)
    return module.Buffer(path, tree)
end

function module.TreeDebugBuffer(buf)
    local self = {}
    self.path = '[tree-debug]'
    local lines = {}

    function self.refresh(tree)
        if tree == buf.tree then return tree end
        lines = {}
        for line in iter_fmt_tree(buf.tree) do
            lines[#lines+1] = line
        end
        return buf.tree
    end

    function self.iter_lines_from(start_line, start_byte)
        return coroutine.wrap(function ()
            for line_nb, line in ipairs(lines) do
                line_nb = line_nb - 1
                if start_byte and line_nb == start_line then
                    line = line:sub(start_byte + 1)
                end
                if line_nb >= start_line then
                    coroutine.yield(line_nb, 0, true, line)
                end
            end
        end)
    end

    function self.get_line_count()
        return #lines
    end
    function self.get_line_len(line)
        return #lines[line + 1] + 1
    end
    return self
end

return module
