#!/usr/bin/env luajit-2.1.0-beta3
require('stdlib')
local zmt = require('zmt')

-- Helpers
local function piece_list(tree)
    local l = {}
    for iter, node, piece in zmt.iter_nodes(tree, 0, 0) do
        l[#l + 1] = piece
    end
    return l
end

local function line_list(tree)
    local l = {''}
    for _, _, is_end, piece in zmt.iter_lines(tree, 0, 1e100) do
        l[#l] = l[#l] .. piece
        if is_end then
            l[#l + 1] = ''
        end
    end
    return l
end

-- Check a bunch of attributes of a meta tree all at once. We compare the
-- expected pieces/lines with the actual values too.
local function check_tree(tree, exp_pieces, exp_lines)
    -- Verify metadata is correct
    zmt.zmt.verify_node(tree.root)

    local pieces = piece_list(tree)
    local lines = line_list(tree)

    local tree_size = zmt.zmt.get_tree_total_size(tree)

    -- Verify we got the expected pieces/lines
    assert_eq(pieces, exp_pieces)
    assert_eq(lines, exp_lines)

    -- Verify we get the right number of bytes
    local raw_bytes = table.concat(pieces)
    assert_eq(tree_size.byte, #raw_bytes)

    local raw_bytes_nonl = table.concat(lines)
    assert_eq(tree_size.byte - tree_size.line, #raw_bytes_nonl)
    -- tree_size.line counts newlines, but lines is newline-separated strings,
    -- thus contains 1 more entry
    assert_eq(tree_size.line + 1, #lines)
end

local function tree_insert_bytes(tree, offset, data)
    return zmt.zmt.insert_bytes_at_offset(tree, offset, data, #data)
end

-- Tests
local TESTS = {
    test_stdlib = function ()
        assert_eq(0, 0)
        assert_neq(0, 1)
        assert_neq(0, '0')
        assert_neq(0, {})
        assert_eq({}, {})
        assert_neq({}, {1})
        assert_eq({1}, {1})
        assert_eq({{1}, {1}}, {{1}, {1}})
        assert_neq({{1}, {1}}, {{1}, {2}})
        assert_eq({{1}, {1, 2}}, {{1}, {1, 2}})
        assert_neq({{1}, {1, 2}}, {{1}, {1, 2, 3}})
    end,

    test_tree = function ()
        local tree = zmt.zmt.create_tree(true)
        check_tree(tree, {''}, {''})

        -- XXX leaves a node with no bytes
        tree = tree_insert_bytes(tree, 0, 'a\nb')
        check_tree(tree, {'a\nb', ''}, {'a', 'b'})

        tree = tree_insert_bytes(tree, 1, 'a')
        check_tree(tree, {'a', 'a', '\nb', ''}, {'aa', 'b'})
    end,
}

for name, fn in pairs(TESTS) do
    io.stdout:write(right_pad('Running ' .. name .. '...', 50))
    local ok, res = xpcall(fn, debug.traceback)
    if ok then
        print('[\027[32mPASS\027[0m]')
    else
        print('[\027[31mFAIL\027[0m]')
        print(res)
    end
end
