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
        assert_eq(piece_list(tree), {""})
        assert_eq(line_list(tree), {""})
    end,
}

for name, fn in pairs(TESTS) do
    io.stdout:write(right_pad(name .. '...', 50))
    local ok, res = xpcall(fn, debug.traceback)
    if ok then
        print('[\027[32mPASS\027[0m]')
    else
        print('[\027[31mFAIL\027[0m]')
        print(res)
    end
end
