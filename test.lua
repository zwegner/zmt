#!/usr/bin/env luajit
require('stdlib')
local zmt = require('zmt')
local ed = require('ed')
local ui = require('ui')
local input = require('input')
local CTRL, ESC = input.CTRL, input.ESC
local ts = require('ts')

local VERBOSE = false

-- Helpers

local TEST_NAME = ''
local function test_name(name)
    TEST_NAME = name
    if verbose then
        print(('  inside test [%s]'):format(TEST_NAME))
    end
end

local function piece_list_from_iter(iter)
    local l = {}
    for iter, node, piece in iter do
        l[#l + 1] = piece
    end
    return l
end

local function piece_list(tree)
    return piece_list_from_iter(zmt.iter_nodes(tree, 0, 0))
end

local function line_list(tree)
    local l = {''}
    for _, _, is_end, piece in zmt.iter_lines(tree, 0, 0) do
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
    zmt.lib.verify_node(tree.root)

    local pieces = piece_list(tree)
    local lines = line_list(tree)

    local tree_size = zmt.lib.get_tree_total_size(tree)

    -- Verify we got the expected pieces/lines
    assert_eq(pieces, exp_pieces, 'pieces match')
    assert_eq(lines, exp_lines, 'lines match')

    -- Verify we get the right number of bytes
    local raw_bytes = table.concat(pieces)
    assert_eq(tree_size.byte, #raw_bytes, 'byte size matches')

    local raw_bytes_nonl = table.concat(lines)
    assert_eq(tree_size.byte, #raw_bytes_nonl, 'line byte size matches')
    -- tree_size.line counts newlines, but lines is newline-separated strings,
    -- thus contains 1 more entry
    assert_eq(tree_size.line + 1, #lines, 'line count matches')

    -- Make sure a crazy offset works
    for i, byte, line in iter_unpack{{1e10, 0}, {0, 1e10}} do
        local iter, node = zmt.iter_start(tree, line, byte, true)
        assert_eq(node, nil, 'big jump finishes the iterator')
        assert_eq(iter[0].start_offset.byte, tree_size.byte,
                'big jump gets the right offset')
    end

    -- Verify that all the line lengths are correct
    for i, line in ipairs(lines) do
        assert_eq(zmt.lib.get_tree_line_length(tree, i - 1), #line,
            ('length of line %s matches'):format(i))
    end
end

local function tree_insert_bytes(tree, offset, data)
    return zmt.lib.insert_bytes_at_offset(tree, 0, offset, data, #data)
end

local function create_fake_buffer(name, data, piece_size)
    if type(data) == 'table' then
        line = ''
        for _, length in ipairs(data) do
            for i = 0, length-1 do
                line = line .. (i % 10)
            end
            line = line .. '\n'
        end
        data = line
    end
    local chunk = zmt.lib.write_new_chunk(data, #data)
    local tree = zmt.lib.dumb_read_data(chunk, piece_size)
    local buf = zmt.Buffer(name, tree)
    buf.query = ts.TS_NULL_QUERY
    return buf
end

-- A wrapper around a few UI things
local function DumbUI(windows)
    local self = {}
    local input_handler = input.InputHandler()
    local state = ed.EdState(windows, ts.TS_NULL_CONTEXT)
    function self.feed(input)
        input_handler.reset(state.mode)
        for _, char in iter_bytes(input) do
            local action, count, _, data = input_handler.feed(char)
            if action ~= nil then
                state.handle_action(action, count, data)
                input_handler.reset(state.mode)
            end
        end
    end
    function self.refresh()
        for _, win in ipairs(windows) do
            win.render(true)
        end
    end
    return self
end

local function check_grid(name, win, expected_grid, check_attrs)
    test_name(name)
    win.render(true)
    act_grid = ui.get_grid_lines(win, check_attrs)

    -- Parse the expected grid
    local exp_grid = split(expected_grid, '\n')
    exp_grid = amap(exp_grid, function (row)
        return row:match('^ +|(.*)|$')
    end)

    local function print_grids()
        local exp_width = math.max(unpack(amap(exp_grid,
                function(row) return #row end))) + 3
        local act_width = math.max(unpack(amap(act_grid,
                function(row) return #row end))) + 3
        -- Work around for lua formatting not supporting %*s
        local fmt = '    %-'..act_width..'s   %-'..exp_width..'s%s\n'
        logf(fmt, 'Actual', 'Expected', '')
        for i = 1, math.max(#act_grid, #exp_grid) do
            local exp, act = exp_grid[i], act_grid[i]
            logf(fmt, act and '|'..act..'|', exp and '|'..exp..'|',
                    act ~= exp and '   <--' or '')
        end
    end
    assert_eq(act_grid, exp_grid, name, print_grids)
end

-- Tests
local TESTS = {
    {'stdlib', function ()
        test_name('assert')
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

        test_name('strip')
        assert_eq(strip(' \t\na \t\n b \t\n'), 'a \t\n b')

        test_name('split')
        assert_eq(split('a\nb\nc', '\n'), {'a', 'b', 'c'})
        assert_eq(split('\nabc\n', '\n'), {'', 'abc', ''})
        assert_eq(split('\n\n\nabc\n\n\n', '\n+'), {'', 'abc', ''})

        test_name('areverse')
        assert_eq(areverse{}, {})
        assert_eq(areverse{'a'}, {'a'})
        assert_eq(areverse{'a', 'b', 'c'}, {'c', 'b', 'a'})

        test_name('enum')
        local e = enum('E', 'a, b, c\nd,\ne')
        assert_eq(#e, 5)
        assert_eq(str(e.a), 'E.a')
        assert_eq(astr(e), '{E.a, E.b, E.c, E.d, E.e}')
        assert_eq(e.a, e.a, 'values are equal')
        assert_neq(e.a, e.b, 'values are distinct')
        local e2 = enum('E2', 'a')
        assert_neq(e.a, e2.a, 'values are distinct')
        local e_table = {[e.a]=1, [e.b]=2}
        assert_eq(e_table[e.a], 1, 'values are hashable')
        assert_eq(e_table[e.b], 2, 'values are hashable')
    end},

    {'tree 1', function ()
        test_name('basic tree 1')
        local tree = zmt.lib.create_tree(true)
        check_tree(tree, {}, {''})

        -- XXX leaves a node with no bytes
        test_name('basic tree 2')
        tree = tree_insert_bytes(tree, 0, 'a\nb')
        check_tree(tree, {'a\nb'}, {'a\n', 'b'})

        test_name('basic tree 3')
        tree = tree_insert_bytes(tree, 1, 'c')
        check_tree(tree, {'a', 'c', '\nb'}, {'ac\n', 'b'})

        -- Modify in two spots, on either side of the current hole
        test_name('basic tree 4')
        new_tree_1 = tree_insert_bytes(tree, 1, 'd')
        check_tree(new_tree_1, {'a', 'd', 'c', '\nb'}, {'adc\n', 'b'})

        test_name('basic tree 5')
        new_tree_2 = tree_insert_bytes(tree, 2, 'd')
        check_tree(new_tree_2, {'a', 'c', 'd', '\nb'}, {'acd\n', 'b'})

        -- Make sure the old tree is untouched
        test_name('basic tree 6')
        check_tree(tree, {'a', 'c', '\nb'}, {'ac\n', 'b'})

        -- Verify zero-width fillers are patched properly
        for i = 0, 2 do
            test_name('basic tree 7+'..i)
            tree = zmt.lib.split_at_offset(tree, 0, i)
            check_tree(tree, {'a', 'c', '\nb'}, {'ac\n', 'b'})
        end
    end},

    {'tree 2', function ()
        test_name('setup')
        local tree = zmt.lib.create_tree(true)

        local piece = '0123456789\n'
        for i = 1, 10 do
            tree = tree_insert_bytes(tree, 1e10, piece)
        end
        local pieces = rep(piece, 10)
        local lines = concat(pieces, {''})
        check_tree(tree, pieces, lines)

        -- Insert a bunch of times into the middle of lines
        test_name('split lines')
        for i = 1, 10 do
            -- Dumb index math
            local off = 5 + 14 * (i-1)
            local idx = (i-1)*3 + 1
            tree = tree_insert_bytes(tree, off, 'xxx')
            pieces[idx] = '56789\n'
            table.insert(pieces, idx, 'xxx')
            table.insert(pieces, idx, '01234')
            lines[i] = '01234xxx56789\n'
            check_tree(tree, pieces, lines)
        end
    end},

    {'tree 3--iteration', function ()
        test_name('setup')
        -- Create a tree with 4x 4-byte nodes
        local lines = {'0123456\n', '89abcdef'}
        local data = table.concat(lines)
        local chunk = zmt.lib.write_new_chunk(data, #data)
        local tree = zmt.lib.dumb_read_data(chunk, 4)
        check_tree(tree, {'0123', '456\n', '89ab', 'cdef'}, lines)

        -- Split the tree in all possible spots
        for split_offset = 0, 16 do
            local subtest = ('split at %d'):format(split_offset)
            test_name(subtest)
            local tree = tree
            tree = zmt.lib.split_at_offset(tree, 0, split_offset)
            for byte = 0, 16 do
                test_name(('%s: start at %s'):format(subtest, byte))
                local pieces = {}
                for _, forwards in ipairs{false, true} do
                    local iter, node = zmt.iter_start(tree, 0, byte, forwards)
                    local i = zmt.iter_from(iter, node, forwards)
                    local p = piece_list_from_iter(i)
                    if forwards then
                        pieces = concat(pieces, p)
                    else
                        pieces = concat(pieces, areverse(p))
                    end
                end
                assert_eq(table.concat(pieces), data, 'pieces match up')
            end
        end
    end},

    {'tree 4--deletion', function ()
        test_name('setup')
        -- Create a tree with 4x 4-byte nodes
        local lines = {'0123456\n', '89abcdef'}
        local data = table.concat(lines)
        local chunk = zmt.lib.write_new_chunk(data, #data)
        local tree = zmt.lib.dumb_read_data(chunk, 4)
        check_tree(tree, {'0123', '456\n', '89ab', 'cdef'}, lines)

        for _, split_offset in ipairs{-1, 0, 2, 4, 6, 8, 9} do
            local subtest = ('split at %d'):format(split_offset)
            test_name(subtest)
            -- Split the tree at the given offset
            local tree = tree
            if split_offset >= 0 then
                tree = zmt.lib.split_at_offset(tree, 0, split_offset)
            end

            -- Loop through multiple permutations of starting/stopping on a node
            -- boundary or within a node
            for start = 4, 5 do
                for stop = 7, 9 do
                    test_name(('%s: delete %s-%s'):format(subtest, start, stop))

                    -- Create the expected piece state by chopping the full data
                    -- string up
                    local pieces, lines = {}, {}
                    local p_i, l_i = 1, 1
                    local i = 1
                    for _, c in iter_bytes(data) do
                        c = string.char(c)
                        -- Append to the current piece if this char isn't deleted
                        if i <= start or i > stop then
                            pieces[p_i] = (pieces[p_i] or '') .. c
                            lines[l_i] = (lines[l_i] or '') .. c
                            if c == '\n' then
                                l_i = l_i + 1
                            end
                        end
                        -- Move to the next piece if this is a boundary
                        if (i == start or i == stop or i % 4 == 0 or
                            i == split_offset) and pieces[p_i] then
                            p_i = p_i + 1
                        end
                        i = i + 1
                    end

                    -- Delete the range and check the output
                    local td = zmt.lib.delete_byte_range(tree, start, stop)
                    check_tree(td, pieces, lines)
                end
            end
        end
    end},

    {'line wrapping', function ()
        local buf = create_fake_buffer('[test]', rep(30, 5), 4)
        local win = ui.Window(buf, 10, 20)

        check_grid('lines wrap properly 1', win, [[
            |   1 ^012345678901234|
            |     567890123456789|
            |   2 012345678901234|
            |     567890123456789|
            |   3 012345678901234|
            |     567890123456789|
            |   4 012345678901234|
            |     567890123456789|
            |   5 012345678901234|
            |[test]              |
        ]])

        win.rows, win.cols = 8, 12
        win.mark_dirty()

        check_grid('lines wrap properly 2', win, [[
            |   1 ^0123456|
            |     7890123|
            |     4567890|
            |     1234567|
            |     89     |
            |   2 0123456|
            |     7890123|
            |[test]      |
        ]])
    end},

    {'visual mode', function ()
        local buf = create_fake_buffer('[test]', rep(10, 3), 8)
        local win = ui.Window(buf, 4, 20)
        win.show_line_numbers = false
        local dumb_ui = DumbUI({win})

        dumb_ui.feed('v2j')
        check_grid('visual mode works', win, [[
            |{visual:0123456789}          |
            |{visual:0123456789}          |
            |{visual:^0}123456789          |
            |{status:[test]              }|
        ]], true)

        dumb_ui.feed(ESC..'kd$kv2j')
        check_grid('visual mode works on empty lines', win, [[
            |{visual:0123456789}          |
            |{visual: }                   |
            |{visual:^0}123456789          |
            |{status:[test]              }|
        ]], true)

        dumb_ui.feed(ESC..'ggv')
        -- XXX refresh so we get the right cursor position
        dumb_ui.refresh()
        dumb_ui.feed('2'..CTRL('e'))
        check_grid('visual mode is updated from scrolling', win, [[
            |{visual:^0}123456789          |
            |                    |
            |                    |
            |{status:[test]              }|
        ]], true)
        dumb_ui.feed('2'..CTRL('y'))
        check_grid('visual mode is updated from scrolling 2', win, [[
            |{visual:0123456789}          |
            |{visual: }                   |
            |{visual:^0}123456789          |
            |{status:[test]              }|
        ]], true)

        -- HACKish: create another buffer with longer lines
        buf = create_fake_buffer('[test]', rep(30, 3), 8)
        win.buf = buf
        dumb_ui.feed(ESC..'ggv')
        -- XXX refresh so we get the right cursor position
        dumb_ui.refresh()
        dumb_ui.feed('3'..CTRL('e'))
        check_grid('visual mode works with start byte > 0', win, [[
            |{visual:^0}123456789          |
            |01234567890123456789|
            |0123456789          |
            |{status:[test]              }|
        ]], true)

        dumb_ui.feed('3'..CTRL('y'))
        check_grid('visual mode works with start byte > 0', win, [[
            |{visual:01234567890123456789|
            |0123456789}          |
            |{visual:^0}1234567890123456789|
            |{status:[test]              }|
        ]], true)
    end},

    {'row-wise cursor and scrolling', function ()
        local buf = create_fake_buffer('[test]', {20, 30, 0, 10, 15, 0, 0, 3}, 8)
        local win = ui.Window(buf, 8, 20)
        local dumb_ui = DumbUI({win})

        dumb_ui.feed('gj')
        check_grid('cursor movement works', win, [[
            |   1 012345678901234|
            |     ^56789          |
            |   2 012345678901234|
            |     567890123456789|
            |   3                |
            |   4 0123456789     |
            |   5 012345678901234|
            |[test]              |
        ]])

        dumb_ui.feed('3gj')
        check_grid('cursor movement works', win, [[
            |   1 012345678901234|
            |     56789          |
            |   2 012345678901234|
            |     567890123456789|
            |   3 ^               |
            |   4 0123456789     |
            |   5 012345678901234|
            |[test]              |
        ]])

        dumb_ui.feed('3'..CTRL('E'))
        check_grid('scrolling works row-wise', win, [[
            |---2-567890123456789|
            |   3 ^               |
            |   4 0123456789     |
            |   5 012345678901234|
            |   6                |
            |   7                |
            |   8 012            |
            |[test]              |
        ]])

        -- XXX Go to the first line and scroll up since we aren't handling
        -- scrolling due to cursor movement properly yet
        dumb_ui.feed(CTRL('y')..'gg')
        -- XXX refresh so we get the right cursor position
        dumb_ui.refresh()
        dumb_ui.feed('3'..CTRL('E'))
        check_grid('scrolling moves cursor', win, [[
            |---2-^567890123456789|
            |   3                |
            |   4 0123456789     |
            |   5 012345678901234|
            |   6                |
            |   7                |
            |   8 012            |
            |[test]              |
        ]])
    end},
}

if arg[1] == '-v' then
    verbose = true
    table.remove(arg, 1)
end

local test_filter = #arg > 0 and arg[1]

for i, name, fn in iter_unpack(TESTS) do
    if not test_filter or name:find(test_filter) then
        logf('%s', right_pad('Running test ' .. name .. '...', 50))
        TEST_NAME = ''
        local ok, res = xpcall(fn, debug.traceback)
        if ok then
            log('[\027[32mPASS\027[0m]')
        else
            log('[\027[31mFAIL\027[0m]')
            if #TEST_NAME > 0 and not verbose then
                logf('  inside test [%s]:\n', TEST_NAME)
            end
            if ERROR_INFO_FN then ERROR_INFO_FN() end
            log(res)
        end
    end
end
