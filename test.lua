#!/usr/bin/env luajit
require('stdlib')
local zmt = require('zmt')

-- Helpers

local TEST_NAME = ''
local function test_name(name)
    TEST_NAME = name
end

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
        local iter, node = zmt.iter_start(tree, line, byte)
        assert_eq(node, nil, 'big jump finishes the iterator')
        assert_eq(iter[0].start_offset.byte, tree_size.byte,
                'big jump gets the right offset')
    end

    -- Verify that all the line lengths are correct
    for i, line in ipairs(lines) do
        assert_eq(zmt.zmt.get_tree_line_length(tree, i - 1), #line,
            ('length of line %s matches'):format(i))
    end
end

local function tree_insert_bytes(tree, offset, data)
    return zmt.zmt.insert_bytes_at_offset(tree, 0, offset, data, #data)
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
    end},

    {'tree 1', function ()
        test_name('basic tree 1')
        local tree = zmt.zmt.create_tree(true)
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
            tree = zmt.zmt.split_at_offset(tree, 0, i)
            check_tree(tree, {'a', 'c', '\nb'}, {'ac\n', 'b'})
        end
    end},

    {'tree 2', function ()
        test_name('tree 2')
        local tree = zmt.zmt.create_tree(true)

        local piece = '0123456789\n'
        for i = 1, 10 do
            tree = tree_insert_bytes(tree, 1e10, piece)
        end
        local pieces = rep(piece, 10)
        local lines = concat(pieces, {''})
        check_tree(tree, pieces, lines)

        -- Insert a bunch of times into the middle of lines
        test_name('tree 2--split lines')
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

    {'tree 3--deletion', function ()
        test_name('setup')
        -- Create a tree with 4x 4-byte nodes
        local data = '0123456789abcdef'
        local chunk = zmt.zmt.write_new_chunk(data, #data)
        local tree = zmt.zmt.dumb_read_data(chunk, 4)
        check_tree(tree, {'0123', '4567', '89ab', 'cdef'}, {data})

        -- Some of these fail now
        for _, split_offset in ipairs{-1, 0, 2,
                --4, 6, 8, 9 -- current failures
                } do
            local subtest = ('split at %d'):format(split_offset)
            test_name(subtest)
            -- Split the tree at the given offset
            local tree = tree
            if split_offset >= 0 then
                tree = zmt.zmt.split_at_offset(tree, 0, split_offset)
            end

            -- Loop through multiple permutations of starting/stopping on a node
            -- boundary or within a node
            for start = 4, 5 do
                for stop = 7, 9 do
                    test_name(('%s: delete %s-%s'):format(subtest, start, stop))

                    -- Create the expected piece state by chopping the full data
                    -- string up
                    local pieces = {}
                    local p_i = 1
                    local i = 1
                    for _, c in iter_bytes(data) do
                        -- Append to the current piece if this char isn't deleted
                        if i <= start or i > stop then
                            pieces[p_i] = (pieces[p_i] or '') .. string.char(c)
                        end
                        -- Move to the next piece if this is a boundary
                        if (i == start or i == stop or i % 4 == 0 or
                            i == split_offset) and pieces[p_i] then
                            p_i = p_i + 1
                        end
                        i = i + 1
                    end
                    local new_line = data:sub(1, start) .. data:sub(stop+1)

                    -- Delete the range and check the output
                    local td = zmt.zmt.delete_byte_range(tree, start, stop)
                    check_tree(td, pieces, {new_line})
                end
            end
        end
    end},
}

local test_filter = #arg > 0 and arg[1]

for i, name, fn in iter_unpack(TESTS) do
    if not test_filter or name:find(test_filter) then
        io.stdout:write(right_pad('Running test ' .. name .. '...', 50))
        TEST_NAME = ''
        local ok, res = xpcall(fn, debug.traceback)
        if ok then
            print('[\027[32mPASS\027[0m]')
        else
            print('[\027[31mFAIL\027[0m]')
            if #TEST_NAME > 0 then
                print(('  inside test [%s]:'):format(TEST_NAME))
            end
            print(res)
        end
    end
end
