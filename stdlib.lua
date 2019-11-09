-- "stdlib" stuff that is generic enough to just stick in global scope

function fmt(...)
    local fmt = ('%s '):rep(#{...})
    return fmt:format(...)
end

function logf(fmt, ...)
    io.stderr:write(fmt:format(...))
end

function log(...)
    logf(fmt(...) .. '\n')
end

function range(start, stop, step)
    local result = {}
    for i = start, stop, step do
        result[#result + 1] = i
    end
    return result
end

function enum(name, values)
    local container = {names = {}}
    local meta = {
        __tostring = function (v)
            return ('%s.%s'):format(name, container.names[v.idx])
        end
    }
    -- Strip comments
    values = values:gsub('%-%-[^\n]*\n', '')
    local group = nil
    for idx, value in ipairs(split(values, ',?[ \t\n]+')) do
        value = strip(value)
        -- Parse groups
        if value:sub(-2, -1) == ':{' then
            group = value:sub(1, -3)
        elseif value == '}' then
            group = nil
        -- Normal enum values
        elseif #value > 0 then
            local e_value = {idx=idx, group=group, enum=container}
            setmetatable(e_value, meta)
            assert(not container[value], str(value))
            container[value] = e_value
            container[idx] = e_value
            container.names[idx] = value
        end
    end
    return container
end

function iter_unpack(table)
    return coroutine.wrap(function()
        for i, item in pairs(table) do
            coroutine.yield(i, unpack(item))
        end
    end)
end

function iter_bytes(str)
    return ipairs{str:byte(1, -1)}
end

function strip(s)
    return s:gsub('^[ \t\n]+', ''):gsub('[ \t\n]+$', '')
end

function split(s, pattern)
    local result = {}
    while true do
        local start, stop = s:find(pattern)
        if start == nil then
            result[#result + 1] = s
            break
        end
        result[#result + 1] = s:sub(1, start - 1)
        s = s:sub(stop + 1)
    end
    return result
end

function str(value)
    local meta = getmetatable(value)
    if type(value) == 'table' and (not meta or not meta['__tostring']) then
        local s = ''
        for k, v in pairs(value) do
            s = s .. ('[%s] = %s, '):format(str(k), str(v))
        end
        return '{' .. s .. '}'
    elseif type(value) == 'string' then
        -- %q annoyingly replaces '\n' with '\\\n', that is, a backslash and
        -- then an actual newline. Replace the newline with an 'n'.
        return (('%q'):format(value):gsub('\n', 'n'))
    else
        return tostring(value)
    end
end

function astr(array)
    return '{' .. table.concat(amap(array, str), ', ') .. '}'
end

function amap(array, fn)
    local result = {}
    for _, v in ipairs(array) do
        result[#result + 1] = fn(v)
    end
    return result
end

function right_pad(str, len)
    return str .. (' '):rep(len - #str)
end

function concat(...)
    local result = {}
    for _, table in ipairs{...} do
        for _, item in ipairs(table) do
            result[#result + 1] = item
        end
    end
    return result
end

function merge(...)
    local result = {}
    for _, table in ipairs{...} do
        for key, value in pairs(table) do
            result[key] = value
        end
    end
    return result
end

function rep(v, n)
    local result = {}
    for _ = 1, n do
        result[#result + 1] = v
    end
    return result
end

-- Nested equality comparison. Returns a table {false, msg} if not equal,
-- or nil otherwise, mostly for an easy interface
local function equals(a, b, path)
    local meta_a, meta_b  = getmetatable(a), getmetatable(b)
    if type(a) == 'table' and type(b) == 'table' and
           (not meta_a or not meta_a['__tostring'] or
           not meta_b or not meta_b ['__tostring']) then
        local seen_keys = {}
        for k, v1 in pairs(a) do
            seen_keys[k] = true
            local v2 = b[k]
            local r = equals(v1, v2, concat(path, {k}))
            if r then return r end
        end
        for k, v2 in pairs(b) do
            if not seen_keys[k] then
                local v1 = a[k]
                local r = equals(v1, v2, concat(path, {k}))
                if r then return r end
            end
        end
    elseif a ~= b then
        msg = ('%s ~= %s'):format(str(a), str(b))
        if #path > 0 then
            path = '[' .. table.concat(path, '][') .. ']'
            msg = msg .. ', in index ' ..  path
        end
        return {false, msg}
    end
end

-- Hacky global variable to store a custom function that's called on errors
ERROR_INFO_FN = nil

function assert_neq(a, b, name, error_fn, expected)
    expected = expected or false
    local result, msg = unpack(equals(a, b, {}) or {true})
    msg = ('assertion%s failed%s'):format(name and ' ['..name..']' or '',
        msg and ': ' .. msg or '')
    ERROR_INFO_FN = error_fn
    assert(result == expected, msg)
    ERROR_INFO_FN = nil
end

function assert_eq(a, b, name, error_fn)
    return assert_neq(a, b, name, error_fn, true)
end
