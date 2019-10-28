-- "stdlib" stuff that is generic enough to just stick in global scope

function logf(fmt, ...)
    io.stderr:write(fmt:format(...) .. '\n')
end

function log(...)
    local fmt = ('%s '):rep(#{...})
    logf(fmt, ...)
end

function range(start, stop, step)
    local result = {}
    for i = start, stop, step do
        result[#result + 1] = i
    end
    return result
end

function enum(stop)
    return unpack(range(1, stop, 1))
end

function iter(...)
    return ipairs({...})
end

function iter_bytes(str)
    return ipairs({str:byte(1, -1)})
end

function str(value)
    if type(value) == 'table' then
        local s = ''
        for k, v in pairs(value) do
            s = s .. ('[%s] = %s, '):format(str(k), str(v))
        end
        return '{' .. s .. '}'
    elseif type(value) == 'string' then
        -- %q annoyingly replaces '\n' with '\\\n', that is, a backslash and
        -- then an actual newline. Replace the newline with an 'n'.
        return ('%q'):format(value):gsub('\n', 'n')
    else
        return tostring(value)
    end
end
