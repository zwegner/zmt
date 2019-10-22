ffi = require('ffi')

function load_lib(lib_path, header)
    local lib = ffi.load(lib_path)
    local defs = io.open(header)
    ffi.cdef(defs:read('*all'))
    defs:close()
    return lib
end
zmt = load_lib('_out/zmt.so', '_out/zmt.h')

-- Iterate through the piece tree
function iter_pieces(root)
    return coroutine.wrap(function()
        local iter = ffi.new('meta_iter_t[1]')
        local node = zmt.iter_start(iter, root)
        while node ~= nil do
            local l = node.leaf
            -- end is a keyword. Oh well, it's the proper variable name
            local data = ffi.string(l.chunk_data + l.start, l['end'] - l.start)
            coroutine.yield(data)
            node = zmt.iter_next(iter)
        end
    end)
end

for piece in iter_pieces(root) do
    io.stdout:write(data)
end

piece = zmt.map_file(arg[1])
root = zmt.dumb_read_data(piece);
