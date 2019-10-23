def rules(ctx):
    files = ['zmt']

    #c_flags = ['-fcolor-diagnostics', '-O3']
    c_flags = ['-fcolor-diagnostics', '-g']

    o_files = []
    for base in files:
        c_file = '%s.c' % base
        o_file = '_out/%s.o' % base
        d_file = '_out/%s.d' % base
        cmd = ['cc', '-o', o_file, '-c', c_file, '-MD'] + c_flags
        ctx.add_rule(o_file, [c_file], cmd, d_file=d_file)
        o_files += [o_file]

    # Main binary
    bin_file = '_out/zmt.so'
    cmd = ['cc', '-fPIC', '-shared', '-o', bin_file] + o_files
    ctx.add_rule(bin_file, o_files, cmd)

    # Preprocessed headers, with some options to make luajit grok it
    pre_headers = ['zmt.h']
    pre_cmd = ['cc', '-P', '-E', '-std=c99', '-U__BLOCKS__', '-D_Nullable=',
            '-D_Nonnull=', '-o', '_out/pre.h'] + pre_headers
    ctx.add_rule('_out/pre.h', pre_headers, pre_cmd)
