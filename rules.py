import os

def rules(ctx):
    ts_inc = ['-Itree-sitter/lib/include', '-Itree-sitter/lib/src']

    # Input files. Entries are (dir, flags, files)
    src_files = [
        ('.', ts_inc, ['zmt.c', ]),
        ('tree-sitter/lib/src', ts_inc, ['lib.c']),
        ('tree-sitter-c/src', ts_inc, ['parser.c']),
    ]

    c_flags = ['-fcolor-diagnostics', '-Wall', '-Werror']
    configs = [
        ['rel', ['-O3']],
        ['deb', ['-g']],
    ]

    for [conf_path, conf_flags] in configs:
        o_files = []
        for [path, flags, files] in src_files:
            for f in files:
                base, ext = os.path.splitext(f)
                c_file = '%s/%s' % (path, f)
                o_file = '_out/%s/%s/%s.o' % (conf_path, path, base)
                d_file = '_out/%s/%s/%s.d' % (conf_path, path, base)
                cmd = ['cc', '-o', o_file, '-c', c_file, '-MD', *c_flags,
                        *conf_flags, *flags]
                ctx.add_rule(o_file, [c_file], cmd, d_file=d_file)
                o_files += [o_file]

        # Main shared library
        bin_file = '_out/%s/zmt.so' % conf_path
        cmd = ['cc', '-fPIC', '-shared', '-o', bin_file, '-lncurses',
                *c_flags, *conf_flags, *o_files]
        ctx.add_rule(bin_file, o_files, cmd)

    # Preprocessed headers, with some options to make luajit grok it
    pre_headers = ['zmt.h']
    pre_cmd = ['cc', '-P', '-E', '-std=c99', '-U__BLOCKS__', '-D_Nullable=',
            '-D_Nonnull=', '-o', '_out/pre.h'] + ts_inc + pre_headers
    ctx.add_rule('_out/pre.h', pre_headers, pre_cmd)
