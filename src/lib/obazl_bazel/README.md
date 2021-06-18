# build file parsing etc.

starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference

bazelbuild buildfile parser stuff:  https://github.com/bazelbuild/buildtools/tree/master/build

lexer:  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go



generate tokens.h from tokens.txt

rg -N . src/lib/obazl_bazel/tokens.txt | sort | uniq > toks.h
nl -w 2 -n rz -s ' ' toks.h > toksnl.h

paste -d ' ' toksnl.h toks.h | cut -d ' ' -f2,3 | sed 's/^/#define /' > src/lib/obazl/bazel/tokens.h

generate tokens.c, with token_name lookup table

sed 's/\(.*\)/[\1] = "\1",/' toks.h > src/lib/obazl_bazel/tokens.c




