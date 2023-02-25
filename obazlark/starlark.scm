(if *debugging*
    (format #t "loading obazl/starlark.scm\n"))

(define *select-protases* '())

(load "dune.scm")

(load "starlark/parsers.scm")
(load "starlark/singletons.scm")
(load "starlark/cc.scm")
(load "starlark/emit.scm")
(load "starlark/headers.scm")
(load "starlark/executables_starlark.scm")
(load "starlark/aggregates.scm")
(load "starlark/conditionals.scm")
(load "starlark/opam.scm")
(load "starlark/ppx_starlark.scm")
(load "starlark/tests.scm")
(load "starlark/genrule.scm")
(load "starlark/profiles.scm")
(load "starlark/rules_starlark.scm")
(load "starlark/shell.scm")
(load "starlark/tools.scm")
(load "starlark/attributes.scm")
(load "starlark/filegroups.scm")
(load "starlark/starlark.scm")

(if *debugging*
    (format #t "loaded obazl/starlark.scm\n"))
