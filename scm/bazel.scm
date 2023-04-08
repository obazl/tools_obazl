(if #t ; (or *mibl-debug-s7-loads* *mibl-debug-s7*)
    (format #t "loading starlark.scm\n"))

(define *select-protases* '())

(load "libmibl.scm")
;; (load "dune.scm")

(load "bazel/parsers.scm")
(load "bazel/singletons.scm")
(load "bazel/cc.scm")
(load "bazel/deps.scm")
(load "bazel/emit.scm")
(load "bazel/headers.scm")
(load "bazel/executables_starlark.scm")
(load "bazel/aggregates.scm")
(load "bazel/conditionals.scm")
(load "bazel/opam.scm")
(load "bazel/prologues.scm")
(load "bazel/ppx_starlark.scm")
(load "bazel/tests.scm")
(load "bazel/genrule.scm")
(load "bazel/profiles.scm")
(load "bazel/rules_starlark.scm")
(load "bazel/shell.scm")
(load "bazel/tools.scm")
(load "bazel/attributes.scm")
(load "bazel/filegroups.scm")
(load "bazel/starlark.scm")

(if *mibl-debug-s7*
    (format #t "loaded starlark.scm\n"))
