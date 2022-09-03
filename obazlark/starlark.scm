;; (format #t "loading obazl/starlark.scm\n")

(define *select-protases* '())

(load "dune.scm")

(load "starlark/headers.scm")
(load "starlark/executables.scm")
(load "starlark/aggregates.scm")
(load "starlark/conditionals.scm")
(load "starlark/singletons.scm")
(load "starlark/ppx.scm")
(load "starlark/emit.scm")
(load "starlark/genrule.scm")
(load "starlark/profiles.scm")
(load "starlark/rules.scm")
(load "starlark/tools.scm")
(load "starlark/attributes.scm")
(load "starlark/filegroups.scm")
(load "starlark/parsers.scm")
(load "starlark/starlark.scm")
(load "starlark/tests.scm")

;; (format #t "loaded obazl/starlark.scm\n")
