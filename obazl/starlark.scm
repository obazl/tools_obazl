(format #t "loading obazl/starlark.scm\n")

;; GLOBAL CONFIGS:
(define *ns-topdown* #t)
(define *ns-archives* #t) ;; otherwise emit ocaml_ns_library
(define *agg-library* #f) ;; emit ocaml_library for unwrapped 'library'
(define *build-dyads* #t) ;; emit ocaml_signature for dyads

(load "dune.scm")
(load "starlark/templates.scm")
(load "starlark/conversion.scm")
(load "starlark/executables.scm")
(load "starlark/aggregates.scm")
(load "starlark/singletons.scm")
(load "starlark/ppx.scm")
(load "starlark/rules.scm")

(format #t "loaded obazl/starlark.scm\n")
