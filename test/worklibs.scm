;; library stanzas
(define _pkg
  (let* ((_   (load "dune.scm"))
       ;; WARNING: arg to mibl-load-project is relative to cwd,
       ;; but arg to hash-table-ref below is relative to ws root,
       ;; which may not be the same.
       (arg

        ;;;; unwrapped:
        ;; "test/dune/stanzas/library/unwrapped/default"
        ;; "test/dune/stanzas/library/unwrapped/standard"
        ;; "test/dune/stanzas/library/unwrapped/exclusions"
        ;; "test/dune/stanzas/library/unwrapped/inclusions"

        ;;;; 'select' libdeps
        ;; ;; w/o directs deps:
        ;; "test/dune/stanzas/library/select/sigs"
        ;; "test/dune/stanzas/library/select/structs"
        ;; ;; w/directs deps:
        ;; "test/dune/stanzas/library/select/sigs_directs"
        ;; "test/dune/stanzas/library/select/structs_directs"
        ;; ;; mixed
        ;; "test/dune/stanzas/library/select/mixed"
        ;; "test/dune/stanzas/library/select/tezos"

        ;; tezos
        ;; "test/dune/tezos/lib_clic"
        ;; "test/dune/tezos/lib_requester"
        ;; "test/dune/tezos/lib_stblib_unix"
        ;; "test/a"

        ;;;; submodules ('modules' field)
        ;; "test/dune/stanzas/library/submodules/default"
        ;; "test/dune/stanzas/library/submodules/standard"
        ;; "test/dune/stanzas/library/submodules/exclusions"
        ;; "test/dune/stanzas/library/submodules/inclusions"
        ;; "test/dune/stanzas/library/submodules/inclusions_sigs"
        ;; "test/dune/stanzas/library/submodules/subsigs"

        ;; "test/dune/ppx/long"
        ;; "test/dune/ppx/mwe/exe"
        ;; "test/dune/ppx/mwe/lib/singleton"
        ;; "test/dune/ppx/mwe/lib/dyad"
        ;; "test/dune/ppx/pp_deps/exe"
        "test/dune/ppx/ppx_args"
        ;; "test/dune/ppx/ppx_data/single"
        ;; "test/dune/ppx/ppx_data/multiple"
        ;; "test/dune/ppx/pp_deps/lib/mwe"

        )
       (wss (mibl-load-project arg)) (pkgs (cadr (assoc-in '(@ pkgs) wss)))
       (pkg (hash-table-ref pkgs arg))
       (nzs (dune-pkg->mibl pkg))
       )
    nzs))

(let* ((_   (load "bazel.scm")))
  (mibl-pkg->starlark _pkg)
  )

       ;; (pkg-path (car (assoc-val :pkg-path pkg)))
       ;; (build.mibl (string-append pkg-path "/BUILD.mibl"))
    ;; (let ((outp (open-output-file build.mibl)))
    ;;   ;;(write nzs outp)
    ;;   (pretty-print nzs outp)
    ;;   (close-output-port outp))
    ;; (pretty-print nzs (current-output-port))
