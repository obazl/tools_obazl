;; library stanzas
(define pkg
  (let* ((_   (load "dune.scm"))
       ;; WARNING: arg to mibl-load-project is relative to cwd,
       ;; but arg to hash-table-ref below is relative to ws root,
       ;; which may not be the same.
       (arg

        ;;;; submodules ('modules' field)
        ;; "test/dune/stanzas/library/submodules/default"
        ;; "test/dune/stanzas/library/submodules/standard"
        ;; "test/dune/stanzas/library/submodules/exclusions"
        ;; "test/dune/stanzas/library/submodules/inclusions"
        ;; "test/dune/stanzas/library/submodules/inclusions_sigs"
        "test/dune/stanzas/library/submodules/subsigs"

        ;;;; unwrapped:
        ;; "test/dune/stanzas/library/unwrapped/default"
        ;; "test/dune/stanzas/library/unwrapped/standard"
        ;; "test/dune/stanzas/library/unwrapped/exclusions"
        ;; "test/dune/stanzas/library/unwrapped/inclusions"

        ;;;; 'select' libdeps
        ;; ;; w/o directs deps:
        ;; "test/dune/stanzas/library/deps/select/sigs"
        ;; "test/dune/stanzas/library/deps/select/structs"
        ;; ;; w/directs deps:
        ;; "test/dune/stanzas/library/deps/select/sigs_directs"
        ;; "test/dune/stanzas/library/deps/select/structs_directs"
        ;; ;; mixed
        ;; "test/dune/stanzas/library/deps/select/mixed"
        ;; "test/dune/stanzas/library/deps/select/tezos"

        ;; tezos
        ;; "test/dune/tezos/lib_clic"
        ;; "test/dune/tezos/lib_requester"
        ;; "test/dune/tezos/lib_stblib_unix"
        ;; "test/a"
        )
       (wss (mibl-load-project arg)) (pkgs (cadr (assoc-in '(@ pkgs) wss)))
       (pkg (hash-table-ref pkgs arg))
       (nzs (dune-pkg->mibl pkg))
       )
    nzs))

       ;; (pkg-path (car (assoc-val :pkg-path pkg)))
       ;; (build.mibl (string-append pkg-path "/BUILD.mibl"))
    ;; (let ((outp (open-output-file build.mibl)))
    ;;   ;;(write nzs outp)
    ;;   (pretty-print nzs outp)
    ;;   (close-output-port outp))
    ;; (pretty-print nzs (current-output-port))
