;; executable stanzas
(define pkg
  (let* ((_   (load "dune.scm"))
       ;; WARNING: arg to mibl-load-project is relative to cwd,
       ;; but arg to hash-table-ref below is relative to ws root,
       ;; which may not be the same.
       (arg

        ;; "test/dune/stanzas/executable/main_dyad"
        ;; "test/dune/stanzas/executable/main_nodeps"
        ;; "test/dune/stanzas/executable/main_singleton"

        "test/dune/stanzas/executable/flags"

        ;; "test/dune/stanzas/executable/mwe"

        ;; "test/dune/stanzas/executable/manifest/default"
        ;; "test/dune/stanzas/executable/manifest/standard"
        ;; "test/dune/stanzas/executable/manifest/exclusions"
        ;; "test/dune/stanzas/executable/manifest/inclusions"

        ;; "test/dune/stanzas/executable/select"

        ;; "test/dune/stanzas/executable/s_packer"

        ;; "bin"
        )
       (wss (mibl-load-project arg))
       (pkgs (cadr (assoc-in '(@ pkgs) wss)))
       (pkg (hash-table-ref pkgs arg))
       (nzs (dune-pkg->mibl pkg))
       )
    nzs))

(let* ((_   (load "bazel.scm")))
  (mibl-pkg->starlark pkg)
  )

       ;; (pkg-path (car (assoc-val :pkg-path pkg)))
       ;; (build.mibl (string-append pkg-path "/BUILD.mibl"))
    ;; (let ((outp (open-output-file build.mibl)))
    ;;   ;;(write nzs outp)
    ;;   (pretty-print nzs outp)
    ;;   (close-output-port outp))
    ;; (pretty-print nzs (current-output-port))
