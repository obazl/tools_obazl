;; library stanzas
(define _pkg
  (let* ((_   (load "dune.scm"))
       ;; WARNING: arg to mibl-load-project is relative to cwd,
       ;; but arg to hash-table-ref below is relative to ws root,
       ;; which may not be the same.
       (arg

        ;; "test/dune/stanzas/rule/action/bash"
        ;; "test/dune/stanzas/rule/action/cat"
        ;; "test/dune/stanzas/rule/action/cmp"
        ;; "test/dune/stanzas/rule/action/copy"
        ;; "test/dune/stanzas/rule/action/diff"
        ;; "test/dune/stanzas/rule/action/mixed"
        ;; "test/dune/stanzas/rule/action/system"
        ;; "test/dune/stanzas/rule/action/with-stdout-to/bash"
        "test/dune/stanzas/rule/action/with-stdout-to/cat"
        ;; "test/dune/stanzas/rule/action/with-stdout-to/chdir"
        ;; "test/dune/stanzas/rule/action/with-stdout-to/run"
        ;; "test/dune/stanzas/rule/action/write-file"

        ;; "test/dune/stanzas/rule/action/progn"
        ;; "test/dune/stanzas/rule/action/echo"
        ;; "test/dune/stanzas/rule/action/chdir"

        ;; "test/dune/stanzas/rule/action/run/bash"
        ;; "test/dune/stanzas/rule/action/run/cp"
        ;; "test/dune/stanzas/rule/action/run/env"
        ;; "test/dune/stanzas/rule/action/run/exe"
        ;; "test/dune/stanzas/rule/action/run/interop/rust"
        ;; "test/dune/stanzas/rule/action/run/literal/cwe"
        ;; "test/dune/stanzas/rule/action/run/literal/subdir"
        ;; "test/dune/stanzas/rule/action/run/nostatic"
        ;; "test/dune/stanzas/rule/action/run/tools/a"
        ;; "test/dune/stanzas/rule/action/run/var/a"
        ;; "test/dune/stanzas/rule/action/run/var/b"
        ;; "test/dune/stanzas/rule/action/run/var/tag"
        ;; "test/dune/stanzas/rule/action/run/with_deps"

        )
       (wss (mibl-load-project arg))
       (pkgs (cadr (assoc-in '(@ pkgs) wss)))
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
