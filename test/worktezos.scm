;; library stanzas
(define _wss
  (let* ((_   (load "dune.scm"))
       ;; WARNING: arg to mibl-load-project is relative to cwd,
       ;; but arg to hash-table-ref below is relative to ws root,
       ;; which may not be the same.
       (arg

        ;; "test/dune/tezos/lib_clic"
        ;; "test/dune/tezos/lib_requester"
        ;; "test/dune/tezos/lib_stdlib_unix"
        "test/dune/tezos"

        )
       (wss (mibl-load-project arg)))
    wss))

(assoc-in  '(@ name) _wss)
(assoc-in  '(@ path) _wss)
(assoc-in  '(@ exports) _wss)
(define _pkgs (cadr (assoc-in  '(@ pkgs) _wss)))

(for-each (lambda (p) (format #t "~A\n" p)) (hash-table-keys _pkgs))

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
