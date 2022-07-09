;; deps/literals/test.scm

(define literals
  '(
    ;; ((../../foo/a.ml a.mli)
    ;;  (:_ "dune/stanzas/rule/deps/foo/a.ml" "dune/stanzas/rule/deps/literals/a.mli"))

    ;; ((../foo.ml ../globtest.ml ../relative/p.ml)
    ;;  (:_ "dune/stanzas/rule/deps/relative/p.ml" "dune/stanzas/rule/deps/globtest.ml" "dune/stanzas/rule/deps/foo.ml"))

    ((index.html (:css (glob_files *.css)))
     ((::css "dune/stanzas/rule/deps/literals/style.css"
             "dune/stanzas/rule/deps/literals/custom.css")
      (:_ "dune/stanzas/rule/deps/literals/index.html")))

    ;; ((index.html (:css (glob_files *.css))
    ;;              foo.html (:js foo.js bar.js)
    ;;              (:img (glob_files *.png) (glob_files *.jpg)))
    ;;  (output))

    ))

(define paths
  '((:ws-path "/Users/gar/obazl/mibl/test")
    (:pkg-path "dune/stanzas/rule/deps/glob")
    (:realpath "/Users/gar/obazl/mibl/test/dune/stanzas/rule/deps/glob")))

(begin
  (load "dune.scm")
  (let* ((paths '((:ws-path "/Users/gar/obazl/mibl/test")
                  (:pkg-path "dune/stanzas/rule/deps/literals")
                  (:realpath
                   "/Users/gar/obazl/mibl/test/dune/stanzas/rule/deps/literals")))

         (rs (map (lambda (lit-pair)
                    ;; ((a.ml a.mli) ("a.ml" "a.mli"))
                    (format #t "Literals: ~A\n" lit-pair)
                    (let* ((lits (car lit-pair))
                           (stanza-alist `((targets foo bar)
                                           ;; INSERT PATTERN HERE:
                                           ;; (deps ,lits)
                                           ,(cons 'deps lits)
                                           (action (run foo %{targets}))))
                           (_ (format #t "Stanza-Alist: ~A\n" stanza-alist))
                           (result (expand-rule-deps paths stanza-alist)))
                      (format #t "RES: ~A\n" result)
                      (format #t "EXP: ~A\n" (cadr lit-pair))
                      ;; (set-equal? (assoc-val ':_ result)
                      ;;             (assoc-val ':_ (cdr lit-pair)))))
                      (equal? result (cadr lit-pair))))
                  ;; result))
                  literals)))
    rs))
