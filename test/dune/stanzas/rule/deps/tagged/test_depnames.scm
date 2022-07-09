(load "dune/dune_actions.scm")

(begin
  (load "dune.scm")
  (let* ((pkg '((:ws-path "/Users/gar/obazl/mibl/test")
                (:pkg-path "dune/stanzas/rule/deps/glob")
                (:realpath "/Users/gar/obazl/mibl/test/dune/stanzas/rule/deps/glob")
                (:dune-stanzas
                 (rule (targets foo.ml)
                       ;; (deps (:< gen.exe))
                       (deps (:< gen.sh))
                       ;; (deps (:< ../gen.exe))
                       ;; (deps (:< gen/gen.exe))
                       ;; (deps (:< foo gen.sh))
                       ;; (deps
                       ;;  index.html
                       ;;  (:css (glob_files *.css))
                       ;;  foo.html
                       ;;  (:js foo.js bar.js)
                       ;;  (:img (glob_files *.png) (glob_files *.jpg))
                       ;;  )
                        )
                       (action (run %{<} %{targets})))))
         (stanza-alist (cdar (assoc-val :dune-stanzas pkg))))
    (expand-rule-deps pkg stanza-alist)))
