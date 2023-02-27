(load "bash.scm")

(define (-find-match-in-stanza key pkg-path stanza)
  (if (or *debug-genrules* *debugging*)
      (format #t "~A: ~A~%" (blue "-find-match-in-stanza") key))
  (let* ((key (if (keyword? key) key (string->keyword key)))
         (deps (assoc-val :deps stanza))
         (found (find-if (lambda (dep)
                           (if (or *debug-genrules* *debugging*)
                               (format #t "~A: ~A~%" (yellow "checking dep") dep))
                           (eq? key (car dep))) deps)))
    (if (or *debug-genrules* *debugging*)
        (format #t "~A: ~A~%" (red "found") found))
    (if found
        (let* ((lbl (cdr found))
               (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (yellow "lbl") lbl)))
               (pkg (assoc-val :pkg lbl))
               (tgt (if-let ((t (assoc-val :tgt lbl)))
                            (format #f "$(rootpath ~A)" (cdr t))
                            (if-let ((t (assoc-val ::glob #|:tgts|# lbl)))
                                    (format #f "$(rootpaths ~A)" t)
                                    (error 'fixme
                                           (format #f "no tgt/tgts in dep: ~A" found)))))
               )
          tgt)
        (let ((outputs (assoc-val :outputs stanza))
              (found (find-if (lambda (out)
                                (if (or *debug-genrules* *debugging*)
                                    (format #t "~A: ~A~%" (yellow "checking out") out))
                                (eq? key (car out))) outputs)))
          (if found
              "bar"
              (error 'fixme "TODO: search exports"))))))

(define (-resolve-match match pkg-path stanza)
  (if (or *debug-genrules* *debugging*)
      (begin
        (format #t "~A: ~A~%" (blue "-resolve-match") match)
        (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
        (format #t "~A: ~A~%" (blue "stanza") stanza)))
  (let* ((key (string->keyword match)))
    (case key
      ((:deps)
       (let* ((dassoc (assoc key stanza))
              (replace (map (lambda (dep)
                              (if (or *debug-genrules* *debugging*)
                                  (format #t "~A: ~A~%" (yellow "dep") dep))
                              ;;FIXME: support :fg
                              ;;FIXME: match paths
                              (if-let ((t (assoc :tgt (cdr dep))))
                                      (format #f "$(rootpath :~A)" (cdr t))
                                      (if-let ((t (assoc ::glob #|:tgts|# (cdr dep))))
                                              (format #f "$(rootpaths :~A)" (cdr t))
                                              (error 'fixme
                                                     (format #f "missing tgt/tgts: ~A" dep)))))
                            (cdr dassoc))))
         (if (or *debug-genrules* *debugging*)
             (format #t "~A: ~A~%" (red "replace") replace))
         (string-join replace)))
      (else
       (-find-match-in-stanza key pkg-path stanza)))))

(define (-expand-outputs pkg-path stanza)
  (if (or *debug-genrules* *debugging*)
      (format #t "~A: ~A~%" (blue "-expand-outputs") stanza))
  (let ((outs (assoc-val :outputs stanza)))
    (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (yellow "outs") outs))
    "FOOBAR"))

;; FIXME: account for :ctx, which is derived from (chdir ...) in dune
;; the target should be written to the :ctx dir?
;; but then what if multiple rules chdir to same dir? e.g. %{workspace_root}
;; possible resolution: handle all :ctx rules in a separate pass?
;; for now just emit a comment
(define (starlark-emit-genrule outp pkg-path stanza)
  (if (or *debug-genrules* *debugging*)
      (format #t "~A: ~A\n" (bgblue "starlark-emit-genrule") stanza))
  (let* ((action (assoc-val :actions stanza))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (uwhite "action") action)))
         (tool (cadr (assoc-in '(:cmd :tool) action)))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (uwhite "tool") tool)))
         (bash-cmd? (eq? tool 'bash))
         (deps (assoc-val :deps stanza))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (cyan "deps") deps)))
         (srcs (deps->srcs-attr pkg-path tool deps))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (cyan "genrule srcs") srcs)))
         ;; (_ (error 'X "stop genrule"))
         ;; FIXME: derive from :args, :stdout, etc.
         ;; if %{targets} is in cmd string, ...
         ;; else if we have (:stdout ...), ...
         (with-stdout? (assoc-in '(:actions :stdout) stanza))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (ucyan "with-stdout?") with-stdout?)))
         (outputs (assoc-val :outputs stanza))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (ucyan "outputs") outputs)))
         (outs (if outputs (outputs->outs-attr pkg-path outputs) '()))
         (_ (if (or *debug-genrules* *debugging*) (format #t "~A: ~A~%" (ucyan "outs") outs)))

         (name (if (truthy? outs) (format #f "__~A__" (outs 0))
                   (if (truthy? srcs)
                       (string-left-trim ":" (srcs 0))
                       (if (truthy? deps)
                           (if (keyword? (caar deps))
                               (keyword->symbol (caar deps))
                               (caar deps)
                                         )))))
         )

    ;; progn: list of actions. should be just one?
    (if (or *debug-genrules* *debugging*)
        (for-each
         (lambda (cmd)
           (if (eq? :cmd (car cmd))
               (begin
                 (format #t "GENRULE ACTION (A): ~A~%" cmd)
                 (if (eq? :stdout (car cmd))
                     (format #t "GENRULE STDOUT (A): ~A~%" cmd)
                     (format #t "GENRULE UNHANDLED (A): ~A~%" cmd)
                     ;;(error 'fixme (format #f "unknown genrule cmd: ~A" cmd))
                     ))))
         action))

    (if-let ((ctx (assoc-in '(:actions :ctx) stanza)))
            (begin
              (format outp "## omitted:\n")
              (format outp "## (chdir ~A (run ~A ...))\n\n"
                    (cadr ctx)
                    (cadr (assoc-in '(:actions :cmd :tool) stanza))))
            ;; else
            (begin
              (if (or *debug-genrules* *debugging*) (format #t "  outs: ~A~%" outs))

              (if (list? stanza)
                  (begin
                    (if (or *debug-genrules* *debugging*)
                        (format #t "~A~%" (uwhite "emitting genrule")))
                    ;; (format outp "## ~A\n" (assoc-val 'dune (stanza)))
                    ;; (format outp "## (\n")
                    ;; (for-each (lambda (sexp)
                    ;;             (format outp "##   ~A\n" sexp))
                    ;;           stanza)
                    ;; (format outp "## )\n")

                    (format outp "########\n")
                    (format outp "genrule(\n")
                    (format outp "    outs  = [\n")
                    (if (truthy? outs)
                        (format outp "~{        \"~A\"~^,\n~}\n" outs)
                        (if (equal? ::diff tool)
                            (format outp "        \"~A.diff.out\"~%" name)))
                    ;; (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    (format outp "    ],\n")

                    (format outp "    name  = \"~A\",\n" name)

                    (if (truthy? srcs)
                        (if bash-cmd?
                            (emit-bash-srcs outp srcs pkg-path stanza)
                            (emit-bash-srcs outp srcs pkg-path stanza)
                            ;; (begin
                            ;;   (format #t "~A: ~A~%" (red "SrcS") srcs)
                            ;;   (format outp "    SRCS  = [\n")
                            ;;   (format outp "~{        \"~A\"~^,\n~}\n" srcs)
                            ;;   (format outp "    ],\n"))
                            ))

                    (if (or *debug-genrules* *debugging*)
                        (format #t "~A~%" (uwhite "emitting tool attrib")))
                    (if bash-cmd?
                        (emit-bash-cmd outp with-stdout? outs pkg-path stanza)
                        (emit-shell-cmd outp tool
                                        with-stdout?
                                        srcs
                                        deps ;; (dissoc '(::tools) deps)
                                        action
                                        outputs ;; outs
                                        pkg-path
                                        stanza))

                    (if (and (not (member tool shell-tool-kws))
                             (not bash-cmd?))
                        (begin
                          (format outp "    exec_tools = [\n")
                          (for-each ;; fixme: don't iterate this twice
                           (lambda (cmd)
                             (if (or *debug-genrules* *debugging*) (format #t "Processing CMD: ~A~%" cmd))
                             (if (eq? :cmd (car cmd))
                                 (let-values
                                     (((tool-dep? tool args)
                                       (derive-cmd pkg-path cmd deps outputs)))
                                   (if (or *debug-genrules* *debugging*)
                                       (begin
                                         (format #t "cmd Tool-Dep?: ~A~%" tool-dep?)
                                         (format #t "cmd TooL: ~A~%" tool)
                                         (format #t "cmd ArgS: ~A~%" args)))

                                   (if (not (null? tool-dep?))
                                       (begin
                                         (format outp "~{        \"~A\"~^,\n~}\n"
                                                 (list tool)) ;;FIXME: support multiple tools
                                         )))))
                           action)
                          (format outp "    ]\n")))
                    (format outp ")~%")
                    (newline outp))
                    )))
            ))

