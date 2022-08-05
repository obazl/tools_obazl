;; FIXME: account for :ctx, which is derived from (chdir ...) in dune
;; the target should be written to the :ctx dir?
;; but then what if multiple rules chdir to same dir? e.g. %{workspace_root}
;; possible resolution: handle all :ctx rules in a separate pass?
;; for now just emit a comment
(define (starlark-emit-genrule outp pkg-path stanza)
  (format #t "~A: ~A\n" (magenta "STARLARK-EMIT-GENRULE") stanza)
  (let* ((action (assoc-val :actions stanza))
         (_ (format #t "action: ~A~%" action))
         (tool (cadr (assoc-in '(:cmd :tool) action)))
         (_ (format #t "tool: ~A~%" tool))
         (deps (assoc-val :deps stanza))
         (_ (format #t "~A: ~A~%" (cyan "deps") deps))
         (srcs (deps->srcs-attr pkg-path deps))
         (_ (format #t "~A: ~A~%" (cyan "srcs") srcs))

         ;; FIXME: derive from :args, :stdout, etc.
         ;; if %{targets} is in cmd string, ...
         ;; else if we have (:stdout ...), ...
         (outputs (assoc-val :outputs stanza))
         (_ (format #t "outputs: ~A~%" outputs))
         (outs (-outputs->outs-attr pkg-path outputs))

         (name (format #f "__~A__"
                       (outs 0))))

    ;; progn: list of actions. should be just one?
    (for-each
     (lambda (cmd)
       (if (eq? :cmd (car cmd))
           (format #t "GENRULE ACTION: ~A~%" cmd)
           (if (eq? :stdout (car cmd))
               (format #t "GENRULE STDOUT: ~A~%" cmd)
               (error 'fixme (format #f "unknown genrule cmd: ~A" cmd)))))
     action)

    (if-let ((ctx (assoc-in '(:actions :ctx) stanza)))
            (begin
              (format outp "## omitted:\n")
              (format outp "## (chdir ~A (run ~A ...))\n\n"
                    (cadr ctx)
                    (cadr (assoc-in '(:actions :cmd :tool) stanza))))
            ;; else
            (begin
              (format #t "  outs: ~A~%" outs)

              (format outp "################  rule  ################\n")
              (if (list? stanza)
                  (begin
                    ;; (format outp "## ~A\n" (assoc-val 'dune (stanza)))
                    ;; (format outp "## (\n")
                    ;; (for-each (lambda (sexp)
                    ;;             (format outp "##   ~A\n" sexp))
                    ;;           stanza)
                    ;; (format outp "## )\n")

                    (format outp "genrule(\n")
                    (format outp "    name  = \"~A\",\n" name)

                    (if (not (null? srcs))
                        (begin
                          (format #t "~A: ~A~%" (red "SrcS") srcs)
                          (format outp "    srcs  = [\n")
                          (format outp "~{        \"~A\"~^,\n~}\n" srcs)
                          (format outp "    ],\n")))

                    (format outp "    outs  = [\n")
                    (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    ;; (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    (format outp "    ],\n")

                    (format outp "    cmd   = \" \".join([\n")
                    (for-each
                     (lambda (cmd)
                       (format #t "~A: ~A~%" (magenta "PROCESSING cmd") cmd)
                       (if (eq? :cmd (car cmd))
                           (let-values
                               (((tool-dep? tool xargs)
                                 (-derive-cmd pkg-path cmd deps outputs)))
                             ;; (format #t "~A:\n" (red "derived cmd"))
                             (format #t "~A: ~A~%" (magenta "tool-dep?")
                                     tool-dep?)
                             (format #t "~A: ~A~%" (magenta "tool") tool)
                             (format #t "~A: ~A~%" (magenta "XARGS") xargs)

                             (if tool-dep?
                                 ;; HACK: $(location ...)
                                 (format outp "        \"$(location ~A)\",\n" tool)
                                 (format outp "        \"~A\",\n" tool))
                             (format outp "~{        \"~A\",~^\n~}\n" xargs))
                           ;; else
                           (if (eq? :stdout (car cmd))
                               (format outp "        \"> $@\"\n")
                               (error 'unknown-cmd
                                      (format #f "unknown cmd: ~A" cmd)))
                           ))
                     action)
                    (format #t "~A~%" (red "emitted cmd attrib"))
                    (format outp "    ]),\n")

                    (if (not (member tool shell-tool-kws))
                        (begin
                          (format outp "    tools = [\n")
                          (for-each ;; fixme: don't iterate this twice
                           (lambda (cmd)
                             (format #t "Processing CMD: ~A~%" cmd)
                             (if (eq? :cmd (car cmd))
                                 (let-values
                                     (((tool-dep? tool args)
                                       (-derive-cmd pkg-path cmd deps outputs)))
                                   (format #t "  Tool-Dep?: ~A~%" tool-dep?)
                                   (format #t "  TooL: ~A~%" tool)
                                   (format #t "  ArgS: ~A~%" args)

                                   (if (not (null? tool-dep?))
                                       (begin
                                         (format outp "~{        \"~A\"~^,\n~}\n"
                                                 (list tool)) ;;FIXME: support multiple tools
                                         )))))
                           action)
                          (format outp "    ]\n")))
                    (format outp ")\n"))
                    )))
            ))

