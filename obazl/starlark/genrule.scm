(define (-find-match-in-stanza key pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-find-match-in-stanza") key)
  (let* ((key (if (keyword? key) key (string->keyword key)))
         (deps (assoc-val :deps stanza))
         (found (find-if (lambda (dep)
                           (format #t "~A: ~A~%" (yellow "checking dep") dep)
                           (eq? key (car dep))) deps)))
    (format #t "~A: ~A~%" (red "found") found)
    (if found
        (let* ((lbl (cdr found))
               (_ (format #t "~A: ~A~%" (yellow "lbl") lbl))
               (pkg (assoc-val :pkg lbl))
               (tgt (if-let ((t (assoc-val :tgt lbl)))
                            (format #f "$(location ~A)" (cdr t))
                            (if-let ((t (assoc-val :tgts lbl)))
                                    (format #f "$(locations ~A)" t)
                                    (error 'fixme
                                           (format #f "no tgt/tgts in dep: ~A" found)))))
               )
          tgt)
        (let ((outputs (assoc-val :outputs stanza))
              (found (find-if (lambda (out)
                                (format #t "~A: ~A~%" (yellow "checking out") out)
                                (eq? key (car out))) outputs)))
          (if found
              "bar"
              (error 'fixme "TODO: search exports"))))))

(define (-resolve-match match pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-resolve-match") match)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
  (format #t "~A: ~A~%" (blue "stanza") stanza)
  (let* ((key (string->keyword match)))
    (case key
      ((:deps)
       (let* ((dassoc (assoc key stanza))
              (replace (map (lambda (dep)
                              (format #t "~A: ~A~%" (yellow "dep") dep)
                              ;;FIXME: support :fg
                              ;;FIXME: match paths
                              (if-let ((t (assoc :tgt (cdr dep))))
                                      (format #f "$(location :~A)" (cdr t))
                                      (if-let ((t (assoc :tgts (cdr dep))))
                                              (format #f "$(locations :~A)" (cdr t))
                                              (error 'fixme
                                                     (format #f "missing tgt/tgts: ~A" dep)))))
                            (cdr dassoc))))
         (format #t "~A: ~A~%" (red "replace") replace)
         (string-join replace)))
      (else
       (-find-match-in-stanza key pkg-path stanza)))))

(define (-expand-bash-tool tool pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-expand-bash-tool") tool)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
  (format #t "~A: ~A~%" (blue "stanza") stanza)

  (if (member (if (string? tool) (string->symbol tool) tool)
              shell-tools)
      #f
      (let* ((key (string->keyword tool)))
        ;; search :deps
        (let* ((deps (assoc-val :deps stanza))
               (_ (format #t "~A: ~A~%" (yellow "deps") deps))
               (match (find-if (lambda (dep)
                                 (format #t "~A: ~A~%" (yellow "dep") dep)
                                 (eq? key (car dep)))
                               deps)))
          (format #t "~A: ~A~%" (yellow "match") match)
          (if match
              (let* ((lbl (cdr match))
                     (pkg (assoc-val :pkg lbl))
                     (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                     (pkg (if (equal? pkg-path pkg) "" pkg))
                     (tgt (if-let ((t (assoc-val :tgt lbl)))
                                  (format #f "~A:~A" pkg t)
                                  (error 'fixme "bash tool has :tgts"))))
                (format #t "~A: ~A~%" (yellow "RESOLVED") tgt)
                tgt)
              arg)))))

(define (-expand-bash-arg arg pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-expand-bash-arg") arg)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
  (format #t "~A: ~A~%" (blue "stanza") stanza)
  (let* ((key (string->keyword arg)))
    ;; search :deps
    (let* ((deps (assoc-val :deps stanza))
           (_ (format #t "~A: ~A~%" (yellow "deps") deps))
           (match (find-if (lambda (dep)
                           (format #t "~A: ~A~%" (yellow "dep") dep)
                           (eq? key (car dep)))
                         deps)))
      (format #t "~A: ~A~%" (yellow "match") match)
      (if match
          (let* ((lbl (cdr match))
                 (pkg (assoc-val :pkg lbl))
                 (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                 (pkg (if (equal? pkg-path pkg) "" pkg))
                 (tgt (if-let ((t (assoc-val :tgt lbl)))
                              (format #f "$(location ~A:~A)" pkg t)
                              (if-let ((t (assoc-val :tgts lbl)))
                                      (format #f "$(locations ~A:~A)" pkg t)
                                      (error 'fixme "lbl missing tgt/tgs")))))
            (format #t "~A: ~A~%" (yellow "RESOLVED") tgt)
            tgt)
          arg))))

;; handle all pct-vars: %{deps}, %{target}, etc.
;; also filename literals
(define (-expand-bash-args args pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-expand-bash-args") args)
  (let* ((pct-regex "%{([^}]+)}")
         (rgx (regex.make))
         (rx (regcomp rgx pct-regex REG_EXTENDED)))
    (let* ((arg-list (string-tokenize args char-set:graphic)))
      (format #t "~A: ~A~%" (red "arg-list") arg-list)

      ;; FIXME: treat first arg as tool

      (let* ((expanded-tool
             (-expand-bash-tool (car arg-list) pkg-path stanza))
            (_ (format #t "~A: ~A~%" (yellow "expanded TOOL") expanded-tool))
            (expanded-args
             (map (lambda (arg)
                    (format #t "~A: ~A~%" (red "ARG") arg)
                    (let* ((match-ct 2)
                           (res (regexec rgx arg match-ct 0))
                           (_ (format #t "~A: ~A~%"
                                      (red "regex res") res)))
                      (if (int-vector? res)
                          (let* ((rm-so (int-vector-ref res 0))
                                 (rm-eo (int-vector-ref res 1))
                                 (sub-so (int-vector-ref res 2))
                                 (sub-eo (int-vector-ref res 3))
                                 (pfx  (substring arg 0 rm-so))
                                 (sfx  (substring arg rm-eo))
                                 (match (substring arg sub-so sub-eo))
                                 (replacement (-resolve-match match pkg-path stanza)))
                            ;; (format #t "~A: ~A~%" (red "sub_so") sub-so)
                            ;; (format #t "~A: ~A~%" (red "sub_eo") sub-eo)
                            ;; (format #t "~A: ~A~%" (red "rm_so") rm-so)
                            ;; (format #t "~A: ~A~%" (red "rm_eo") rm-eo)
                            ;; (format #t "~A: ~A~%" (red "match") match)
                            ;; (format #t "~A: ~A~%" (red "pfx") pfx)
                            ;; (format #t "~A: ~A~%" (red "sfx") sfx)
                            (format #f "~A~A~A" pfx replacement sfx))
                          (-expand-bash-arg arg pkg-path stanza))))
                  arg-list)))
        (regfree rgx)
        (let* ((expanded-args
                (if-let ((stdout (assoc-in '(:actions :stdout) stanza)))
                        (begin
                          (format #t "~A: ~A~%" (yellow "stdout") stdout)
                          (append expanded-args (list "> $@")))
                        expanded-args)))
          (format #t "~A: ~A~%" (red "EXPANDED ARGS") expanded-args)
          (values expanded-tool expanded-args))))))

(define (-emit-bash-cmd outp pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-emit-bash-cmd") stanza)
  (let* ((args (assoc-in '(:actions :cmd :args) stanza))
         (args (cadr args))
         (_ (format #t "~A: ~A~%" (yellow "bash args") args))
         )
    (let-values (((tool parsed-args)
                  (-expand-bash-args args pkg-path stanza)))
      (format #t "~A: ~A~%" (yellow "tool") tool)
      (format #t "~A: ~S~%" (yellow "parsed args") parsed-args)

      (format outp "    cmd_bash   = \" \".join([\n")
      (format outp "~{        ~S~^,~%~}~%" parsed-args)
      ;; (if stdout
      ;;     (format outp "        \"> $@\",\n"))
      (format outp "    ]),\n")
      (if tool
          (format outp "    tools   = [~S]\n" tool))
      )))

(define (-emit-bash-srcs outp srcs pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-emit-bash-srcs") srcs)
  ;; remove bash tool from srcs

  (let* ((args (assoc-in '(:actions :cmd :args) stanza))
         (args (cadr args)))
    (let-values (((tool parsed-args)
                  (-expand-bash-args args pkg-path stanza)))
      (format #t "~A: ~A~%" (yellow "TOOL") tool)
      (let ((srcs (remove tool srcs)))

        (format #t "~A: ~A~%" (red "srcs") srcs)
        (format outp "    srcs  = [\n")
        (format outp "~{        \"~A\"~^,\n~}\n" srcs)
        (format outp "    ],\n")))))

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
         (bash-cmd? (eq? tool 'bash))
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
                        (if bash-cmd?
                            (-emit-bash-srcs outp srcs pkg-path stanza)
                            (begin
                              (format #t "~A: ~A~%" (red "SrcS") srcs)
                              (format outp "    srcs  = [\n")
                              (format outp "~{        \"~A\"~^,\n~}\n" srcs)
                              (format outp "    ],\n"))))

                    (format outp "    outs  = [\n")
                    (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    ;; (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    (format outp "    ],\n")

                    (if bash-cmd?
                        (-emit-bash-cmd outp pkg-path stanza)
                        (begin
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
                          (format outp "    ]),\n")))

                    (if (and (not (member tool shell-tool-kws))
                             (not bash-cmd?))
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

