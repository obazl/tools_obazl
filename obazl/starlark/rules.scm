
(define shell-tools
  '(cat cp copy)) ;; etc

(define (-deps-kw->cmd-args deps)
  (format #t "~A: ~A~%" (blue "-deps-kw->cmd-args") deps)
  (let ((args (map (lambda (dep)
                     (format #t "dep: ~A\n" dep)
                     (case (car dep)
                       ((::) ;; local files, :foo.txt
                        (map (lambda (d)
                               (format #f "$(Location ~A)" d))
                             (cdr dep)))

                       ((:_) ;; //pkg:tgt files
                        (map (lambda (d)
                               (format #f "$(Location //~A)" d))
                             (cdr dep)))

                       (else ;; custom tag
                        (map (lambda (d)
                               ;; FIXME: could contain either :: or :_ files
                               ;; e.g. (:css file_glob(*.css)) => (::css (:: "foo.css"...
                               (format #f "$(Location //~A)" d))
                             (cdr dep)))))
                   deps)))
    (apply append args)))

(define (-arg->dep arg deps)
  (format #t "~A: ~A in ~A~%" (blue "-arg->dep") arg deps)
  ;; e.g. deps:
  ;; ((:_ "rules/copy/config/config.mlh" "rules/copy/foo/bar.x") (:: "baz.y"))
  (let* ((dlist (find-if (lambda (deplist)
                           (format #t "deplist: ~A~%" deplist)
                           (case (car deplist)
                             ((::)
                              (find-if (lambda (d) (string-suffix? arg d))
                                       (cdr deplist)))

                             ((:_)
                              (find-if (lambda (d) (string-suffix? arg d))
                                       (cdr deplist)))
                             (else
                              (find-if (lambda (d) (string-suffix? arg d))
                                       (cdr deplist)))
                             ))
                         deps))
         (dep (if dlist
                  (begin
                    (format #t "found dlist: ~A~%" dlist)
                    (find-if (lambda (d)
                               (format #t "finding?: ~A~%" d)
                               (string-suffix? arg d))
                             (cdr dlist))))))
    (format #t "dep: ~A~%" dep)
    dep))

(define (-resolve-args cmd pkg-path deps outputs)
  (format #t "~A: ~A~%" (blue "-resolve-args") cmd)
  (format #t "~A: ~A~%" (yellow "deps") deps)
  (format #t "~A: ~A~%" (yellow "outputs") outputs)
  (let* ((-args (assoc-val :args (cdr cmd)))
         (_ (format #t "args: ~A~%" -args))
         (args
          (map (lambda (arg)
                 (format #t "Arg: ~A~%" arg)
                 (cond

                  ((equal? arg ::targets) ;; or :targets ??
                   (format #t "~A: ~A~%" (red "Arg is ::targets") arg)
                   (let ((outs (-outputs->outs-attr pkg-path outputs)))
                     (map (lambda (out)
                            (format #f "$(location ~A)" out))
                          outs)))

                  ((equal? arg ::deps)
                   (format #t "~A: ~A~%" (red "Arg is ::deps") arg)
                   (error 'fixme "unhandled %{deps} arg"))

                  ((keyword? arg)
                   (format #t "KW: ~A\n" arg)
                   ;; find args in deplist
                   (let ((found
                          (find-if (lambda (dep)
                                     (format #t "dep: ~A\n" dep)
                                     (equal? arg (car dep)))
                                   deps)))
                     (if found
                         (begin
                           (format #t "~A: ~A~%" (red "FOUND") found)
                           (let* ((label (cdr found))
                                  (pkg (assoc-val :pkg label))
                                  (tgt-tag (caadr label))
                                  (_ (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag))
                                  (tgt (assoc-val tgt-tag label)))
                             (if (equal? pkg pkg-path)
                                 (if (eq? tgt-tag :tgts)
                                     (format #f "$(locations ~A)" tgt)
                                     (format #f "$(location ~A)" tgt))
                                 (if (eq? tgt-tag :tgts)
                                     (format #f "$(locations //~A~A)"
                                              pkg tgt)
                                     (format #f "$(location //~A~A)"
                                             pkg tgt)))))
                         (error 'fixme "kw arg unresolved"))))

                  ((string? arg) ;; e.g. a file literal
                   (format #t "arg: string literal\n")
                   ;; how do we know which strings need $(location)?
                   ;; assumption: files must be listed in deps, so any
                   ;; strings we see are just string args
                   (if-let ((x (-arg->dep arg deps)))
                           ;; (assoc arg deps)))
                           (begin
                             (format #t "found arg ~A in deps\n" x)
                             (let* ((fname (format #f "~A" x))
                                    (dname (dirname fname))
                                    (bname (basename fname)))
                               (let ((tmp (format #f "$(location ~A)"
                                                  (if (equal dname pkg-path)
                                                      bname fname))))
                                 tmp)))

                           (begin
                             (format #t "arg not in deps: ~A~%" arg)
                             arg)))

                  ((proper-list? arg) ;; (:_ a.x b.y...)
                   ;; e.g. from %{deps} or a custom var
                   (map (lambda (a)
                          (let* ((dirname (dirname a))
                                 (basename (basename a)))
                            (format #f "$(location ~A)"
                                    (if (equal dirname pkg-path)
                                        basename a))))
                        (cdr arg)))

                  (else
                   (error 'fixme "unhandled arg"))
                        ))
               -args)))
    (format #t "~A: ~A~%" (red "Args") args)
    args))

(define (-derive-cmd pkg-path cmd deps targets)
  (format #t "~A: ~A~%" (blue "-derive-cmd") cmd)
  (format #t "targets: ~A~%" targets)
  (format #t "deps: ~A~%" deps)
  ;; (format #t "stdout: ~A~%" (assoc-val :stdout cmd))

  ;; ((:cmd (:run (:tool (:pkg :bin) (:tgt ...
  ;; ((:cmd (:tool cat) (:args ...

  ;; tool: two forms, one for builtin cmd, one for run
  ;; builtin: (:cmd (:tool cat) ...)
 ;; run: (:cmd (:tool (:pkg :bin) (:tgt foo)) ...)

  (let* ((cmd-alist (cdr cmd))
         (tool (car (assoc-val :tool cmd-alist)))
         (_ (format #t "tool: ~A~%" tool))
         ;; (_ (format #t "tool: ~A~%" (cdr tool)))
         ;; (_ (format #t "(alist? (cdr tool)) ~A~%" (alist? (cdr tool))))
         (tool (if (keyword? tool)
                   (let* ((tool-tlbl (assoc tool deps))
                          (tool-label (cdr tool-tlbl))
                          (tool-pkg (assoc-val :pkg tool-label))
                          (tool-tag (caadr tool-label))
                          (_ (format #t "~A: ~A~%" (cyan "tool-tag") tool-tag))
                          (tool-tgt (case tool-tag
                                      ((:tgt)
                                       (assoc-val :tgt tool-label))
                                      ((:tgts)
                                       (assoc-val :tgts tool-label))
                                      ((:fg)
                                       (format #f "*~A*"
                                       (assoc-val :fg tool-label)))
                                      (else
                                       (error 'fixme (format #f "~A: ~A~%" (red "unrecognized tool tag") tool-tag))))))
                     (format #t "~A: ~A~%" (red "tool-label") tool-label)
                     (format #t "~A: ~A~%" (red "tool-pkg") tool-pkg)
                     (format #t "~A: ~A~%" (red "tool-tgt") tool-tgt)
                     (if (equal? tool-pkg pkg-path)
                         (format #f "~A" tool-tgt)
                         (format #f "//~A:~A" tool-pkg tool-tgt)))
                   (error 'fixme "tool not kw")))
         (_ (format #t "RESOLVED TOOL: ~A~%" tool))
         (tool-dep? (if (member tool shell-tools) ;;FIXME: find better method
                       '() #t))
         (_ (format #t "tool-dep?: ~A~%" tool-dep?))

        ;; FIXME: shell cmds v. targets that go in tools attr

         ;; FIXME: map dune builtins like 'copy' to shell cmds

         (args (-resolve-args cmd pkg-path deps targets))
         (_ (format #t "~A: ~A~%" (red "RESOLVED ARGS") args))
         ;; args may mix strings and lists of strings
         (args (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() args))
         (args (if (assoc-val :stdout (cdr cmd))
                   (append args (list "> $@")) args))
         (_ (format #t "args: ~A~%" args))
         ;; (args (format #f "~A" args))
         )
    (format #t "TOol-dep? ~A~%" tool-dep?)
    (format #t "TOol ~A~%" tool)
    (format #t "ARgs ~A~%" args)
    (values tool-dep? tool args)))

(define (-outputs->outs-attr pkg-path outputs)
  (format #t "~A: ~A\n" (blue "-outputs->outs-attr") outputs)
  (let* ((outs (map (lambda (out-tlbl)
                      (format #t "out-tlbl: ~A\n" out-tlbl)
                      (let* ((tag (car out-tlbl))
                             (_ (format #t "~A: ~A~%" (yellow "tag") tag))
                             (label (cdr out-tlbl))
                             (_ (format #t "~A: ~A~%" (yellow "label") label))
                             (pkg (assoc-val :pkg label))
                             (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                             (tgt (assoc-val :tgt label))
                             (_ (format #t "~A: ~A~%" (yellow "tgt") tgt)))
                        (if (equal? pkg pkg-path)
                            tgt
                            ;; should not happen?
                            (format #f "~A:~A" pkg tgt))))
                        outputs))
         (outs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() outs)))
    (format #t "OUTS: ~A\n" outs)
    outs))

;; FIXME: account for :ctx, which is derived from (chdir ...) in dune
;; the target should be written to the :ctx dir?
;; but then what if multiple rules chdir to same dir? e.g. %{workspace_root}
;; possible resolution: handle all :ctx rules in a separate pass?
;; for now just emit a comment
(define (starlark-emit-genrule outp pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-genrule") stanza)
  (let* ((action (assoc-val :actions stanza))
         (_ (format #t "action: ~A~%" action))
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

    ;; progn: list of actions.
    (for-each
     (lambda (cmd)
       (format #t "GENRULE ACTION: ~A~%" cmd))
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

                    (format #t "~A: ~A~%" (red "SrcS") srcs)
                    (format outp "    srcs  = [\n")
                    (format outp "~{        \"~A\"~^,\n~}\n" srcs)
                    (format outp "    ],\n")

                    (format outp "    outs  = [\n")
                    (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    ;; (format outp "~{        \"~A\"~^,\n~}\n" outs)
                    (format outp "    ],\n")

                    (format outp "    cmd   = \" \".join([\n")
                    (for-each
                     (lambda (cmd)
                       (format #t "PROCESSING cmd: ~A~%" cmd)
                       (if (eq? :cmd (car cmd))
                           (let-values
                               (((tool-dep? tool xargs)
                                 (-derive-cmd pkg-path cmd deps outputs)))
                             ;; (format #t "~A:\n" (red "derived cmd"))
                             (format #t "  tool-dep?: ~A~%" tool-dep?)
                             (format #t "  tool: ~A~%" tool)
                             (format #t "~A: ~A~%" (red "ARGSARGS") xargs)

                             (if (not (null? tool-dep?))
                                 ;; HACK: $(location ...)
                                 (format outp "        \"$(location ~A)\",\n" tool)
                                 (format outp "        \"~A\",\n" tool))
                             (format outp "~{        \"~A\"~^,\n~}\n" xargs))
                           ;; else
                           (error 'missing-cmd "command w/o :cmd")
                           ))
                     ;; if :stdout, add "> $@"
                     action)
                    (format outp "    ]),\n")

                    (format outp "    tools = [\n")
                    (for-each ;; fixme: don't iterate this twice
                     (lambda (cmd)
                       (format #t "Processing Cmd: ~A~%" cmd)
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
                               ))))
                     action)
                    (format outp "    ]\n")
                    (format outp ")\n"))
                    )))
            ))

(define (starlark-emit-skylib-write-file outp cmd pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-skylib-write-file") cmd)
  (let* ((args (assoc :args (cdr cmd)))
         (_ (format #t "~A: ~A~%" (yellow "args") args))
         (out-tag (cadr args))
         (_ (format #t "~A: ~A~%" (yellow "out-tag") out-tag))
         (outputs (assoc :outputs (cdr stanza)))
         (_ (format #t "~A: ~A~%" (yellow "outputs") outputs))
         (tfql (assoc out-tag (cdr outputs)))
         (_ (format #t "~A: ~A~%" (yellow "tfql") tfql))
         (out-pkg (assoc-val :pkg (cdr tfql)))
         (_ (format #t "~A: ~A~%" (yellow "out-pkg") out-pkg))
         (out-tgt (assoc-val :tgt (cdr tfql)))
         (_ (format #t "~A: ~A~%" (yellow "out-tgt") out-tgt))

         (outfile (if (equal? out-pkg pkg-path)
                      out-tgt (format #f "~A/~A" out-pkg out-tgt)))
         (_ (format #t "~A: ~A~%" (yellow "outfile") outfile))

         (content (last (last args))) ;; (assoc '(:content) (cddr args)))
         (_ (format #t "~A: ~A~%" (green "content") content)))

      (format outp "###########\n")
      (format outp "write_file(\n")
      (format outp "    out     = \"~A\",\n" outfile)
      (format outp "    content = [\"~A\"],\n" content)
      (format outp "    name    = \"__~A__\"\n" outfile)
      (format outp ")\n")))


(define (starlark-emit-write-file-target outp stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-write-file-target") stanza)
  (let* ((output (assoc-val :outputs (cdr stanza)))
        (_ (format #t "~A: ~A~%" (yellow "output") output))
        (pkg-tgt (cdar output))
        (_ (format #t "~A: ~A~%" (yellow "pkg-tgt") pkg-tgt))
        (pkg (assoc-val :pkg pkg-tgt))
        (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
        (tgt (assoc-val :tgt pkg-tgt))
        (_ (format #t "~A: ~A~%" (yellow "tgt") tgt))
        (out  (format #f "~A" tgt))
        (name (format #f "__~A__" tgt))
        (args (assoc-in '(:actions :cmd :args) (cdr stanza)))
        (_ (format #t "~A: ~A~%" (yellow "args") args))
        (content (cadr (find-if (lambda (arg)
                            ;; (format #t "~A: ~A~%" (yellow "arg") arg)
                            (if (list? arg)
                                (equal? (car arg) :content)
                               #f))
                          (cdr args)))))

    (format outp "###########\n")
    (format outp "write_file(\n")
    (format outp "    out      = \"~A\",\n" out)
    (format outp "    content  = [\"\"\"")
    (format outp "~A" content)
    (format outp "\"\"\"],~%")
    (format outp "    name     = \"~A\"\n" name)
    (format outp ")")
    (newline outp)
    (newline outp)

    ;; debugging
    ;; (if (list? stanza)
    ;;     (begin
    ;;       (format outp "## (\n")
    ;;       (for-each (lambda (sexp)
    ;;                   (format outp "##   ~A\n" sexp))
    ;;                 stanza)
    ;;       (format outp "## )\n")))
    ))

(define (starlark-emit-rule-target outp pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-rule-target") stanza)

  ;; FIXM: :actions may contain multiple :cmd (progn); iterate
  (for-each (lambda (cmd)
              (format #t "rule cmd: ~A~%" cmd)
              (let* ((tool (assoc-val :tool (cdr cmd)))
                     (_ (format #t "tool: ~A~%" tool)))
                (case (car tool)
                  ((:write-file)
                   (starlark-emit-write-file-target outp stanza))
                  (else
                   (starlark-emit-genrule outp pkg-path stanza)))))
            (assoc-val :actions stanza)))


(define (starlark-emit-rule-targets outp pkg) ;;fs-path stanzas)
  (format #t "~A: ~A~%" (blue "starlark-emit-rule-targets") pkg)

  ;; same code as starlark-emit-aggregate-targets, but we want to put
  ;; aggregates and rules in different locations.
  (let ((flag #t)
        (pkg-path (car (assoc-val :pkg-path pkg))))
    (for-each (lambda (stanza)
                (format #t "stanza tag ~A\n" (car stanza))
                (format #t "RULE ACTION: ~A\n" (assoc :actions (cdr stanza)))
                (case (car stanza)
                  ((:rule :genrule :with-stdout-to :write-file)
                   (if flag
                       (begin
                         (format outp "########################\n")
                         (format outp "####  Rule Targets  ####\n")
                         (newline outp)
                         (set! flag #f)))))

                (case (car stanza)
                  ((:rule)
                   (let* ((actions (assoc :actions (cdr stanza)))
                          (cmd-list (assoc-in* '(:actions :cmd) (cdr stanza)))
                          (_ (format #t "~A: ~A~%" (green "cmd-list") cmd-list))
                          (cmd-ct (length cmd-list)))
                     (if (> cmd-ct 1)
                         (for-each (lambda (cmd)
                                     (format #t "~A: ~A~%" (red "cmd") cmd)
                                     (let ((tool (car (assoc-val :tool (cdr cmd)))))
                                       (case tool
                                         ((:write-file)
                                          (starlark-emit-skylib-write-file outp cmd pkg-path stanza))
                                         (else
                                          (starlark-emit-rule-target outp pkg-path (cdr stanza))))))
                                   cmd-list)
                         (starlark-emit-rule-target outp pkg-path (cdr stanza)))))

                  ((:write-file)
                   (starlark-emit-write-file-target outp stanza))
                  ;;FIXME the rest are obsolete
                  ((:with-stdout-to)
                   (if (not (assoc-in '(:cmd :universe) (cdr stanza)))
                       (starlark-emit-with-stdout-to-target outp fs-path
                                                            (cdr stanza))
                       ;; else FIXME: deal with universe stuff
                       ))
                  (else
                   ;; skip
                   )))
              (assoc-val :dune pkg))))

