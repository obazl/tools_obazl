
(define shell-tools
  '(cat cp)) ;; etc

(define (-deps-kw->cmd-args deps)
  (format #t "~A: ~A~%" (blue "-deps-kw->cmd-args") deps)
  (let ((args (map (lambda (dep)
                     (format #t "dep: ~A\n" dep)
                     (case (car dep)
                       ((::) ;; local files, :foo.txt
                        (map (lambda (d)
                               (format #f "$(location ~A)" d))
                             (cdr dep)))

                       ((:_) ;; //pkg:tgt files
                        (map (lambda (d)
                               (format #f "$(location //~A)" d))
                             (cdr dep)))

                       (else ;; custom tag
                        (map (lambda (d)
                               ;; FIXME: could contain either :: or :_ files
                               ;; e.g. (:css file_glob(*.css)) => (::css (:: "foo.css"...
                               (format #f "$(location //~A)" d))
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

(define (-derive-cmd pkg-path cmds deps targets)
  (format #t "~A: ~A~%" (blue "-derive-cmd") cmds)
  (format #t "targets: ~A~%" targets)
  (format #t "deps: ~A~%" deps)
  (format #t "stdout: ~A~%" (assoc-val :stdout cmds))

  ;; ((:cmd (:run (:tool (:pkg :bin) (:tgt ...
  ;; ((:cmd (:tool cat) (:args ...

  ;; tool: two forms, one for builtin cmds, one for run
  ;; builtin: (:cmd (:tool cat) ...)
 ;; run: (:cmd (:tool (:pkg :bin) (:tgt foo)) ...)


  (let* ((tool (assoc-in '(:cmd :tool) cmds))
         (_ (format #t "tool: ~A~%" tool))
         ;; (_ (format #t "tool: ~A~%" (cdr tool)))
         (_ (format #t "(alist? (cdr tool)) ~A~%" (alist? (cdr tool))))
         (tool (if (alist? (cdr tool))
                   (case (caadr tool)
                     ((::)
                      )
                     ((:_)
                      (let* ((tlist (cdr tool))
                             ;; (_ (format #t "tlist ~A~%" tlist))
                             (tlist (car tlist))
                             ;; (_ (format #t "tlist ~A~%" tlist))
                             (t (format #f "~A" (cadr tlist)))
                             ;; (_ (format #t "t ~A~%" t))
                             (dname (dirname t))
                             (bname (basename t)))
                        ;; (format #t "bname ~A~%" bname)
                        (format #f "//~A:~A" dname bname)))
                     (else
                      ))
                   (cadr tool)))
         ;; (tool (if (member tool shell-tools) ;;FIXME: find better method
         ;;           tool
         ;;           (format #f "~A" tool)))
         (_ (format #t "RESOLVED TOOL: ~A~%" tool))
         (tool-dep? (if (member tool shell-tools) ;;FIXME: find better method
                       '() #t))
         (_ (format #t "tool-dep?: ~A~%" tool-dep?))

        ;; FIXME: shell cmds v. targets that go in tools attr

         ;; FIXME: map dune builtins like 'copy' to shell cmds

         (args (cdr (assoc-in '(:cmd :args ) cmds)))
         (_ (format #t "args: ~A~%" args))
         (args
          (map (lambda (arg)
                 (format #t "Arg: ~A~%" arg)
                 (cond
                  ((symbol? arg) ;; dune 'vars', e.g. (::foo "x" "y"...)
                   (format #t "SYM: ~A\n" arg)
                   ;; find args in deplist
                   (if-let ((x (assoc arg deps)))
                           (map (lambda (a)
                                  (format #t "in: ~A\n" a)
                                  (let* ((fname (format #f "~A" a))
                                         (dname (dirname fname))
                                         (bname (basename fname)))
                                    (format #f "$(location ~A)"
                                            (if (equal dname pkg-path)
                                                bname fname))))
                                (cdr x))
                           (if (equal? arg ::deps)
                               (-deps-kw->cmd-args deps)
                               (error 'unmapped-var "unmapped var in cmd"))))

                  ((string? arg) ;; e.g. a file literal
                   (format #t "string\n")
                   ;; how do we know which strings need $(location)?
                   ;; assumption: files must be listed in deps, so any
                   ;; strings we see are just string args
                   (if-let ((x (-arg->dep arg deps)))
                             ;; (assoc arg deps)))
                           (begin
                                  (format #t "arg ~A in deps\n" x)
                                  (let* ((fname (format #f "~A" x))
                                         (dname (dirname fname))
                                         (bname (basename fname)))
                                    (format #f "$(location ~A)"
                                            (if (equal dname pkg-path)
                                                bname fname))))

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

                  (else (format #f "FIXME2_~A" arg))))
               args))
         ;; args may mix strings and lists of strings
         (args (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() args))
         (args (if (assoc-val :stdout cmds)
                   (append args (list "> $@")) args))
         (_ (format #t "args: ~A~%" args))
         ;; (args (format #f "~A" args))
         )
    (values tool-dep? tool args)))

(define (-targets->outs pkg-path targets)
  (format #t "~A: ~A\n" (blue "-targets->outs") targets)
  (let* ((outs (map (lambda (src)
                      (format #t "src: ~A\n" src)
                      (case (car src)

                        ((::) ;; pkg-local files
                         (map (lambda (srcfile)
                                (format #t "srcfile: ~A~%" srcfile)
                                (let ((dname (dirname srcfile))
                                      (bname (basename srcfile)))
                                  ;; (-pkg-path
                                  ;;      (car (assoc-val :pkg src-alist))))
                                  (format #f "~A"
                                          (if (equal dname pkg-path)
                                              bname srcfile))))
                              ;; (car
                              ;;  (assoc-val :file srcfile)))))
                              (cdr src)))

                        ((:_) ;; non-local, not-tagged
                         ;; e.g. (:_ "foo/bar/a.txt" "foo/bar/b.txt" ...)
                         (map (lambda (srcfile)
                                (format #t "srcfile: ~A~%" srcfile)
                                (let ((dname (dirname srcfile))
                                      (bname (basename srcfile)))
                                  ;; (-pkg-path
                                  ;;      (car (assoc-val :pkg src-alist))))
                                  (format #f "~A"
                                          (if (equal dname pkg-path)
                                              bname srcfile))))
                              ;; (car
                              ;;  (assoc-val :file srcfile)))))
                              (cdr src)))

                         ;; else tagged (:foo . "foo.txt")
                         (else
                          (format #f "~A"
                                  (let* ((srcfile (cdr src))
                                         (dname (dirname srcfile))
                                         (bname (basename srcfile)))
                                    (if (equal dname pkg-path)
                                        bname srcfile))))))
                    targets))
         (outs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() outs)))
    (format #t "OUTS: ~A\n" outs)
    outs))

(define (starlark-emit-skylib-target outp stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-skylib-target") stanza)
  (let* ((outfile (cadar (assoc-val :targets stanza)))
         (_ (format #t "outfile: ~A~%" outfile))

         (content (assoc-in '(:action :cmd :args :content) stanza))
         (_ (format #t "~A: ~A~%" (green "content") content)))

      (format outp "###########\n")
      (format outp "write_file(\n")
      (format outp "    name    = \"__~A__\",\n" outfile)
      (format outp "    out     = \"~A\"\n" outfile)
      (format outp "    content = [\"~A\"]\n" (cadr content))
      (format outp ")\n")))

;; FIXME: account for :ctx, which is derived from (chdir ...) in dune
;; the target should be written to the :ctx dir?
;; but then what if multiple rules chdir to same dir? e.g. %{workspace_root}
;; possible resolution: handle all :ctx rules in a separate pass?
;; for now just emit a comment
(define (starlark-emit-genrule outp pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-genrule") stanza)
  (let* ((action (assoc-val :action stanza))
         (_ (format #t "action: ~A~%" action))
         (deps (assoc-val :deps stanza))
         (_ (format #t "~A: ~A~%" (cyan "deps") deps))
         (srcs (-deps->srcs-attr pkg-path deps))
         (_ (format #t "~A: ~A~%" (cyan "srcs") srcs))

         ;; FIXME: derive from :args, :stdout, etc.
         ;; if %{targets} is in cmd string, ...
         ;; else if we have (:stdout ...), ...
         (targets (assoc-val :targets stanza))
         (_ (format #t "targets: ~A~%" targets))
         (outs (-targets->outs pkg-path targets))

         (name (format #f "__~A__"
                       (outs 0))))

    ;; progn: list of actions.
    (for-each
     (lambda (cmd)
       (format #t "ACTION: ~A~%" cmd))
     action)

    (if-let ((ctx (assoc-in '(:action :ctx) stanza)))
            (begin
              (format outp "## omitted:\n")
              (format outp "## (chdir ~A (run ~A ...))\n\n"
                    (cadr ctx)
                    (cadr (assoc-in '(:action :cmd :tool) stanza))))
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
                       (format #t "processing cmd: ~A~%" cmd)
                       (let-values
                           (((tool-dep? tool args)
                             (-derive-cmd pkg-path `(,cmd) deps targets)))
                         ;; (format #t "~A:\n" (red "derived cmd"))
                         (format #t "  tool-dep?: ~A~%" tool-dep?)
                         (format #t "  tool: ~A~%" tool)
                         (format #t "  args: ~A~%" args)

                         (if (not (null? tool-dep?))
                             (format outp "        \"$(location ~A)\",\n" tool)
                             (format outp "        \"~A\",\n" tool))
                         (format outp "~{        \"~A\"~^,\n~}\n" args)))
                     ;; if :stdout, add "> $@"
                     action)
                    (format outp "    ]),\n")

                    (format outp "    tools = [\n")
                    (for-each ;; fixme: don't iterate this twice
                     (lambda (cmd)
                       (format #t "processing cmd: ~A~%" cmd)
                       (let-values
                           (((tool-dep? tool args)
                             (-derive-cmd pkg-path `(,cmd) deps targets)))
                         (if (not (null? tool-dep?))
                             (begin
                               (format outp "~{        \"~A\"~^,\n~}\n"
                                       (list tool)) ;;FIXME: support multiple tools
                               ))))
                     action)
                    (format outp "    ]\n")))

                    (format outp ")\n")
                    )
            )))

(define (starlark-emit-rule-target outp pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-rule-target") stanza)
  (let* ((tool (assoc-in '(:action :cmd :tool) stanza))
         (_ (format #t "tool: ~A~%" tool)))
    (case (cadr tool)
      ((:skylib-write-file) (starlark-emit-skylib-target outp stanza))
      (else
       (starlark-emit-genrule outp pkg-path stanza)))))

(define (starlark-emit-rule-targets outp pkg) ;;fs-path stanzas)
  (format #t "~A: ~A~%" (blue "starlark-emit-rule-targets") pkg)

  ;; same code as starlark-emit-aggregate-targets, but we want to put
  ;; aggregates and rules in different locations.
  (let ((flag #t)
        (pkg-path (car (assoc-val :pkg-path pkg))))
    (for-each (lambda (stanza)
                (format #t "ACTION: ~A\n" (assoc :action (cdr stanza)))
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
                   (starlark-emit-rule-target outp pkg-path (cdr stanza)))
                  ;;FIXME the rest are obsolete
                  ((:run-cmd)
                   (starlark-emit-run-cmd-target outp fs-path (cdr stanza)))
                  ((:with-stdout-to)
                   (if (not (assoc-in '(:cmd :universe) (cdr stanza)))
                       (starlark-emit-with-stdout-to-target outp fs-path
                                                            (cdr stanza))
                       ;; else FIXME: deal with universe stuff
                       ))
                  ((:write-file)
                   (starlark-emit-write-file-target outp (cdr stanza)))
                  (else
                   ;; skip
                   )))
              (assoc-val :dune pkg))))

