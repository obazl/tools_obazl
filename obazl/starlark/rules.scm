(define (-derive-cmd pkg-path action deps targets)
  (format #t "~A: ~A~%" (blue "-derive-cmd") action)
  (format #t "targets: ~A~%" targets)
  (format #t "deps: ~A~%" deps)
  (format #t "stdout: ~A~%" (assoc-val :stdout action))

  ;; ((:cmd (:run (:tool (:pkg :bin) (:tgt ...
  ;; ((:cmd (:tool cat) (:args ...

  (let* ((tool (cadr (assoc-in '(:cmd :tool) action)))
         (_ (format #t "tool: ~A~%" tool))
        ;; FIXME: shell cmds v. targets that go in tools attr

         ;; FIXME: map dune builtins like 'copy' to shell cmds

         (args (cdr (assoc-in '(:cmd :args ) action)))
         (_ (format #t "args: ~A~%" args))
         (args
          (map (lambda (arg)
                 (format #t "Arg: ~A~%" arg)
                 (cond
                  ((symbol? arg) ;; dune 'vars', e.g. ::foo, ::<
                   (format #t "SYM\n")

                   ;; FIXME: lookup sym in deps list

                   (let* ((fname (format #f "~A" arg))
                          (dname (dirname fname))
                          (bname (basename fname)))
                     (format #f "$(location ~A)"
                             (if (equal dname pkg-path)
                                 bname fname))))

                  ((string? arg) ;; e.g. a file literal
                   (format #t "string\n")
                   ;; how do we know which strings need $(location)?
                   (let* ((dname (dirname arg))
                          (bname (basename arg)))
                     (format #f "$(location ~A)"
                             (if (equal dname pkg-path)
                                 bname arg))))

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
         (args (if (assoc-val :stdout action)
                   (append args (list "> $@")) args))
         (_ (format #t "args: ~A~%" args))
         ;; (args (format #f "~A" args))
         )
    (values tool args)))

(define (-targets->outs pkg-path targets)
  (format #t "~A: ~A\n" (blue "-targets->outs") targets)
  (let* ((outs (map (lambda (src)
                      (format #t "src: ~A\n" src)
                      (if (equal? :_ (car src))
                          ;; (:_ "foo.txt" "bar.txt" ...)
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
                               (cdr src))
                          ;; else tagged (:foo . "foo.txt")
                          (format #f "~A"
                                  (let* ((srcfile (cdr src))
                                         (dname (dirname srcfile))
                                         (bname (basename srcfile)))
                                    (if (equal dname pkg-path)
                                        bname srcfile)))))
                    targets))
         (outs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() outs)))
    (format #t "OUTS: ~A\n" outs)
    outs))

(define (tool-dep? t)
  ;; is tool t something that needs to go in tools = []?
  #f)

(define (starlark-emit-rule-target outp pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-rule-target") stanza)
  (let* ((action (assoc-val :action stanza))
         (_ (format #t "action: ~A~%" action))
         (deps (assoc-val :deps stanza))
         (_ (format #t "~A: ~A~%" (cyan "deps") deps))
         (srcs (-deps->srcs pkg-path deps))
         (_ (format #t "~A: ~A~%" (cyan "srcs") srcs))

         ;; FIXME: derive from :args, :stdout, etc.
         ;; if %{targets} is in cmd string, ...
         ;; else if we have (:stdout ...), ...
         (targets (assoc-val :targets stanza))
         (_ (format #t "targets: ~A~%" targets))
         (outs (-targets->outs pkg-path targets))

         (name (format #f "__~A__"
                       (outs 0))))

    (let-values (((tool args)
                  (-derive-cmd pkg-path action deps targets)))
      (format #t "tool: ~A~%" tool)
      (format #t "args: ~A~%" args)
      (format #t "outs: ~A~%" outs)

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
            (format outp "        \"~A\",\n" tool)
            (format outp "~{        \"~A\"~^,\n~}\n" args)
            (format outp "    ]),\n")

            (if (tool-dep? tool)
                (format outp "    tools = [\"\"],\n"))

            (format outp ")\n")
            )))))

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

