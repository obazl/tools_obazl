(define (-derive-cmd action deps targets)
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
                   (let ((arg-pair (assoc-val arg deps)))
                     (format #f "~A:~A"
                             (car (assoc-val :pkg arg-pair))
                             (car (assoc-val :file arg-pair)))))

                  ((string? arg) ;; e.g. a file literal
                   arg
                   )

                  ((proper-list? arg) ;; ((:pkg :bin) (:tgt :mycmd))
                   "FIXME")

                 ;; (map (lambda (subarg)
                 ;;              (format #t "~A: ~A~%"
                 ;;                      (magenta "subarg") subarg)
                 ;;              (format #t ":: ~A/~A~%"
                 ;;                      (cadar subarg)
                 ;;                      (cadadr subarg))
                 ;;              (format #f "$(location ~A/~A)"
                 ;;                      (cadar subarg)
                 ;;                      (car (cdadr subarg))))
                 ;;      (cdr arg))
                  (else "FIXME2")))
               args))
         (args (if (assoc-val :stdout action)
                   (append args (list "> $@")) args))
         (_ (format #t "args: ~A~%" args))
         ;; (args (format #f "~A" args))
         )
    (values tool args)))

(define (-deps->srcs pkg-path deps)
  (format #t "~A: ~A\n" (blue "-deps->srcs") deps)
  ;; deps is a list of alists; key :maps to list of (:pkg :file) pairs
  ;; key :_ (anonymous) may map to multiple pairs
  ;; other keys are dune 'variables', each mapping to one (:pkg :file) pair
  (let* ((srcs (map (lambda (src-assoc)
                     (format #t "src-assoc: ~A~%" src-assoc)
                     (if (equal? :_ (car src-assoc))
                         (map (lambda (src-alist)
                                (format #t "src-alist: ~A~%" src-alist)
                                (let ((-pkg-path (car (assoc-val
                                                       :pkg src-alist))))
                                  (format #f "~A:~A"
                                          (if (equal pkg-path -pkg-path)
                                              ""
                                              -pkg-path)
                                          (car (assoc-val :file src-alist)))))
                              (cdr src-assoc))
                         ;; else
                         (format #f "~A:~A"
                                 (car (assoc-val :pkg (cdr src-assoc)))
                                 (car (assoc-val :file (cdr src-assoc))))))
                    deps))
         ;; srcs list may contain mix of strings and sublists
         (srcs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() srcs)))
    (format #t "SRCES: ~A\n" srcs)
    srcs))

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
         (outs (map (lambda (src)
                      (let ((-pkg-path (car (assoc-val :pkg (cadr src)))))
                      (format #f "~A~A"
                              (if (equal pkg-path -pkg-path)
                                  ""
                                  (string-append -pkg-path "/"))
                              (car (assoc-val :file (cadr src))))))
                    targets)))

    (let-values (((tool args)
                  (-derive-cmd action deps targets)))
      (format #t "tool: ~A~%" tool)
      (format #t "args: ~A~%" args)

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
            (format outp "    name  = \"foo\",\n")

            (format outp "    srcs  = [\n")
            (format outp "~{        \"~A\"~^,\n~}\n" srcs)
            (format outp "    ],\n")

            (format outp "    outs  = [\n")
            (format outp "~{        \"~A\"~^,\n~}\n" outs)
            (format outp "    ],\n")

            (format outp "    cmd   = \" \".join([\n")
            (format outp "        \"~A\",\n" tool)
            (format outp "~{        \"~A\"~^,\n~}\n" args)
            (format outp "    ]),\n")
            ;; tools only needed if tool is listed as a dep?
            (format outp "    tools = [\"\"],\n")
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

