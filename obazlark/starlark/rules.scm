;; rules.scm

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
                             (cdr dlist)))
                  #f)))
    (format #t "dep: ~A~%" dep)
    dep))

(define (-resolve-args cmd pkg-path deps outputs)
  (format #t "~A: ~A~%" (blue "-resolve-args") cmd)
  (format #t "~A: ~A~%" (yellow "deps") deps)
  (format #t "~A: ~A~%" (yellow "outputs") outputs)
  (let* ((-args (assoc-val :args (cdr cmd)))
         (_ (format #t "-args: ~A~%" -args))
         (args
          (map (lambda (arg)
                 (format #t "Mapping arg: ~A~%" arg)
                 (cond
                  ((or (equal? arg :targets)
                       (equal? arg :outputs))
                   (format #t "~A: ~A~%" (red "Arg is") arg)
                   (let ((outs (outputs->outs-attr pkg-path outputs)))
                     (map (lambda (out)
                            (format #f "$(location ~A)" out))
                          outs)))

                  ((equal? arg ::deps)
                   (format #t "~A: ~A~%" (red "Arg is ::deps") arg)
                   ;;(error 'fixme "unhandled %{deps} arg")
                     (map (lambda (dep)
                            (format #t "~A: ~A~%" (yellow "dep") dep)
                            (let* ((label (cdr dep))
                                   (pkg (assoc-val :pkg label))
                                   (tgt-tag (caadr label))
                                   (_ (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag))
                                   (tgt (assoc-val tgt-tag label)))
                              (if (equal? pkg pkg-path)
                                  (if (eq? tgt-tag :glob) ;; :tgts)
                                      (format #f "$(locations :~A)" tgt)
                                      (format #f "$(location :~A)" tgt))
                                  (if (eq? tgt-tag :glob) ;; :tgts)
                                      (format #f "$(locations //~A:~A)"
                                              pkg tgt)
                                      (format #f "$(location //~A:~A)"
                                              pkg tgt)))))
                          (dissoc '(::tools) deps)))

                  ;; ((member arg ocaml-std-pkgs)
                  ;;  (error 'X "ocaml-std-pkgs"))

                  ((keyword? arg)
                   (format #t "KW: ~A\n" arg)
                   (if (equal? ::pkg-dir arg)
                       "./"
                       ;; find args in deplist
                       (let ((found
                              (find-if (lambda (dep)
                                         (format #t "dep: ~A\n" dep)
                                         (equal? arg (car dep)))
                                       deps)))
                         (if found
                             (begin
                               (format #t "~A: ~A~%" (red "FOUND arg") found)
                               (if (equal? (cdr found) ::unresolved)
                                   (symbol->string (keyword->symbol (car found)))
                                   (let* ((label (cdr found))
                                          (ws (if-let ((ws (assoc-val :ws label)))
                                                      ws ""))
                                          (pkg (let ((p (format #f "~A" (assoc-val :pkg label))))
                                                 (if (string=? p "./") "." p)))
                                          (_ (format #t "~A: ~A~%" (bgred "pkg") pkg))
                                          ;; tgt tag may be :tgt, :tgts, :glob, or :fg
                                          (_ (format #t "~A: ~A~%" (bgyellow "getting tgt tag") label))
                                          (tgt-pair (dissoc '(:ws) label))
                                          (_ (format #t "~A: ~A~%" (yellow "tgt-pair") tgt-pair))
                                          (tgt-pair (car (dissoc '(:pkg) tgt-pair)))
                                          (_ (format #t "~A: ~A~%" (yellow "tgt-pair") tgt-pair))
                                          (tgt-tag (car tgt-pair))
                                          (_ (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag))
                                          (tgt (cdr tgt-pair)))
                                     (if (equal? pkg pkg-path)
                                         (if (eq? tgt-tag :glob) ;; :tgts)
                                             (format #f "`realpath $(locations :~A)`" tgt)
                                             (format #f "`realpath $(location :~A)`" tgt))
                                         (if (eq? tgt-tag :glob) ;; :tgts)
                                             (format #f "`realpath $(locations ~A//~A:~A)`"
                                                     ws pkg tgt)
                                             (format #f "`realpath $(location ~A//~A:~A)`"
                                                     ws pkg tgt))))))
                             ;; else not found in :deps, try :outputs?

                             (error 'fixme
                                    (format #f "~A: ~A~%"
                                            (red "kw arg unresolved") arg))))))

                  ((string? arg) ;; e.g. a file literal
                   (format #t "arg: string literal\n")
                   (if (char=? #\- (arg 0))
                       arg
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
                                 arg))))

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

(define (derive-cmd pkg-path cmd deps targets)
  (format #t "~A: ~A~%" (ublue "derive-cmd") cmd)
  (format #t "~A: ~A~%" (blue "targets") targets)
  (format #t "~A: ~A~%" (blue "deps") deps)
  ;; (format #t "stdout: ~A~%" (assoc-val :stdout cmd))

  ;; ((:cmd (:run (:tool (:pkg :bin) (:tgt ...
  ;; ((:cmd (:tool cat) (:args ...

  ;; tool: two forms, one for builtin cmd, one for run
  ;; builtin: (:cmd (:tool cat) ...)
 ;; run: (:cmd (:tool (:pkg :bin) (:tgt foo)) ...)

  (let* ((cmd-alist (cdr cmd))
         (tool (car (assoc-val :tool cmd-alist)))
         (_ (format #t "~A: ~A~%" (blue "tool") tool))
         ;; (_ (format #t "tool: ~A~%" (cdr tool)))
         ;; (_ (format #t "(alist? (cdr tool)) ~A~%" (alist? (cdr tool))))
         (tool (-tool-for-genrule pkg-path tool deps))
         (_ (format #t "~A: ~A~%" (blue "RESOLVED TOOL for genrule") tool))
         ;; (_ (error 'X "STOP derive-cmd"))

         (tool-dep? (not (member tool shell-tools))) ;;FIXME: find better way
         ;; (_ (format #t "tool-dep?: ~A~%" tool-dep?))

         ;; (tool (if tool-dep?
         ;;           (let ((pkg (assoc-val :pkg (cdr tool)))
         ;;                 (tgt (assoc-val :tgt (cdr tool))))
         ;;             (format #f "//~A:~A" pkg tgt))
         ;;           tool))

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
    ;; (format #t "TOol-dep? ~A~%" tool-dep?)
    (format #t "TOol ~A~%" tool)
    (format #t "ARgs ~A~%" args)
    (values tool-dep? tool args)))

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
      (format outp "    content = [~S],\n" content)
      (format outp "    name    = \"__~A__\"\n" outfile)
      (format outp ")\n")))

;; if target is in this pkg, then no need to copy, just alias
;; fully-qualified label
(define (starlark-emit-copy-file outp pkg-path stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-copy-file") stanza)
  (let* ((output (assoc-val :outputs (cdr stanza)))
         (_ (format #t "~A: ~A~%" (yellow "output") output))
         (output-alist (cdar output))
         (_ (format #t "~A: ~A~%" (yellow "output-alist") output-alist))
         (out-pkg (assoc-val :pkg output-alist))
         (_ (format #t "~A: ~A~%" (yellow "out-pkg") out-pkg))
         (_ (format #t "~A: ~A~%" (yellow "pkg-path") pkg-path))
         (out-tgt (assoc-val :tgt output-alist))
         (_ (format #t "~A: ~A~%" (yellow "out-tgt") out-tgt))

         (deps (assoc-val :deps stanza))
         (_ (format #t "~A: ~A~%" (uwhite "deps") deps))

         (cmd-args (cdr (assoc-in '(:actions :cmd :args) stanza)))
         (_ (format #t "~A: ~A~%" (uwhite "cmd-args") cmd-args))
         (src-arg (assoc-val (car cmd-args) deps))
         (_ (format #t "~A: ~A~%" (ured "src-arg") src-arg))

         (in-pkg (assoc-val :pkg src-arg))
         (_ (format #t "~A: ~A~%" (yellow "in-pkg") in-pkg))
         (in-pkg (if (equal? in-pkg "./")
                     "//"
                     (if (equal? pkg-path in-pkg)
                         ""
                         (format #f "//~A" in-pkg))))

         (in-tgt (assoc-val :tgt src-arg))
         (_ (format #t "~A: ~A~%" (yellow "in-tgt") in-tgt))

         ;; (_ (error 'STOP "stop cp"))
         )
    ;; (error 'STOP "stop cp")
    (if (equal? out-pkg pkg-path)
        (begin
          (format #t "~A: ~A~%" (ured "emitting copy_file") output)
          (format outp "######\n")
          (format outp "copy_file(~%")
          (format outp "    name = \"__~A\",~%" out-tgt)
          (format outp "    src  = \"~A:~A\",~%" in-pkg in-tgt)
                  ;; (if (equal? pkg-path in-pkg) "" "//")
                  ;; in-tgt)
          (format outp "    out  = \"~A\"~%" out-tgt)
          (format outp ")~%")
          )
        ;; copying to external pkg dir???
        (let ((out  (format #f "~A" tgt))
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
          (format outp "copy_file(\n")
          ;; (format outp "    out      = \"~A\",\n" out)
          ;; (format outp "    content  = [")
          ;; (format outp "~S" content)
          ;; (format outp "],~%")
          ;; (format outp "    name     = \"~A\"\n" name)
          (format outp ")")
          (newline outp)
          (newline outp)))))

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
    (format outp "    content  = [")
    (format outp "~S" content)
    (format outp "],~%")
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

  ;; FIXME: if rule is alias of test executable, skip it - the
  ;; executable will be translated to ocaml_test

  ;; FIXM: :actions may contain multiple :cmd (progn); iterate
  (for-each (lambda (cmd)
              (format #t "Rule cmd: ~A~%" cmd)
              (if (eq? :cmd (car cmd))
                  (let* ((tool (assoc-val :tool (cdr cmd)))
                         (_ (format #t "tool: ~A~%" tool)))
                    (case (car tool)
                      ((:write-file)
                       (starlark-emit-write-file-target outp stanza))
                      ;; ((::cat) ;=> genrule
                      ;;  (error 'fixme "IMPLEMENT :cat"))
                      ((:copy ::copy)
                       (starlark-emit-copy-file outp pkg-path stanza))
                      ;; the rest of dune-dsl-cmds (dune_stanza_rule.scm)
                      (else
                       ;; (if (is-test-executable? tool) ...
                       (starlark-emit-genrule outp pkg-path stanza))))))
            (assoc-val :actions stanza)))


(define (starlark-emit-rule-targets outp pkg) ;;fs-path stanzas)
  (format #t "~A: ~A~%" (blue "starlark-emit-rule-targets") pkg)

  ;; same code as starlark-emit-aggregate-targets, but we want to put
  ;; aggregates and rules in different locations.
  (let ((flag #t)
        (pkg-path (car (assoc-val :pkg-path pkg))))
    (for-each (lambda (stanza)
                (format #t "stanza tag ~A\n" (car stanza))
                (if (alist? (cdr stanza))
                    (format #t "RULE ACTION: ~A\n" (assoc :actions (cdr stanza))))
                (case (car stanza)
                  ((:rule :genrule :with-stdout-to :write-file)
                   (if flag
                       (begin
                         (format outp "############################# Rules ##################################~%")
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
                                          ;; rule with multiple cmds (progn)
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

