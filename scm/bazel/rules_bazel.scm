;; rules.scm

(define (-deps-kw->cmd-args deps)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (blue "-deps-kw->cmd-args") deps))
  (let ((args (map (lambda (dep)
                     (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "dep: ~A\n" dep))
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
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A in ~A~%" (blue "-arg->dep") arg deps))
  ;; e.g. deps:
  ;; ((:_ "rules/copy/config/config.mlh" "rules/copy/foo/bar.x") (:: "baz.y"))
  (let* ((dlist (find-if (lambda (deplist)
                           (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "deplist: ~A~%" deplist))
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
                    (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "found dlist: ~A~%" dlist))
                    (find-if (lambda (d)
                               (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "finding?: ~A~%" d))
                               (string-suffix? arg d))
                             (cdr dlist)))
                  #f)))
    (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "dep: ~A~%" dep))
    dep))

(define (-deps->args deps)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (blue "-deps->args") deps))
  (map (lambda (dep) 
        (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "dep") dep))
         (if (eq? ::tools (car dep))
             (values)
             ;; FIXME: (cdr dep) is a (:pkg) (:tgt) pair. deal with it.
             (cdr dep)))
       deps))

(define (-resolve-args cmd pkg-path deps outputs)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (blue "-resolve-args") cmd)
        (format #t "~A: ~A~%" (yellow "deps") deps)
        (format #t "~A: ~A~%" (yellow "outputs") outputs)))
  (let* ((-args (assoc-val :args (cdr cmd)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "-args: ~A~%" -args)))
         (args
          (map (lambda (arg)
                 (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "Mapping arg: ~A~%" arg))
                 (cond
                  ((or (equal? arg :targets)
                       (equal? arg :outputs))
                   (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "Arg is") arg))
                   (let ((outs (outputs->outs-attr pkg-path outputs)))
                     (map (lambda (out)
                            (format #f "$(location ~A)" out))
                          outs)))

                  ((equal? arg ::deps)
                   (if (or *mibl-debug-emit* *mibl-debug-s7*)
                       (format #t "~A: ~A~%" (red "Arg is ::deps") arg))
                   ;;(error 'fixme "unhandled %{deps} arg")
                     (map (lambda (dep)
                            (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                (format #t "~A: ~A~%" (yellow "dep") dep))
                            (let* ((label (cdr dep))
                                   (pkg (assoc-val :pkg label))
                                   (tgt-tag (caadr label))
                                   (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                          (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag)))
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
                   (if (or *mibl-debug-emit* *mibl-debug-s7*)
                       (begin
                         (format #t "KW arg: ~A\n" arg)
                         (format #t "deps: ~A\n" deps)))
                   (if (equal? :deps arg)
                       (begin
                         (-deps->args deps))
                         ;; (error 'x "x"))
                       (if (equal? ::pkg-dir arg)

                           "./"
                           ;; find args in deplist
                           (let ((found
                                  (find-if (lambda (dep)
                                             (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "checking dep: ~A\n" dep))
                                             (equal? arg (car dep)))
                                           deps)))
                             (if found
                                 (begin
                                   (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                       (format #t "~A: ~A~%" (red "FOUND arg") found))
                                   (if (equal? (cdr found) ::unresolved)
                                       (symbol->string (keyword->symbol (car found)))
                                       (let* ((label (cdr found))
                                              (ws (if-let ((ws (assoc-val :ws label)))
                                                          ws ""))
                                              (pkg (let ((p (format #f "~A" (assoc-val :pkg label))))
                                                     (if (string=? p "./") "." p)))
                                              (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                                     (format #t "~A: ~A~%" (bgred "pkg") pkg)))
                                              ;; tgt tag may be :tgt, :tgts, :glob, or :fg
                                              (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                                     (format #t "~A: ~A~%" (bgyellow "getting tgt tag") label)))
                                              (tgt-pair (dissoc '(:ws) label))
                                              (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                                     (format #t "~A: ~A~%" (yellow "tgt-pair") tgt-pair)))
                                              (tgt-pair (car (dissoc '(:pkg) tgt-pair)))
                                              (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                                     (format #t "~A: ~A~%" (yellow "tgt-pair") tgt-pair)))
                                              (tgt-tag (car tgt-pair))
                                              (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                                     (format #t "~A: ~A~%" (red "tgt-tag") tgt-tag)))
                                              (tgt (cdr tgt-pair)))
                                         (if (equal? pkg pkg-path)
                                             (if (eq? tgt-tag :glob) ;; :tgts)
                                                 (format #f "`realpath $(locations :~A)`" tgt)
                                                 (format #f "$(location :~A)" tgt))
                                             (if (eq? tgt-tag :glob) ;; :tgts)
                                                 (format #f "`realpath $(locations ~A//~A:~A)`"
                                                         ws pkg tgt)
                                                 (format #f "$(location ~A//~A:~A)"
                                                         ws pkg tgt))))))
                                 ;; else not found in :deps, try :outputs?

                                 (error 'fixme
                                        (format #f "~A: ~A~%"
                                                (red "kw arg unresolved") arg)))))))
                  ((number? arg) (format #f "~A" arg))

                  ((string? arg) ;; e.g. a file literal
                   (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "arg: string literal\n"))
                   (if (char=? #\- (arg 0))
                       arg
                   ;; how do we know which strings need $(location)?
                   ;; assumption: files must be listed in deps, so any
                   ;; strings we see are just string args
                       (if-let ((x (-arg->dep arg deps)))
                               ;; (assoc arg deps)))
                               (begin
                                 (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "found arg ~A in deps\n" x))
                                 (let* ((fname (format #f "~A" x))
                                        (dname (dirname fname))
                                        (bname (basename fname)))
                                   (let ((tmp (format #f "$(location ~A)"
                                                      (if (equal dname pkg-path)
                                                          bname fname))))
                                     tmp)))

                               (begin
                                 (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "arg not in deps: ~A~%" arg))
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
    (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "Args") args))
    args))

(define (derive-cmd pkg-path cmd deps targets)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "derive-cmd") cmd)
        (format #t "~A: ~A~%" (blue "targets") targets)
        (format #t "~A: ~A~%" (blue "deps") deps)))
  ;; (format #t "stdout: ~A~%" (assoc-val :stdout cmd))

  ;; ((:cmd (:run (:tool (:pkg :bin) (:tgt ...
  ;; ((:cmd (:tool cat) (:args ...

  ;; tool: two forms, one for builtin cmd, one for run
  ;; builtin: (:cmd (:tool cat) ...)
 ;; run: (:cmd (:tool (:pkg :bin) (:tgt foo)) ...)

  (let* ((cmd-alist (cdr cmd))
         (tool (car (assoc-val :tool cmd-alist)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (blue "tool") tool)))
         ;; (_ (format #t "tool: ~A~%" (cdr tool)))
         ;; (_ (format #t "(alist? (cdr tool)) ~A~%" (alist? (cdr tool))))
         (tool (-tool-for-genrule pkg-path tool deps))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (blue "RESOLVED TOOL for genrule") tool)))
         ;; (_ (error 'X "STOP derive-cmd"))

         (tool-dep? (not (member tool shell-tools))) ;;FIXME: find better way
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "tool-dep?: ~A~%" tool-dep?)))

         ;; (tool (if tool-dep?
         ;;           (let ((pkg (assoc-val :pkg (cdr tool)))
         ;;                 (tgt (assoc-val :tgt (cdr tool))))
         ;;             (format #f "//~A:~A" pkg tgt))
         ;;           tool))

        ;; FIXME: shell cmds v. targets that go in tools attr

         ;; FIXME: map dune builtins like 'copy' to shell cmds

         (args (-resolve-args cmd pkg-path deps targets))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (red "RESOLVED ARGS") args)))
         ;; args may mix strings and lists of strings
         (args (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() args))
         (args (if (assoc-val :stdout (cdr cmd))
                   (append args (list "> $@")) args))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "args: ~A~%" args)))
         ;; (args (format #f "~A" args))
         )
    (if (or *mibl-debug-emit* *mibl-debug-s7*)
        (begin
          (format #t "TOol-dep? ~A~%" tool-dep?)
          (format #t "TOol ~A~%" tool)
          (format #t "ARgs ~A~%" args)))
    (values tool-dep? tool args))) ;;))

(define (bazel-emit-skylib-write-file outp cmd pkg-path stanza)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-skylib-write-file") cmd))
  (let* ((args (assoc :args (cdr cmd)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "args") args)))
         (out-tag (cadr args))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "out-tag") out-tag)))
         (outputs (assoc :outputs (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "outputs") outputs)))
         (tfql (assoc out-tag (cdr outputs)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "tfql") tfql)))
         (out-pkg (assoc-val :pkg (cdr tfql)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "out-pkg") out-pkg)))
         (out-tgt (assoc-val :tgt (cdr tfql)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "out-tgt") out-tgt)))

         (outfile (if (equal? out-pkg pkg-path)
                      out-tgt (format #f "~A/~A" out-pkg out-tgt)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (yellow "outfile") outfile)))

         (content (last (last args))) ;; (assoc '(:content) (cddr args)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "~A: ~A~%" (green "content") content))))

      (format outp "###########\n")
      (format outp "write_file(\n")
      (format outp "    out     = \"~A\",\n" outfile)
      (format outp "    content = [~S],\n" content)
      (format outp "    name    = \"__~A__\"\n" outfile)
      (format outp ")\n")))

;; if target is in this pkg, then no need to copy, just alias
;; fully-qualified label
(define (bazel-emit-copy-file outp pkg-path stanza)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-copy-file") stanza))
  (let* ((output (assoc-val :outputs (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "output") output)))
         (output-alist (cdar output))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "output-alist") output-alist)))
         (out-pkg (assoc-val :pkg output-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "out-pkg") out-pkg)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "pkg-path") pkg-path)))
         (out-tgt (assoc-val :tgt output-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "out-tgt") out-tgt)))

         (deps (assoc-val :deps stanza))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "deps") deps)))

         (cmd-args (cdr (assoc-in '(:actions :cmd :args) stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "cmd-args") cmd-args)))
         (src-arg (if (equal? :deps (car cmd-args))
                      deps
                      (assoc-val (car cmd-args) deps)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ured "src-arg") src-arg)))

         (in-pkg (assoc-val :pkg src-arg))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "in-pkg") in-pkg)))
         (in-pkg (if (equal? in-pkg "./")
                     "//"
                     (if (equal? pkg-path in-pkg)
                         ""
                         (format #f "//~A" in-pkg))))

         (in-tgt (assoc-val :tgt src-arg))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "in-tgt") in-tgt)))

         ;; (_ (error 'STOP "stop cp"))
         )
    ;; (error 'STOP "stop cp")
    (if (equal? out-pkg pkg-path)
        (begin
          (if (or *mibl-debug-emit* *mibl-debug-s7*)
              (format #t "~A: ~A~%" (ured "emitting copy_file") output))
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
              (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "args") args)))
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

(define (bazel-emit-write-file-target outp stanza)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-write-file-target") stanza))
  (let* ((output (assoc-val :outputs (cdr stanza)))
        (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "output") output)))
        (pkg-tgt (cdar output))
        (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "pkg-tgt") pkg-tgt)))
        (pkg (assoc-val :pkg pkg-tgt))
        (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "pkg") pkg)))
        (tgt (assoc-val :tgt pkg-tgt))
        (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "tgt") tgt)))
        (out  (format #f "~A" tgt))
        (name (format #f "__~A__" tgt))
        (args (assoc-in '(:actions :cmd :args) (cdr stanza)))
        (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "args") args)))
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

(define (bazel-emit-diff-test-target outp stanza pkg-path)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-diff-test-target") stanza))
  (let* ((output (assoc-val :outputs (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "output") output)))
         (bn (basename pkg-path))
         (args (assoc-in '(:actions :cmd :args) (cdr stanza)))
         (arg1 (keyword->symbol (args 1)))
         (arg2 (keyword->symbol (args 2)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "args") args)))
         ;; (name (format #f "~A_~A_test" arg1 arg2))
         (name (format #f "~A_diff_test" bn)))

    (format outp "###########\n")
    (format outp "diff_test(\n")
    (format outp "    name     = \"~A\",\n" name)
    (format outp "    file1    = \"~A\",\n" arg1)
    (format outp "    file2    = \"~A\",\n" arg2)
    (format outp "    timeout  = \"short\"\n")
    (format outp ")\n")
    (newline outp)
    ))

(define (bazel-emit-bindiff-test-target outp stanza pkg-path)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-bindiff-test-target") stanza))
  (let* ((output (assoc-val :outputs (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "output") output)))
         (bn (basename pkg-path))
         (args (assoc-in '(:actions :cmd :args) (cdr stanza)))
         (arg1 (keyword->symbol (args 1)))
         (arg2 (keyword->symbol (args 2)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "args") args)))
         ;; (name (format #f "~A_~A_test" arg1 arg2))
         (name (format #f "~A_bindiff_test" bn)))

    (format outp "###########\n")
    (format outp "bindiff_test(\n")
    (format outp "    name     = \"~A\",\n" name)
    (format outp "    file1    = \"~A\",\n" arg1)
    (format outp "    file2    = \"~A\",\n" arg2)
    (format outp "    timeout  = \"short\"\n")
    (format outp ")\n")
    (newline outp)
    ))

;; (:cppo (:deps
;;         (:out (:pkg . "lib") (:tgt . "yojson.cppo.mli"))
;;         (:monomorphic.mli (:pkg . "lib") (:tgt . "monomorphic.mli")) ... )
;;        (:outputs (:yojson.mli (:pkg . "lib") (:tgt . "yojson.mli")))
;;        (:actions (:cmd (:tool :cppo) (:args :out "-o" :outputs))))
(define (bazel-emit-cppo-target outp stanza pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-cppo-target") stanza))
  (let* ((pkg-path (assoc-val :pkg-path pkg))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "pkg path") pkg-path)))
         (outputs (assoc-val :outputs (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "outputs") outputs)))
         (name (assoc-val :tgt (cdr (outputs 0))))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "name") name)))
         (outputs (map (lambda (output)
                         (if (or *mibl-debug-emit* *mibl-debug-s7*)
                             (format #t "~A: ~A~%" (yellow "output") output))
                         (let ((tgt-path (assoc-val :pkg (cdr output)))
                               (tgt (assoc-val :tgt (cdr output))))
                           (if (equal? pkg-path tgt-path)
                               (format #f "~A" tgt)
                               (format #f "~A/~A" tgt-path tgt))))
                       outputs))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "outputs") outputs)))
         (cmd-outputs (map (lambda (out) (format #f "$(location ~A)" out))
                           outputs))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "cmd-outputs") cmd-outputs)))
         (deps (assoc-val :deps (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "deps") deps)))
         (srcs (map (lambda (src)
                      (if (or *mibl-debug-emit* *mibl-debug-s7*)
                          (format #t "~A: ~A~%" (yellow "src") src))
                      (let ((tgt-path (assoc-val :pkg (cdr src)))
                            (tgt (assoc-val :tgt (cdr src))))
                        (if (equal? pkg-path tgt-path)
                            (format #f "~A" tgt)
                            (format #f "~A/~A" tgt-path tgt))))
                      deps))
         (include-dirs (remove-duplicates
                        (map (lambda (src)
                               (let ((dn (dirname src)))
                                 (if (string=? "./" dn)
                                     pkg-path
                                     dn)))
                             srcs)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uyellow "include") include-dirs)))
         (args (assoc-in '(:actions :cmd :args) (cdr stanza)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "args") args)))
         (args (flatten
                (map (lambda (arg)
                       (if (or *mibl-debug-emit* *mibl-debug-s7*)
                           (format #t "~A: ~A~%" (yellow "arg") arg))
                       (if-let ((a (assoc-val arg deps)))
                               (let ((_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "A") a)))
                                     (tgt-path (assoc-val :pkg a))
                                     (tgt (assoc-val :tgt (cdr a))))
                                 (if (equal? pkg-path tgt-path)
                                     (format #f "$(location ~A)" tgt)
                                     (format #f "$(location ~A/~A)"
                                             tgt-path tgt)))
                               (if (equal? :outputs arg)
                                   cmd-outputs
                                   arg)))
                     (cdr args)))))
    (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (yellow "args") args))
    ;; (error 'x "x")
    ;; (rule
    ;;  (targets yojson.mli)
    ;;  (deps
    ;;    (:out yojson.cppo.mli)
    ;;    monomorphic.mli
    ;;  ...
    ;;  (action (run cppo %{out} -o %{targets})))

    (format outp "###########\n")
    (format outp "genrule(\n")
    (format outp "    outs  = [\n")
    (format outp "~{        \"~A\"~^,~%~}~%" outputs)
    (format outp "    ],\n")

    (format outp "    name  = \"__~A__\",\n" name)

    (format outp "    srcs  = [~%")
    (format outp "~{        \"~A\"~^,~%~}~%" srcs)
    (format outp "    ],~%")

    (format outp "    cmd   = \" \".join([~%")
    (format outp "        \"$(execpath @cppo//bin:cppo)\",~%")
    (format outp "~{        \"-I\", \"~A\"~^,~%~},~%" include-dirs)
    ;; include GENDIR, in case src files are generated (e.g. by ocamllex)
    (format outp "~{        \"-I\", \"$(GENDIR)/~A\"~^,~%~},~%" include-dirs)
    (format outp "~{        \"~A\"~^,~%~}~%" args)
   (format outp "    ]),~%")

    ;; (if (truthy? srcs)
    ;;     (if bash-cmd?
    ;;         (emit-bash-srcs outp srcs pkg-path stanza)
    ;;         (emit-bash-srcs outp srcs pkg-path stanza)
    ;;         ;; (begin
    ;;         ;;   (format #t "~A: ~A~%" (red "SrcS") srcs)
    ;;         ;;   (format outp "    SRCS  = [\n")
    ;;         ;;   (format outp "~{        \"~A\"~^,\n~}\n" srcs)
    ;;         ;;   (format outp "    ],\n"))
    ;;         ))

    ;; (if bash-cmd?
    ;;     (emit-bash-cmd outp with-stdout? outs pkg-path stanza)
    ;;     (emit-shell-cmd outp tool
    ;;                     with-stdout?
    ;;                     srcs
    ;;                     deps ;; (dissoc '(::tools) deps)
    ;;                     action
    ;;                     outputs ;; outs
    ;;                     pkg-path
    ;;                     stanza))

    (format outp "    exec_tools = [\"@cppo//bin:cppo\"]\n")

    (format outp ")")
    (newline outp)
    (newline outp)
    ))

(define (bazel-emit-rule-target outp pkg-path stanza)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-rule-target") stanza))

  ;; FIXME: if rule is alias of test executable, skip it - the
  ;; executable will be translated to ocaml_test

  ;; FIXM: :actions may contain multiple :cmd (progn); iterate
  (for-each (lambda (cmd)
              (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "Rule cmd: ~A~%" cmd))
              (if (eq? :cmd (car cmd))
                  (let* ((tool (assoc-val :tool (cdr cmd)))
                         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "tool: ~A~%" tool))))
                    (case (car tool)
                      ((:write-file)
                       (bazel-emit-write-file-target outp stanza))
                      ;; ((::cat) ;=> genrule
                      ;;  (error 'fixme "IMPLEMENT :cat"))
                      ((:copy ::copy)
                       (bazel-emit-copy-file outp pkg-path stanza))
                      ;; the rest of dune-dsl-cmds (dune_stanza_rule.scm)
                      (else
                       ;; (if (is-test-executable? tool) ...
                       (bazel-emit-genrule outp pkg-path stanza))))))
            (assoc-val :actions stanza)))

(define (bazel-emit-diff-target outp pkg-path stanza-alist)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (bgblue "bazel-emit-diff-target") stanza-alist))
  (let* ((action (assoc-val :actions stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "action") action)))
         (tool (cadr (assoc-in '(:cmd :tool) action)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "tool") tool)))
         (bash-cmd? (eq? tool 'bash))
         (deps (assoc-val :deps stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (cyan "deps") deps)))
         (srcs (deps->srcs-attr pkg-path tool deps))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (cyan "genrule srcs") srcs)))
         ;; (_ (error 'X "stop genrule"))
         ;; FIXME: derive from :args, :stdout, etc.
         ;; if %{targets} is in cmd string, ...
         ;; else if we have (:stdout ...), ...
         (with-stdout? (assoc-in '(:actions :stdout) stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ucyan "with-stdout?") with-stdout?)))
         (outputs (assoc-val :outputs stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ucyan "outputs") outputs)))
         (outs (if outputs (outputs->outs-attr pkg-path outputs) '()))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ucyan "outs") outs)))

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
    (for-each
     (lambda (cmd)
       (if (eq? :cmd (car cmd))
           (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "GENRULE ACTION (B): ~A~%" cmd))
           (if (eq? :stdout (car cmd))
               (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "GENRULE STDOUT (B): ~A~%" cmd))
               (error 'fixme (format #f "unknown genrule cmd: ~A" cmd)))))
     action)

    (if-let ((ctx (assoc-in '(:actions :ctx) stanza-alist)))
            (begin
              (format outp "## omitted:\n")
              (format outp "## (chdir ~A (run ~A ...))\n\n"
                      (cadr ctx)
                      (cadr (assoc-in '(:actions :cmd :tool) stanza-alist))))
            ;; else
            (begin
              (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "  outs: ~A~%" outs))

              (if (list? stanza-alist)
                  (begin
                    (if (or *mibl-debug-emit* *mibl-debug-s7*)
                        (format #t "~A~%" (uwhite "emitting diff_test")))
                    (format outp "##########\n")
                    (format outp "diff_test(\n")
                    (format outp "    name  = \"~A.diff_test\",\n" name)
                    (format outp "    file1  = \"~A\",~%" (srcs 0))
                    (format outp "    file2  = \"~A\"~%" (srcs 1))

                    (format outp ")~%")
                    (newline outp))
                  )))
    ))

(define (-node-stanza->tool pkg-path args deps)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "-node-stanza->tool") deps))
  (let ((tool-tag (car args)))
    (if-let ((tool (filter (lambda (dep) (equal? tool-tag (car dep)))
                           deps)))
            (let ((pkg (assoc-val :pkg (cdar tool)))
                  (tgt (assoc-val :tgt (cdar tool))))
              (if (or *mibl-debug-emit* *mibl-debug-s7*)
                  (format #t "~A: ~A~%" (red "node :tool") tool))
              (if (equal? (format #f "~A" pkg-path)
                          (format #f "~A" pkg))
                  ;;(format #t "~A: ~A~%" (red "node :tool") tgt)
                  (format #f ":~A" tgt)
                  ;; (format #t "~A: ~A:~A~%" (red "node tool") pkg tgt)
                  (format #f "~A:~A" pkg tgt)))
            (error 'FIXME
                   (format #f "node tool arg ~A not found in deps: ~A"
                           tool-tag deps)))))

(define (bazel-emit-node-target outp pkg-path stanza-alist)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (bgblue "bazel-emit-node-target") stanza-alist))
  (let* ((action (assoc-val :actions stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "action") action)))

         ;; (tool (cadr (assoc-in '(:cmd :tool) action)))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "tool") tool))
         ;; (bash-cmd? (eq? tool 'bash))

         (args (assoc-in '(:cmd :args) action))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (cyan "node args") args)))

         (deps (assoc-val :deps stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (cyan "deps") deps)))

         (tool (-node-stanza->tool pkg-path (cdr args) deps))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "tool") tool)))
         ;; (_ (error 'X "stop node"))

         ;; (srcs (deps->srcs-attr pkg-path tool deps))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (cyan "genrule srcs") srcs)))
         (srcs #f)

         ;; FIXME: derive from :args, :stdout, etc.
         ;; if %{targets} is in cmd string, ...
         ;; else if we have (:stdout ...), ...
         (with-stdout? (assoc-in '(:actions :stdout) stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ucyan "with-stdout?") with-stdout?)))
         (outputs (assoc-val :outputs stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ucyan "outputs") outputs)))
         (outs (if outputs (outputs->outs-attr pkg-path outputs) '()))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ucyan "outs") outs)))

         (name (if (truthy? outs)
                   (format #f "~A" (outs 0))
                   (if (truthy? srcs)
                       (string-left-trim ":" (srcs 0))
                       (if (truthy? deps)
                           (if (keyword? (caar deps))
                               (string-left-trim ":"
                                                 (format #f "~A" (caar deps)))
                               "BAR" ;; (caar deps)
                               )
                           "foobar"))))
         )

    ;; progn: list of actions. should be just one?
    ;; (for-each
    ;;  (lambda (cmd)
    ;;    (if (eq? :cmd (car cmd))
    ;;        (format #t "GENRULE ACTION (C): ~A~%" cmd)
    ;;        (if (eq? :stdout (car cmd))
    ;;            (format #t "GENRULE STDOUT (C): ~A~%" cmd)
    ;;            (error 'fixme (format #f "unknown genrule cmd: ~A" cmd)))))
    ;;  action)

    (if-let ((ctx (assoc-in '(:actions :ctx) stanza-alist)))
            (begin
              (format outp "## omitted:\n")
              (format outp "## (chdir ~A (run ~A ...))\n\n"
                      (cadr ctx)
                      (cadr (assoc-in '(:actions :cmd :tool) stanza-alist))))
            ;; else
            (if (list? stanza-alist)
                (begin
                  (if (or *mibl-debug-emit* *mibl-debug-s7*)
                      (format #t "~A~%" (uwhite "emitting js_run_binary")))
                  (format outp "##############\n")
                  (format outp "js_run_binary(\n")
                  (format outp "    name  = \"__~A__\",\n" name)

                  (format outp "    tool  = \"~A\",~%" tool)
                  (if (truthy? outs)
                      (format outp "    stdout  = \"~A\"~%" (car outs))
                      (format outp "    stdout  = \"~A.stdout\"~%" name))
                  (format outp ")~%")
                  (newline outp))
                ))
    ))

(define (bazel-emit-rule-targets outp pkg) ;;fs-path stanzas)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (blue "bazel-emit-rule-targets") pkg))

  ;; same code as bazel-emit-aggregate-targets, but we want to put
  ;; aggregates and rules in different locations.
  (let ((flag #t)
        (pkg-path (assoc-val :pkg-path pkg)))
    (for-each (lambda (stanza)
                (if (or *mibl-debug-emit* *mibl-debug-s7*)
                    (format #t "stanza tag ~A\n" (car stanza)))
                (if (or *mibl-debug-emit* *mibl-debug-s7*)
                    (if (alist? (cdr stanza))
                        (format #t "RULE ACTION: ~A\n" (assoc :actions (cdr stanza)))))
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
                          (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "cmd-list") cmd-list)))
                          (cmd-ct (length cmd-list)))
                     (if (> cmd-ct 1)
                         (for-each (lambda (cmd)
                                     (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                         (format #t "~A: ~A~%" (red "cmd") cmd))
                                     (let ((tool (car (assoc-val :tool (cdr cmd)))))
                                       (case tool
                                         ((:write-file)
                                          ;; rule with multiple cmds (progn)
                                          (bazel-emit-skylib-write-file outp cmd pkg-path stanza))
                                         (else
                                          (bazel-emit-rule-target outp pkg-path (cdr stanza))))))
                                   cmd-list)
                         (bazel-emit-rule-target outp pkg-path (cdr stanza)))))

                  ((:cppo)
                   (bazel-emit-cppo-target outp stanza pkg))

                  ((:bindiff-test)
                   (bazel-emit-bindiff-test-target outp stanza (assoc-val :pkg-path pkg)))

                  ((:diff-test)
                   (bazel-emit-diff-test-target outp stanza (assoc-val :pkg-path pkg)))

                  ((:write-file)
                   (bazel-emit-write-file-target outp stanza))

                  ;;FIXME the rest are obsolete
                  ((:with-stdout-to)
                   (if (not (assoc-in '(:cmd :universe) (cdr stanza)))
                       (bazel-emit-with-stdout-to-target outp fs-path
                                                            (cdr stanza))
                       ;; else FIXME: deal with universe stuff
                       ))
                  ((:node)
                   (bazel-emit-node-target outp pkg-path (cdr stanza)))
                  ((:diff)
                   (bazel-emit-diff-target outp pkg-path (cdr stanza)))

                  ((:executable :prologues
                                :test
                                :shared-deps
                                :shared-ppx
                                :lex :yacc :menhir
                                :ocamlc
                                :library :archive
                                :ns-archive :ns-library
                                :alias :install :env)
                   (values))

                  ((:shared-opts :shared-link-opts :shared-ocamlc-opts :shared-ocamlopt-opts) (values))

                  (else
                   (error 'FIXME
                          (format #f "unhandled rule stanza: ~A~%" (car stanza)))
                   )))
              (assoc-val :mibl pkg))))


