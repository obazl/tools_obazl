;; (format #t "loading @camlark//bazel_emit_rules.scm\n")

(load "dune.scm")
(load "opam.scm")
(load "string.scm")
;; (load "s7/stuff.scm")
(load "utils.scm")

;; (load "stuff.scm") ;; concatenate

;; (define (bazel-emit-rule-target outp stanza)
;;   (format #t "bazel-emit-rule-target: ~A\n" stanza)
;;   ;; (let ((libname (cdadr (assoc :name stanza)))
;;   ;;       (opts (stanza-opts stanza))
;;   ;;       (deps '("test-dep1" "test-dep2"))
;;   ;;       (modules '("test-mod1" "test-mod2"))
;;   ;;       (submodules (stanza-submodules typ stanza)))

;;     (format outp "################  rule  ################\n")
;;     (if (list? stanza)
;;         (begin
;;           (format outp "## (\n")
;;           (for-each (lambda (sexp)
;;                       (format outp "##   ~A\n" sexp))
;;                     stanza)
;;           (format outp "## )\n"))))

    ;; (format outp "genrule(\n")
    ;; (format outp "    name    = \"~A\",\n" "rulename")
    ;; (format outp "    srcs = [],\n")
    ;; (format outp "    outs = [],\n")
    ;; (format outp "    cmd  = \"\",\n")
    ;; (format outp "    tools = []\n")
    ;; (format outp ")\n\n"))

(define (run-cmd->srcs fs-path stanza)
  (format #t "run-cmd->srcs: ~A\n" fs-path)
  (format #t " stanza: ~A\n" stanza)
  (if-let ((deps (assoc :deps stanza)))
          (begin
            (format #t "run-cmd->srcs: ~A\n" deps)
            (cadr deps))
          '()))

(define (run-cmd->cmd fs-path tool stanza)
  (format #t "run-cmd->cmd: ~A\n" fs-path)
  (format #t " stanza: ~A\n" stanza)
  (if-let ((args (assoc :args stanza)))
          (begin
            (format #t "run-cmd->srcs: ~A\n" deps)
            (cadr deps))
          '()))

(define (bazel-emit-run-cmd-target outp fs-path stanza)
  (format #t "bazel-EMIT-RUN-CMD-target: ~A\n" fs-path)
  (format #t " stanza: ~A\n" stanza)
  ;; (let ((libname (cdadr (assoc :name stanza)))
  ;;       (opts (stanza-opts stanza))
  ;;       (deps '("test-dep1" "test-dep2"))
  ;;       (modules '("test-mod1" "test-mod2"))
  ;;       (submodules (stanza-submodules typ stanza)))
  (let* ((tool (cmd-tool->label stanza))
         ;; (deps (cadr (assoc-in '(:cmd :deps) stanza)))
         (outf (cadr (assoc :out stanza)))
         (name (if outf
                   (string-append "__"
                                  (if (symbol? outf)
                                      (symbol->string outf)
                                      outf))
                   "FIXME"))
         (srcs (run-cmd->srcs fs-path stanza))
         (cmd (run-cmd->cmd fs-path tool stanza))
         )
    (format #t "RUN SRCS: ~A\n" srcs)

    (format outp "######## run cmd ########\n")
    (format outp "genrule(\n")
    (format outp "    name = \"~A\",\n" name)

    (format outp "    outs  = [\n")
    (format outp "        \"~A\"\n" (cadr (assoc :out stanza)))
    (format outp "    ],\n")

    ;; FIXME: handle multiple tools
    (format outp "    tools  = [\"//~A\"],\n" tool)
    (format outp "    cmd  = ~A,\n" cmd)

    (format outp "    srcs  = [\n")
    (for-each (lambda (src)
                (format outp "        \"~A\",\n" src))
              srcs)
    (format outp "    ],\n")
    (format outp ")\n")

    (if (list? stanza)
        (begin
          (format outp "## (\n")
          (for-each (lambda (sexp)
                      (format outp "##   ~A\n" sexp))
                    stanza)
          (format outp "## )\n")))
    )
  )

(define (cmd-tool->label stanza)
  (format #t "cmd-tool->label: ~A\n" stanza)
  ;; assumption: tool has form %{foo}
  ;; FIXME: handle case where tool is a string literal
  (let* ((tool (cadr (assoc-in '(:cmd :tool) stanza)))
         (tool-str (if (symbol? tool) (symbol->string tool) tool)))
    (format #t "tool str: ~A\n" tool-str)
    (if (not (char=? #\% (string-ref tool-str 0)))
        tool-str
        (let* ((tool-key (substring tool-str 2 (- (length tool-str) 1)))
               (tool-label (executables-table (string->symbol tool-key))))

          ;; if tool not in (local) executables-table, ???
          ;; eg. %{bin:ocp-ocamlres} => @ocaml//bin:ocp-ocamlres

          (format #t "TOOL KEY: ~A\n" tool-key)
          (format #t "TOOL LBL: ~A\n" tool-label)
          tool-label))))

(define (make-filedep-arg-label arg)
  ;; (format #t "make-filedep-arg-label: ~A\n" arg)
  ;; arg: (:static <path> <fname>), where fname may include dir segs
  ;; may contain '../', e.g.
  ;; (deps (:legacy_builder ../legacy_store/legacy_store_builder.exe))

  (let* ((path (cadr arg))
         (fname (caddr arg))
         (segs (string-split fname #\/)))
    ;; (format #t "  fname ~A\n" fname)
    ;; (format #t "  segs: ~A (len: ~A)\n" segs (length segs))
    (let recur ((segs segs)
                (s path))
      (if (null? segs)
          s
          (begin
            ;; (format #t " recur segs: ~A (len: ~A)\n" segs (length segs))
            (if (< (length segs) 2)
              (string-append s ":" (car segs))
              (recur (cdr segs)
                     (string-append s "/" (car segs)))))))))

(define (with-stdout-to->cmd tool deps stanza)
  (format #t "with-stdout-to->cmd: ~A\n" tool)
  (format #t "stanza: ~A\n" stanza)
  (let* ((args (cadr (assoc-in '(:cmd :args) stanza)))
         (_ (format #t "CMD ARGS: ~A\n" args))
         (out (cadr (assoc :out stanza)))
         (vars (if-let ((vars (assoc :vars stanza)))
                          (cadr vars) #f))
         (_ (format #t "CMD VARS: ~A\n" vars))
         (pgm (string-append
               "\" \".join([\n"
               (format #f "        \"$(execpath ~A) \",\n" tool)
               ;; (format #f "        \"~A\""
               (string-join
                (map (lambda (arg)
                       (cond
                        ((string? arg)
                         (format #f "        ~S," arg))
                        ((pair? arg)
                         (case ((car arg))
                           ((:_srcfile :_genfile)
                            (format #f "        \"$(rootpath //~A)\","
                                    (make-filedep-arg-label arg)))
                           ((:_string)
                            (format #f "        \"~A\"," (cadr arg)))

                           ((:_number)
                            (format #f "        \"~A\"," (cadr arg)))

                           (else ;; ???
                            )))))
                     args)
                "\n")
               "\n        \"> $@\"\n"
               "        ])")))
    pgm))

(define (bazel-emit-with-stdout-to-target outp fs-path stanza)
  (format #t "STARLARK-EMIT-with-stdout-to-target: ~A\n" fs-path)
  ;; (format #t "  stanza: ~A\n" stanza)
  (let* ((tool (cmd-tool->label stanza))
         (deps (cadr (assoc-in '(:cmd :deps) stanza)))
         (cmd (with-stdout-to->cmd tool deps stanza)))

    (format #t "DEPS: ~A\n" deps)
    ;; If the output is a .ml or .mli file, then we need to also
    ;; generate a compilation target (ocaml_signature, ocaml_module).
    ;; However, here we do not have deps and flags for them; if a lib
    ;; depends on the generated files, thats where the generation
    ;; should happen.

    (format outp "######## with-stdout-to ########\n")
    (format outp "genrule(\n")
    (format outp "    name = \"~A\",\n"
            (string-append "__"
                           (symbol->string (cadr (assoc :out stanza)))))

    (format outp "    outs  = [\n")
    (format outp "        \"~A\"\n" (cadr (assoc :out stanza)))
    (format outp "    ],\n")

    (format outp "    tools  = [\"~A\"],\n" tool)
    (format outp "    cmd  = ~A,\n" cmd)
    (if deps
        (begin
          (format outp "    srcs  = [\n")

          (for-each (lambda (out)
                      (if (pair? out)
                          (let ((path (cadr out))
                                (fpath (caddr out))
                                (src (make-filedep-arg-label out)))
                            (format outp "        \"//~A\",\n" src))
                          (error 'bad-arg
                         (format #f "Expected pair for genrule src: ~A" out))))
                    deps)

          (format outp "    ],\n")))

    (format outp ")\n")

    ;; (if (list? stanza)
    ;;     (begin
    ;;       (format outp "## (\n")
    ;;       (for-each (lambda (sexp)
    ;;                   (format outp "##   ~A\n" sexp))
    ;;                 stanza)
    ;;       (format outp "## )\n")))
    ))

;; (define (bazel-emit-rule-targets outp fs-path stanzas)
;;   ;; (format #t "bazel-emit-rule-targets")

;;   ;; same code as bazel-emit-aggregate-targets, but we want to put
;;   ;; aggregates and rules in different locations.
;;   (let ((flag #t))
;;     (for-each (lambda (stanza)
;;                 (case (car stanza)
;;                   ((rule :genrule :with-stdout-to :write-file)
;;                    (if flag
;;                        (begin
;;                          (format outp "########################\n")
;;                          (format outp "####  Rule Targets  ####\n")
;;                          (newline outp)
;;                          (set! flag #f)))))

;;                 (case (car stanza)
;;                   ((rule)
;;                    (bazel-emit-rule-target outp (cdr stanza)))
;;                   ((:run-cmd)
;;                    (bazel-emit-run-cmd-target outp fs-path (cdr stanza)))
;;                   ((:with-stdout-to)
;;                    (if (not (assoc-in '(:cmd :universe) (cdr stanza)))
;;                        (bazel-emit-with-stdout-to-target outp fs-path
;;                                                             (cdr stanza))
;;                        ;; else FIXME: deal with universe stuff
;;                        ))
;;                   ((:write-file)
;;                    (bazel-emit-write-file-target outp (cdr stanza)))
;;                   (else
;;                    ;; skip
;;                    )))
;;               stanzas)))

;; install targets - ignore
  ;; (if (assoc-in '(:stanzas install) (cdr dune-pkg-tbl))
  ;;     (begin
  ;;       (format outp "## install targets\n")
  ;;       (newline outp)))

(define (bazel-emit-build-files dune-pkg-tbls)
  (format #t "bazel-emit-build-files\n")
  (for-each
   (lambda (dune-pkg-tbl)
     (for-each
      (lambda (pkg-kv)
        ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
        (let* ((fs-path (car pkg-kv))
               (stanzas-assoc (assoc :stanzas (cdr pkg-kv))))
          ;; (format #t "emitting stanzas for: ~A\n" fs-path)
          ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
          (if stanzas-assoc ;; assoc returns #f if not found
              (let* ((stanzas (cadr stanzas-assoc))
                     (build-file (string-append fs-path "/BUILD.bazel"))
                     ;; fs-path should already exist
                     (outp
                      (catch #t
                             (lambda ()
                               (open-output-file build-file))
                             (lambda args
                               (format #t "OPEN ERROR"))
                               )))
                ;; (format #t "\nEmitting ~A, port ~A\n" build-file outp)

                (bazel-emit-build-file-hdr outp pkg-kv)
                ;; (newline outp)

                ;; (format #t "emitting executables\n")
                (bazel-emit-executable-targets outp fs-path stanzas)

                ;; (format #t "emitting aggregates\n")
                (bazel-emit-aggregate-targets outp fs-path stanzas)

                ;; (format #t "emitting module files\n")
                (bazel-emit-file-targets outp fs-path stanzas (cdr pkg-kv))

                ;; (format #t "emitting file generators\n")
                ;; ocamllex, ocamlyacc, etc.
                (bazel-emit-file-generators outp fs-path stanzas)

                ;; (format #t "emitting ppxes\n")
                (bazel-emit-ppxes outp fs-path stanzas)

                ;; (format #t "emitting rules\n")
                (bazel-emit-rule-targets outp fs-path stanzas) ;; (cdr pkg-kv))

                (close-output-port outp)

                ;; (let ((stanzas (cdr (assoc :stanzas (cdr path_pkg))))
                ;;       (srcfiles (if-let ((srcs (assoc :srcfiles (cdr path_pkg))))
                ;;                         (cadr srcs)
                ;;                         '())))
                ;;   )

                ;; (let ((lib-stanzas (filter-stanzas :library stanzas)))
                ;;   (if (not (null? lib-stanzas))
                ;;       (emit-library-args fs-path lib-stanzas srcfiles out-port)))

                ;; (let ((exec-stanzas (filter-stanzas 'executable stanzas)))
                ;;   (if (not (null? exec-stanzas))
                ;;       (begin
                ;;         (emit-executable-args fs-path exec-stanzas srcfiles out-port))))

                ;; (let ((execs-stanzas (filter-stanzas 'executables stanzas)))
                ;;   (if (not (null? execs-stanzas))
                ;;       (emit-executables-args fs-path execs-stanzas srcfiles out-port)
                ;;         ))

                ))))
      (cadr dune-pkg-tbl)))
   dune-pkg-tbls)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (bazel-elaborate-pkg-tbls dune-pkg-tbls modules-tbl)
;;   (for-each
;;    (lambda (dune-pkg-tbl)
;;      (for-each
;;       ;; for each pkg:
;;       ;;    for each srcfile:
;;       ;;       get module name for srcfile
;;       ;;       get depslist from dependencies sexp
;;       ;;       for each dep in depslist:
;;       ;;          is dep in an -opened ns?
;;       ;;             for each ns listed in 'flags' fld of aggregator:
;;       ;;                lookup the ns in modules tbl, search its submods
;;       ;;             alternatively: keep list of nss with module in m-tbl
;;       ;;             if :ns use it else use :name
;;       ;;             get path for label
;;       ;;       pull opts from aggregator module in dune-pkg
;;       ;;    for each library/executable stanza:
;;       ;;       for each srcdep ('modules' fld):
;;       ;;       for each libdep ('libraries' fld):

;;       (lambda (pkg-kv)
;;         ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
;;         (let* ((fs-path (car pkg-kv))
;;                (pkg (cdr pkg-kv))
;;                (stanzas-assoc (assoc :stanzas pkg)))
;;           ;; (format #f "fs-path: ~A" fs-path)
;;           ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
;;           (if stanzas-assoc ;; assoc returns #f if not found
;;               (let* ((stanzas (cadr stanzas-assoc)))

;;                 (bazel-emit-build-file-hdr outp pkg-kv)
;;                 ;; (newline outp)

;;                 (bazel-emit-aggregate-targets outp stanzas) ;; pkg-kv)
;;                 (newline outp)

;;                 ;; (cdr pkg-kv) == pkg

;;                 (bazel-emit-file-targets outp stanzas (cdr pkg-kv))
;;                 (newline outp)

;;                 (bazel-emit-rule-targets outp stanzas) ;; (cdr pkg-kv))
;;                 (newline outp)

;;                 (close-output-port outp)

;;                 ;; (let ((stanzas (cdr (assoc :stanzas (cdr path_pkg))))
;;                 ;;       (srcfiles (if-let ((srcs (assoc :srcfiles (cdr path_pkg))))
;;                 ;;                         (cadr srcs)
;;                 ;;                         '())))
;;                 ;;   )

;;                 ;; (let ((lib-stanzas (filter-stanzas :library stanzas)))
;;                 ;;   (if (not (null? lib-stanzas))
;;                 ;;       (emit-library-args fs-path lib-stanzas srcfiles out-port)))

;;                 ;; (let ((exec-stanzas (filter-stanzas 'executable stanzas)))
;;                 ;;   (if (not (null? exec-stanzas))
;;                 ;;       (begin
;;                 ;;         (emit-executable-args fs-path exec-stanzas srcfiles out-port))))

;;                 ;; (let ((execs-stanzas (filter-stanzas 'executables stanzas)))
;;                 ;;   (if (not (null? execs-stanzas))
;;                 ;;       (emit-executables-args fs-path execs-stanzas srcfiles out-port)
;;                 ;;         ))

;;                 ))))
;;       dune-pkg-tbl))
;;    dune-pkg-tbls)
;;   )
