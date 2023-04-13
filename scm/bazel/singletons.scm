(if (or *mibl-debug-emit* *mibl-debug-s7*)
    (format #t "loading bazel/singletons.scm\n"))

;; FIXME FIXME FIXME FIXME FIXME
;; lookup label for this sym
(define (-make-select-condition sym)
  (format #f "//foo/bar:~A" sym))

(define (make-selector module stanza)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A, ~A\n" (ublue "make-selector") module stanza))
  (let* ((module-name (car module))
         (filename (let ((pairs (cdr module)))
                     (if-let ((pr (assoc-val :ml_ pairs)))
                             pr
                             (if-let ((pr (assoc-val :mli_ pairs)))
                                     pr
                                     (error 'bad-module
                                            "no generated file")))))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "filename: ~A\n" filename)))
         (conditionals (assoc-in '(:compile :deps :conditionals) (cdr stanza))))
    (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "conditionals: ~A\n" conditionals))
    (let ((the-conditional (find-if (lambda (c)
                                      (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                          (format #t "c: ~A\n" c))
                                      (eq? filename
                                           (assoc-val :target c)))
                                    (cdr conditionals))))
      (if (or *mibl-debug-emit* *mibl-debug-s7*)
          (format #t "the-conditional: ~A\n" the-conditional))
      (error 'stop "STOP make-selector")

      (if the-conditional
          (let ((selectors (assoc-val :selectors the-conditional)))
            (if (or *mibl-debug-emit* *mibl-debug-s7*)
                (format #t "selectors: ~A\n" selectors))
            (map (lambda (s) (list
                              (-make-select-condition (car s))
                              (cdr s)))
                 selectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-get-ppx-args ppx-alist libname)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (ublue "-get-ppx-args") ppx-alist))
  ;; opts come in pairs
  (let* ((opts (if-let ((opts (assoc-val :opts ppx-alist)))
                       opts '()))
         (args (if-let ((args (assoc-val :args ppx-alist)))
                       args '()))
         (opts (append opts args))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uyellow "ppx opts") opts)))
         ;; (flags (assoc-in '(:runtime-opts :flags) ppx-alist))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uyellow "ppx flags") flags)))
         ;; (options (assoc-in '(:runtime-opts :options) ppx-alist))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uyellow "ppx options") options)))
         (ppx-args (if opts
                       (map (lambda (opt)
                              (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                  (format #t "~A: ~A~%" (uyellow "opt") opt))
                              (case opt
                               (($LIBNAME) libname)
                               (else opt)))
                           (flatten opts))
                       #f)))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uyellow "ppx options") options)))
         ;; (ppx-args (append (if flags (cdr flags) '())
         ;;                   (if options options '()))))
    (if (or *mibl-debug-emit* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (bgyellow "ppx args") ppx-args))
    ppx-args))

;; WARNING: :modules have form (A (:ml a.ml)(:mli a.mli))
;; but :structures have form (A . a.ml)
;; if *mibl-build-dyads*, first emit ocaml_module, then ocaml_signature
(define (-emit-freestanding-module outp ws module pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (bgblue "-emit-freestanding module") module))
  ;; (format #t "~A: ~A~%" (bgyellow "pkg") pkg)
  (let* ((pkg-name (pkg->pkg-name pkg))
         (pkg-prologue-ct (if-let ((pkg-prologue (assoc-val :prologues pkg)))
                                   (length pkg-prologue) 0))

         ;; (stanza-alist (cdr stanza))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A\n" (uwhite "stanza-alist") stanza-alist)))

         (shared-ppx (if-let ((shppx (assoc-in '(:mibl :shared-ppx) pkg)))
                             (cadr shppx) #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (bgyellow "shared-ppx") shared-ppx)))

         ;; (privname (if-let ((privname (assoc-val :privname stanza-alist)))
         ;;                   privname
         ;;                   #f))

         ;; (local-deps (if (proper-list? module)
         ;;                 (if (alist? (cdr module))
         ;;                     ;; (A (:ml a.ml) (:mli a.mli) (:ml-deps...) (:mli-deps...))
         ;;                     (let* ((ml-locals (if-let ((locals (assoc :ml-deps (cdr module))))
         ;;                                               (cdr locals)
         ;;                                               '()))
         ;;                            (mli-locals (if-let ((locals (assoc :mli-deps (cdr module))))
         ;;                                                (cdr locals)
         ;;                                                '()))
         ;;                            (locals (concatenate ml-locals mli-locals)))
         ;;                       (remove-duplicates locals))
         ;;                     ;; else (A a.ml Foo Bar ...)
         ;;                     (cdr module))
         ;;                 ;; else (A . a.ml) from :structures
         ;;                 ;; should not happen?
         ;;                 (cdr module)))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "Local-deps") local-deps)))
         )
    (let-values (((ml-local-deps mli-local-deps) (module->bazel-local-deps (car module) pkg)))
      (mibl-trace "ml-local-deps" ml-local-deps)
      (mibl-trace "mli-local-deps" mli-local-deps)

      (if (alist? (cdr module))
          ;; proper alist (A (:ml a.ml)(:mli a.mli)) (or :ml_, :mli_)
          (let* ((_ (if (or *mibl-debug-emit* *mibl-debug-s7*)
                        (format #t "~A: ~A~%" (green "emitting freestanding module (proper assoc-list)") module)))
                 (modname (car module))
                 (srcs    (cdr module))
                 (select-sigfile #f)
                 ;; (select-sigfile (assoc-val :mli_ srcs))
                 ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "select-sigfile") select-sigfile)))
                 ;; (sigfile (if select-sigfile
                 ;;              (make-selector module stanza)
                 ;;              (assoc-val :mli srcs)))
                 (select-structfile #f) ;; (assoc-val :ml_ srcs))
                 ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "select-structfile") select-structfile)))
                 ;; (structfile (if select-structfile
                 ;;                 (make-selector module stanza)
                 ;;                 (assoc-val :ml srcs)))
                 ;; ;; for case deps/dynamic
                 ;; (structfile (if structfile structfile
                 ;;                 (assoc-val :ml_ srcs)))

                 (sigfile (if-let ((mli (assoc-val :mli srcs)))
                                  mli
                                  (assoc-val :mli_ srcs)))
                 (structfile (if-let ((mli (assoc-val :ml srcs)))
                                     (car mli)
                                     (assoc-val :ml_ srcs)))
                 )
            ;; (error 'STOP "STOP selmod")
            ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
            ;;         ;;               (cdr opts) '())))
            ;; (if (or *mibl-debug-emit* *mibl-debug-s7*)
            ;;     (begin
            ;;       (format #t "emitting module (proper): ~A: ~A\n" modname srcs)))

            (format outp "01: ocaml_module(\n")
            (format outp "    name          = \"~A\",\n" modname)
            (format outp "    struct        = \"~A\",~%" structfile)
            (if *mibl-build-dyads*
                (format outp "    sig           = \":~A_cmi\",\n" modname))

            (format outp "    deps          = \"~A\",~%" local-deps)

            (format outp ")\n")
            (newline outp))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; proper list but not alist: (Mytest mytest.ml Hello) - no sig
          (let* ((_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "emitting module (proper list): ~A\n" module)))
                 (modname (car module))
                 (structfile (if (proper-list? module) (cadr module)  (cadr module)))
                 (local-deps (if (proper-list? module) (cddr module) '())))

            (format outp "02: ocaml_module(\n")
            (format outp "    name          = \"~A\",\n" modname)
            (format outp "    struct        = \"~A\",\n" structfile)
            (format outp "    deps          = \"~A\",~%" local-deps)
            ;; (emit-deps outp this-is-main prologue? deps-tag
            ;;             ;; stanza
            ;;             agg-deps local-deps dep-selectors testsuite)
            (format outp ")")
            (newline outp)))
      ))

  (if *mibl-build-dyads*
      (if (alist? (cdr module))
          (if (assoc :mli (cdr module))
              ;;FIXME
              (-emit-sig-freestanding outp ws module))))
  )

(define (-emit-dyadic-module outp ws module pkg stanza ns
                             target-selector src-selectors src-default-apodosis
                             opts opts-tag prologue? deps-tag
                             agg-deps local-deps dep-selectors testsuite
                             shared-ppx ppx-codeps ppx-alist ppx-label ppx-id ppx-args)
  (mibl-trace-entry "-emit-dyadic-module" module *mibl-debug-emit*)
  (let* ((stanza-alist (cdr stanza))
         (modname (car module))
         (mibl-trace-let "modname" modname)
         (srcs    (cdr module))
         (select-sigfile #f)
         ;; (select-sigfile (assoc-val :mli_ srcs))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "select-sigfile") select-sigfile)))
         ;; (sigfile (if select-sigfile
         ;;              (make-selector module stanza)
         ;;              (assoc-val :mli srcs)))
         (select-structfile #f) ;; (assoc-val :ml_ srcs))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "select-structfile") select-structfile)))
         ;; (structfile (if select-structfile
         ;;                 (make-selector module stanza)
         ;;                 (assoc-val :ml srcs)))
         ;; ;; for case deps/dynamic
         ;; (structfile (if structfile structfile
         ;;                 (assoc-val :ml_ srcs)))

         (sigfile (if-let ((mli (assoc-val :mli srcs)))
                          (if (list? mli)
                              (car mli) mli)
                          (assoc-val :mli_ srcs)))
         (structfile (if-let ((ml (assoc-val :ml srcs)))
                             (if (list? ml) (car ml) ml)
                             (car (assoc-val :ml_ srcs))))
         )
    (mibl-trace "XXXXXXXXXXXXXXXX" "")
    ;; (error 'STOP "STOP selmod")
    ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
    ;;         ;;               (cdr opts) '())))
    (mibl-trace "srcs" srcs)
    (mibl-trace "structfile" structfile)
    (mibl-trace "sigfile" sigfile)
    ;; (format outp "## this-is-main ~A~%" this-is-main)
    (if (or (assoc :ppx-rewriter stanza-alist)
            (assoc :ppx-deriver stanza-alist)
            ppx-codeps)
        (format outp "ppx_module(\n")
        (format outp "ocaml_module(\n"))
    (format outp "    name          = \"~A\",\n" modname)
    (if (and ns (not *mibl-ns-topdown*))
        (format outp "    ns_resolver   = \":ns.~A\",\n" ns))

    (if (or select-structfile select-sigfile)
        (format outp "    module        = \"~A\",\n" modname))

    (if (or *mibl-debug-emit* *mibl-debug-s7*)
        (if (truthy? target-selector)
            (begin
              (format #t "~A: ~A~%" (bgred "IS TGT MLFILE?") target-selector)
              (format #t "~A: ~A~%" (bgred "TARGET?") (assoc-val :target target-selector))
              ))_)

    (if (truthy? target-selector)
        (if (eq? (fnmatch "*.ml" (format #f "~A" (assoc-val :target target-selector)) 0) 0)
            (begin
              (if (or *mibl-debug-emit* *mibl-debug-s7*)
                  (format #t "~A: ~A~%" (uwhite "src-selectors")
                          src-selectors))
              (format outp "    struct        = select({~%")
              (format outp "~{        \"//bzl/import:~A?\": \"~A\",~^~%~}~%"
                      src-selectors)
              (format outp "        \"//conditions:default\": \"~A\"~%"
                      src-default-apodosis)
              ;; (format outp "        }, no_match_error=\"no file selected\"\n")
              (format outp "    }),~%")

              ))
        ;; else
        (begin
          (format outp "    struct        = \"~A\",~%" structfile)
          ))

    (if *mibl-build-dyads*
        (format outp "    sig           = \":~A_cmi\",\n" modname)
        ;; else
        (if select-sigfile
            (begin
              (format outp "    sig           = select(~%")
              (format outp "        {~%")
              (format outp "~{~{~8T~S: \"~S\",~%~}~}" sigfile)
              (format outp "        },~%")
              (format outp "        no_match_error=\"no file selected\"),\n")
              )
            ;; else
            (begin
              (format outp "    sig           = \"~A\",\n" sigfile)
              )))

    ;; (format outp "    ## sig      = \":~A_cmi\",\n" modname)
    (if opts ;; (not (null? opts))
        (if prologue?
            ;; should not happen?
            (format outp "    opts          = OPTS_~A, ## X0\n" opts-tag)
            ;; else no main-module, no prologue
            (format outp "    opts          = OPTS_~A,\n" opts-tag))
        )

    (mibl-trace "emitting deps A" deps-tag *mibl-debug-emit*)
    (emit-deps outp
                #f ;; this-is-main
                prologue? deps-tag
                ;;stanza
                agg-deps local-deps dep-selectors testsuite)

    ;; (if (not (null? local-deps))
    ;;     (format outp "    local-deps          = ~A,\n" local-deps))

    (if ppx-label
        (begin
          (mibl-trace "emitting ppx" ppx-alist *mibl-debug-emit*)
          (format outp "01:    ppx           = \"~A\",\n" ppx-label)
          ;;FIXME: handle :scope
          (if (truthy? ppx-args)
              (begin
                (mibl-trace "emitting ppx-args" ppx-args *mibl-debug-emit*)
                (format outp "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args)))))

              ;; (let* ((mod (assoc-in `(:modules ,modname) pkg))
              ;;        (mfile (car (assoc-val :ml (cdr mod))))
              ;;        )
              ;;   (format outp
              ;;           "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n"
              ;;           (append (list "-loc-filename"
              ;;                         (format #f "~A" mfile))
              ;;                   ppx-args))))))

    ;; (if (truthy? ppx-alist)
    ;;     (begin
    ;;       (if (> (length shared-ppx) 1)
    ;;           (format outp "A    ppx           = \"~A:Appx_~A.exe\",\n" ;; #X2
    ;;                   ppx-pkg ppx-id)
    ;;           (format outp "B    ppx           = \"~A:ppx.exe\",\n" ppx-pkg))
    ;;       ;; ppx-name)
    ;;       ;; (cadr (assoc :name ppx-alist)))
    ;;       ;; (if (not
    ;;       ;;      (equal? :all (cadr (assoc :scope
    ;;       ;;                                ppx-alist))))

    ;;       (mibl-trace "emitting ppx-args" ppx-args *mibl-debug-emit*)
    ;;       ;;FIXME: handle :scope
    ;;       (if (truthy? ppx-args)
    ;;           (let* ((mod (assoc-in `(:modules ,modname) pkg))
    ;;                  (mfile (car (assoc-val :ml (cdr mod))))
    ;;                  )
    ;;             (format outp
    ;;                     "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n"
    ;;                     (append (list "-loc-filename"
    ;;                                   (format #f "~A" mfile))
    ;;                             ppx-args))))
    ;;       ;; (if-let ((codeps (assoc-val :ppx-codeps stanza-alist)))
    ;;       ;;         (format outp "    ppx_codeps    = [~{\"~S\"~^, ~}],\n" codeps))
    ;;       ))

    (mibl-trace "emitting ppx-codeps" ppx-codeps *mibl-debug-emit*)
    (if ppx-codeps
        (format outp "    ppx_codeps    = [~{\"~A\"~^, ~}]\n" ppx-codeps))

    (format outp ")\n")
    (newline outp)

    ;; emit sig
    (if *mibl-build-dyads*
        (-emit-sig outp ws pkg module stanza-alist))
    )
  )

;;FIXME: rename to -emit-struct-module
(define (-emit-improper-module outp module pkg
                               stanza  ;; the aggregator
                               ns
                               target-selector src-selectors src-default-apodosis
                               opts opts-tag prologue? deps-tag
                               agg-deps local-deps dep-selectors testsuite
                               shared-ppx ppx-codeps ppx-alist ppx-label ppx-id ppx-args)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (ublue "-emit-improper-module") module))
  (let* ((modname (car module))
         (structfile (if (proper-list? module) (cadr module)  (cdr module)))
         ;; (local-deps (if (proper-list? module) (cddr module) '()))
         (ns (case (car stanza)
               ((:executable)
                (let ((main (assoc-val :main (cdr stanza))))
                  (if (member modname (assoc-val :prologue (cdr stanza)))
                      ;; (if *mibl-executable-ns-split*
                      ;;     (format #f "nsPrologue_~A" main)
                          (format #f "Exe_ns")
                          ;; )
                      (if (member modname (assoc-val :main-deps (cdr stanza)))
                          ;;(format #f "ns.~A" main)
                          (format #f "Exe_ns")
                          #f))))
               ((:ns-archive :ns-library)
                ;; only for bottom-up?
                #f)
               (else #f))))
               ;; (derive-exe-ns pkg module)))

    (let-values (((ml-local-deps mli-local-deps) (module->bazel-local-deps (car module) pkg)))
      (mibl-trace "ml-local-deps" ml-local-deps)
      (mibl-trace "mli-local-deps" mli-local-deps)

      (format #t "agg kind: ~A\n" stanza)

      ;; ns:
      ;; if aggregator is :executable
      ;;     if module is in :main-deps of aggregator use exe ns
      ;;     if module is in :prologue of aggregator use local-prologue ns
      ;; if is namespaced use it

      ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
      ;;               (cdr opts) '())))

      (if ppx-codeps
          (format outp "ppx_module(\n")
          ;; (if this-is-main
          ;;     (format outp "ocaml_exec_module(\n")
          (format outp "ocaml_module(\n"))

      (if ns
          (format outp "    ns_resolver   = \":~A\",\n" ns))

      (format outp "    name          = \"~A\",\n" modname)

      ;; (if (and ns (not *mibl-ns-topdown*))
      ;;     (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
      ;; (format outp "    struct        = \"~A\",\n" structfile)

      (if (truthy? target-selector)
          (if (eq? (fnmatch "*.ml" (format #f "~A" (assoc-val :target target-selector)) 0) 0)
              (begin
                (if (or *mibl-debug-emit* *mibl-debug-s7*)
                    (format #t "~A: ~A~%" (uwhite "src-selectors") src-selectors))
                (format outp "    struct        = select({~%")
                (format outp "~{        \"//bzl/import:~A?\": \"~A\",~^~%~}~%"
                        src-selectors)
                (format outp "        \"//conditions:default\": \"~A\"~%"
                        src-default-apodosis)
                ;; (format outp "        }, no_match_error=\"no file selected\"\n")
                (format outp "    }),~%")

                ))
          (begin
            (format outp "    struct        = \"~A\",~%" structfile)
            ))
      (if (or *mibl-debug-emit* *mibl-debug-s7*)
          (begin
            (format #t "~A: ~A~%" (umagenta "emitting opts") opts)
            (format #t "~A: ~A~%" (umagenta "prologue?") prologue?)))
      (if opts ;; (not (null? opts))
          ;; (if this-is-main
          ;;     (if prologue?
          ;;         (format outp "    opts          = [\"-open\", \"~A\"] + OPTS_~A,\n"
          ;;                 (normalize-module-name prologue?) opts-tag)
          ;;         (format outp "    opts          = OPTS_~A,\n" opts-tag))
          ;; (if prologue?
          ;;     ;; should not happen?
          ;;     (format outp "    opts          = OPTS_~A, ## X3\n" opts-tag)
          ;;     ;; else no main-module, no prologue
          ;;     (format outp "    opts          = OPTS_~A,\n" opts-tag))
          ;; )
          ;; else no opts
          ;; (if this-is-main
          ;;     (if prologue?
          ;;         (format outp "    opts          = [\"-open\", \"~A\"],~%" prologue?) ;; this-is-main)
          ;;         ;; else should not happen
          ;;         ;; (format outp "    opts          = OPTS_~A,\n" opts-tag))
          ;;         )
          ;; no opts, no main-module
          (if prologue?
              ;; should not happen?
              (format outp "    opts          = [\"-open\", \"~A\"], ## X4\n" prologue?)
              ;; else no opts, no main-module, no prologue
              )
          ;; )
          )

      (if (or *mibl-debug-emit* *mibl-debug-s7*)
          (format #t "~A: ~A~%" (blue "emitting deps B") deps-tag))
      (emit-deps outp
                  #f ;; this-is-main
                  prologue? deps-tag
                  ;; stanza
                  agg-deps ml-local-deps dep-selectors testsuite)
      ;; (format #t "~A: ~A~%" (red "local-deps") local-deps)
      ;; (if (not (null? local-deps))
      ;;     (if (not (null? agg-deps))
      ;;         (begin
      ;;           (if (equal? :executable (car stanza))
      ;;               (format outp "    deps          = ~A_EXE_DEPS + [\n" (if testsuite testsuite libname))
      ;;               (format outp "Z    deps          = ~A_DEPS + [\n" (if testsuite testsuite libname)))
      ;;           (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
      ;;           (format outp "    ],\n"))
      ;;         (begin
      ;;           (format outp "    deps          = [\n")
      ;;           (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
      ;;           (format outp "    ],\n")))
      ;;     ;; else no local deps, maybe agg deps
      ;;     (if (not (null? agg-deps))
      ;;           (if (equal? :executable (car stanza))
      ;;               (format outp "    deps          = ~A_EXE_DEPS,~%" (if testsuite testsuite libname))
      ;;               (format outp "A    deps          = ~A_DEPS,~%" (if testsuite testsuite libname)))))

    (if ppx-label
        (begin
          (mibl-trace "emitting ppx" ppx-alist *mibl-debug-emit*)
          (format outp "    ppx           = \"~A\",\n" ppx-label)
          (mibl-trace "emitting ppx-args" ppx-args *mibl-debug-emit*)
          ;;FIXME: handle :scope
          (if (truthy? ppx-args)
              (format outp "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args))))

              ;; (let* ((mod (assoc-in `(:modules ,modname) pkg))
              ;;        (mfile (car (assoc-val :ml (cdr mod))))
              ;;        )
              ;;   (format outp
              ;;           "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n"
              ;;           (append (list "-loc-filename"
              ;;                         (format #f "~A" mfile))
              ;;                   ppx-args))))))

      ;; (if ppx-alist
      ;;     (begin
      ;;       (if (> (length shared-ppx) 1)
      ;;           (format outp "C    ppx           = \"~A:ppx_~A.exe\",\n" ppx-pkg ppx-id)
      ;;           (format outp "D    ppx           = \"~A:ppx.exe\",\n" ppx-pkg))

      ;;       (if ppx-args
      ;;           (begin
      ;;             ;; (format #t "~A: ~A~%" "pkg" pkg)
      ;;             ;; (format #t "~A: ~A~%" "modname" modname)
      ;;             ;; (format outp
      ;;             ;;       "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args)
      ;;             (let* ((mod (assoc-in `(:modules ,modname) pkg))
      ;;                    (mfile (if mod
      ;;                               (car (assoc-val :ml (cdr mod)))
      ;;                               (let* ((mstruct (assoc-in `(:structures :static ,modname) pkg)))
      ;;                                 (cadr mstruct))))
      ;;                    )
      ;;               (format outp
      ;;                       "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n"
      ;;                       (append (list "-loc-filename"
      ;;                                     (format #f "~A" mfile))
      ;;                               ppx-args))))
      ;;           )
      ;;       ;; (if ppx-args
      ;;       ;; ;; (if (not  ## why?
      ;;       ;; ;;      (equal? :all (cadr (assoc :scope
      ;;       ;; ;;                                ppx-alist))))
      ;;       ;;     (format outp
      ;;       ;;             "    ppx_args      = [~{~S, ~}], #A1\n" ppx-args
      ;;       ;;             ;;(cadr (assoc :args ppx-alist))
      ;;       ;;             ))
      ;;       )) ;; ppx-alist

      (if ppx-codeps
          (format outp "    ppx_codeps    = [~{\"~S\"~^, ~}]\n" ppx-codeps))

      ;; (if prologue?
      ;;     (format outp "    visibility    = [\"//visibility:private\"]~%"))

      (format outp ")\n")
      ))
  )

(define (-emit-module outp ws module stanza pkg)
  (mibl-trace-entry "-emit-module" module)
  (mibl-trace "stanza" stanza)
  (mibl-trace "pkg" pkg)

  (let* ((dyadic-module? (if (alist? (cdr module)) #t #f))
          ;; (if (or (assoc :mli (cdr module))
          ;;                        (assoc :mli_ (cdr module)))
          ;;                    #t #f))
         (mibl-trace-let "dyadic?" dyadic-module?)

         (pkg-name (pkg->pkg-name pkg))
         ;; (_ (format #t "~A: ~A~%" (bgyellow "pkg-name") pkg-name))

         (stanza-alist (cdr stanza))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A\n" (uwhite "stanza-alist") stanza-alist)))

         (pkg-shared-ppx-alist (if-let ((shppx (assoc-in '(:mibl :shared-ppx) pkg)))
                             (cadr shppx) #f))
         (mibl-trace-let "pkg-shared-ppx-alist" pkg-shared-ppx-alist *mibl-debug-emit*)

         (privname (if-let ((privname (assoc-val :privname stanza-alist)))
                           privname
                           #f))

         (pkg-prologue-ct (if-let ((pkg-prologue (assoc-val :prologues pkg)))
                                   (length pkg-prologue) 0))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (bggreen "pkg-prologue ct") pkg-prologue-ct)))

         ;; if this module is mbr of a prologue, apply its :opts (using global var?)
         (prologue? (if-let ((prologue (assoc-val :prologue stanza-alist)))
                            (let ((m (normalize-module-name pkg-name)))
                              (if (> pkg-prologue-ct 0)
                                  (format #f "~A_execlib_~A" m prologue)
                                  (format #f "~A_execlib" m)))
                            #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (bggreen "prologue") prologue?)))

         (libname (if privname
                      (string-append
                       (string-upcase (stringify privname)))
                      ;; e.g. for (:ocamlc ) stanza
                      #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "em libname: ~A~%" libname)))

         (testsuite (if-let ((ts (assoc-val :in-testsuite (cdr stanza))))
                            (string-upcase (format #f "~A" ts)) #f))

         (ns (assoc-val :ns stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "em ns: ~A~%" ns)))

         (deps-tag (if-let ((shared (assoc-in '(:deps :resolved) stanza-alist)))
                           (if (number? (cdr shared)) (cdr shared) libname)
                           libname))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "deps-tag") deps-tag)))

         (agg-deps (if-let ((deps (assoc-val :deps stanza-alist)))
                           (dissoc '(:conditionals :seldeps) deps)
                       ;; else :executable or :test
                       ;; FIXME: should not be any deps in :compile ?
                       (if-let ((deps (assoc-in '(:compile :deps)
                                                stanza-alist)))
                               deps
                               '())))
         (agg-deps (if (number? deps-tag)
                       (dissoc '(:deps :resolved) agg-deps)
                       agg-deps))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "Agg-deps") agg-deps)))

         ;; (_ (if (equal? (car module) 'Ocaml_protoc_cmdline)
         ;;        (begin
         ;;          (format #t "~A: ~A~%" (uwhite "aggregtor") stanza)
         ;;          (error 'STOP "STOP emit-module"))))

         (deps-conditional (assoc-in '(:deps :conditionals) stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional)))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "module") module)))
         (target-selector
          (if deps-conditional
              ;;FIXME: deps-conditional may have more than one entry
              (let ((x
                     (find-then (lambda (conditional)
                                  (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                      (format #t "~A: ~A~%" (bgred "conditional") conditional))
                                  (let ((ctarget (assoc-val :target conditional)))
                                    (if (alist? (cdr module))
                                        (find-then (lambda (msrc)
                                                     (if (equal? ctarget (cdr msrc))
                                                         conditional #f))
                                                 (cdr module))
                                        (if (equal? ctarget (cdr module))
                                            conditional #f))))
                                (cdr deps-conditional))))
                x)
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "target-selector") target-selector)))

         (src-selectors
          (if target-selector
              (flatten
               (map
                (lambda (sel)
                  (cons (car sel)
                        (cadr sel)))
                (assoc-val :selectors target-selector)))
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "src-selectors") src-selectors)))

         (dep-selectors
          (if target-selector
              (map
               (lambda (sel)
                 (let* ((protasis (car sel))
                        (apodosis (last sel)))
                 (list protasis apodosis)))
               (assoc-val :selectors target-selector))
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "dep-selectors") dep-selectors)))

         (src-default-apodosis
           (if target-selector
               (assoc-val :default target-selector)
               #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "src-default-apodosis") src-default-apodosis)))

         ;; (opts (or (assoc :compile-opts stanza-alist)
         ;;           (assoc :ocamlc-opts stanza-alist)
         ;;           (assoc :ocamlopt-opts stanza-alist)))
         (opts (if-let ((opts (assoc-val :opts stanza-alist)))
                       opts #f))
                       ;; (if (number? opts)
                       ;;     opts
                       ;;     (begin
                       ;;       opts))
                       ;; ;; else executable
                       ;; (if-let ((opts (assoc-in '(:compile :opts)
                       ;;                          stanza-alist)))
                       ;;         opts
                       ;;         #f)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "OPTS") opts)))

         (opts-tag (if (number? opts) opts libname))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ugreen "opts-tag") opts-tag)))

         ;; (ocamlc_opts (if opts ;; (null? opts) '()
         ;;                  (if-let ((x (assoc-val :ocamlc (cdr opts))))
         ;;                          (list (apply string-append
         ;;                                       (map stringify x)))
         ;;                          '())
         ;;                  '()))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "ocamlc_opts") ocamlc_opts)))

         ;; (ocamlopt_opts (if opts ;; (null? opts) '()
         ;;                    (if-let ((flags (assoc-val :ocamlopt (cdr opts))))
         ;;                            (list (apply string-append
         ;;                                         (map stringify flags)))
         ;;                            '())
         ;;                    ()))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "ocamlopt_opts") ocamlopt_opts)))

         ;; we always use shared ppxes (pkg scope), so we have e.g. (:ppx . 1)
         ;; lookup ppx-alist in :shared-ppx
         ;; (ppx-id (get-ppx-id ws (cdr stanza)))
         (ppx-id (if-let ((ppx (assoc-val :ppx stanza-alist))) ppx #f))
         (mibl-trace-let "module ppx-id" ppx-id *mibl-debug-emit*)

         ;; for now ppx-id is sufficient to construct label;
         ;; when we add ws :shared-ppx we will also need the shared-ppx package
         (ppx-label (if ppx-id
                        (if (> (length pkg-shared-ppx-alist) 1)
                            (format #f ":ppx_~A.exe" ppx-id)
                            (format #f ":ppx.exe"))
                        #f))
         (mibl-trace-let "module ppx-label" ppx-label *mibl-debug-emit*)

         ;; we need the ppx alist so we can extract ppx-args
         (ppx-alist (if ppx-id
                        (let ((x (assoc-val ppx-id pkg-shared-ppx-alist)))
                          (if (number? x)
                              (begin ;; look it up in ws :shared-ppx
                                (error 'WS-SHARED-PPX "Not yet implmented: global :shared-ppx"))
                              x))
                          #f))
         (mibl-trace-let "ppx-alist" ppx-alist *mibl-debug-emit*)

         (ppx-args (if ppx-id (-get-ppx-args ppx-alist libname) #f))
         (mibl-trace-let "ppx-args" ppx-args *mibl-debug-emit*)

         (ppx-codeps (if-let ((codeps (assoc-val :ppx-codeps stanza-alist))) codeps #f))
         (mibl-trace-let "ppx-codeps" ppx-codeps *mibl-debug-emit*)

         ;; (ppx-pkg (if *mibl-local-ppx-driver* "" (format #f "//~A" *mibl-shared-ppx-pkg*)))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "ppx-pkg") ppx-pkg)))
         )

    (let-values (((ml-local-deps mli-local-deps) (module->bazel-local-deps (car module) pkg)))
      (mibl-trace "ml-local-deps" ml-local-deps)
      (mibl-trace "mli-local-deps" mli-local-deps)

      ;; (_ (error 'x "X"))

      (if (or *mibl-debug-emit* *mibl-debug-s7*)
          (begin
            (format #t "~A: ~A~%" (uwhite "ppx id") ppx-id)
            (format #t "module libname: ~A~%" libname)
            (format #t "module ns: ~A~%" ns)))
      ;; (if ppx-id (error 'stop "STOP ppx id"))

      ;; FIXME: if *mibl-build-dyads* then emit both ocaml_module and ocaml_signature

      ;; (if (proper-list? module)
      (if (alist? (cdr module))
          ;; proper alist (A (:ml a.ml)(:mli a.mli)) (or :ml_, :mli_)
          (-emit-dyadic-module outp ws module pkg stanza ns
                               target-selector src-selectors src-default-apodosis
                               opts opts-tag prologue? deps-tag
                               agg-deps ml-local-deps dep-selectors testsuite
                               pkg-shared-ppx-alist ppx-codeps ppx-alist ppx-label ppx-id ppx-args)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; proper list but not alist: (Mytest mytest.ml Hello) - no sig
          (-emit-improper-module outp module pkg stanza ns
                                 target-selector src-selectors src-default-apodosis
                                 opts opts-tag prologue? deps-tag
                                 agg-deps ml-local-deps dep-selectors testsuite
                                 pkg-shared-ppx-alist ppx-codeps ppx-alist ppx-label ppx-id ppx-args)

          )))
  ;; FIXME: move to individual emit-X-module routines
  ;; (if *mibl-build-dyads*
  ;;     (if (alist? (cdr module))
  ;;         (if (assoc :mli (cdr module))
  ;;             (-emit-sig outp ws pkg module stanza))))
  stanza)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-emit-modules outp ws pkg modules)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (bgblue "-emit-modules") modules))

  (for-each
   (lambda (module)
     (if (or *mibl-debug-emit* *mibl-debug-s7*)
         (format #t "~%~A: ~A  ~A\n" (ublue "next module")
                 (ured (car module)) module))
     ;; (flush-output-port)

     ;; first find aggregate for this module
     (let* ((modname (car module))
            ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "prologue") prologue)))
            (aggregator (find-if
                         (lambda (stanza)
                           (if (or *mibl-debug-emit* *mibl-debug-s7*)
                               (format #t "~A: ~A~%" (uwhite "searching stanza:") stanza))
                           (begin
                             ;; (format #t "~A: ~A~%" ( "prologue") prologue)
                             (case (car stanza)
                               ((:ns-archive :ns-library)
                                (if-let ((submods
                                          (cdr (assoc-in '(:manifest :modules)
                                                         (cdr stanza)))))
                                        (begin
                                          (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                              (format #t "submods: ~A\n" submods))
                                          (if (member modname submods)
                                              ;;(-emit-module outp ws module stanza)
                                              #t #f))))

                               ((:archive :library)
                                (if-let ((submods
                                          (cdr (assoc-in '(:manifest :modules)
                                                         (cdr stanza)))))
                                        (begin
                                          (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                              (format #t "submods: ~A\n" submods))
                                          (if (member modname submods)
                                              ;;(-emit-module outp ws module stanza)
                                              #t #f))))

                               ((:executable :test)
                                ;; module may be in exec :prologue or shared prologue, or :main-deps
                                (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                    (format #t "~A: ~A for ~A~%" (uwhite "checking :executable") stanza (bgcyan modname)))
                                (if (equal? modname (assoc-val :main (cdr stanza)))
                                    #t
                                    (if-let ((prologue (assoc-val :prologue (cdr stanza))))
                                            (if (number? prologue)
                                                (if (shared-prologue-contains-module? prologue modname pkg)
                                                    #t
                                                    (if-let ((main-deps (assoc-val :main-deps (cdr stanza))))
                                                            (member modname main-deps)
                                                            #f))
                                                (if (member modname prologue)
                                                    #t
                                                    (if-let ((main-deps (assoc-val :main-deps (cdr stanza))))
                                                            (member modname main-deps)
                                                            #f))))))
                                            ;; else check main module deps in pkg files
                                            ;; get main module, search for it in pkg-modules, pkg-structs, check the deps
                                            ;; (let* ((main (assoc-val :main (cdr stanza)))
                                            ;;        (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "checking pkg deps of") main)))
                                            ;;        (mx (if-let ((mdeps (module-name->tagged-label main pkg)))
                                            ;;                    (begin
                                            ;;                      (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                            ;;                          (format #t "~A: ~A~%" (green "found") mdeps))
                                            ;;                      (if (list? (cdr mdeps)) ;; e.g. (Foo foo.ml Bar)
                                            ;;                          (member modname (cddr mdeps))
                                            ;;                          #f)) ;; e.g. (Foo . fool.ml)
                                            ;;                    #f)))
                                            ;;   mx)
                                            ;; )))

                               ((:ocamlc) ;; from (rule (action (run %{bin:ocamlc} ...)))
                                (if-let ((srcs (assoc-val :srcs (cdr stanza))))
                                        (begin
                                          (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                              (format #t "~A: ~A\n" (uwhite "ocamlc srcs") srcs))
                                          (find-if (lambda (src)
                                                     (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                                         (begin
                                                           (format #t "~A: ~A\n" (uwhite "ocamlc src") src)
                                                           (format #t "~A: ~A\n" (uwhite "module file") (cdr module))))
                                                     (string=? (format #f "~A" (assoc-val :tgt (cdr src)))
                                                               (format #f "~A" (cdr module))))
                                                   srcs))))

                               ((:install :lex :yacc :menhir
                                          :cppo :env
                                          :shared-opts :shared-link-opts :shared-ocamlc-opts :shared-ocamlopt-opts
                                          :shared-deps
                                          :diff :alias :node
                                          :write-file
                                          :bindiff-test :diff-test
                                          :sh-test :testsuite :prologues)
                                #f)

                               ((:rule)
                                (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                    (format #t "~A: ~A~%" (bgred "handle :rule for -emit-modules") stanza))
                                #f)

                               (else
                                (error 'UNHANDLED
                                       (format #f "emit-modules, unhandled kind: ~A" stanza))))))
                         (assoc-val :mibl pkg)))
            ) ;; end let bindings

       (if aggregator
           (begin
             (if (or *mibl-debug-emit* *mibl-debug-s7*)
                 (format #t "~A ~A: ~A\n"
                         (uwhite "Found containing aggregator for")
                         modname aggregator))
             (if-let ((main (assoc-val :main (cdr aggregator))))
                     (if (equal? modname main)
                         (begin
                           ;; do not emit here - will be handled by emit-exec routine?
                           ;; (format #t "~A: ~A - not emitting qua module~%" (green "Main exe") modname)
                           )
                         (begin
                           (-emit-module outp ws module aggregator pkg)
                           (format outp "\n"))
                         )
                     ;; else no :main - not an executable
                     (begin
                       (-emit-module outp ws module aggregator pkg)
                       (format outp "\n"))))
           ;; else
           (begin
             (if (or *mibl-debug-emit* *mibl-debug-s7*)
                 (format #t "~A ~A; emitting free-standing target\n"
                         (uwhite "No aggregator found for") modname))
             (-emit-freestanding-module outp ws module pkg)
                       ))
       ))
   ;;(sort! modules (lambda (a b) (sym<? (car a) (car b))))
   modules ; for-each
   ))

;; (define (bazel-emit-singleton-targets outp fs-path stanzas dune-pkg)

(define (bazel-emit-singleton-targets outp ws pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A\n" (blue "bazel-emit-singleton-targets") pkg))

  ;; we emit targets for both static and generated source files; in
  ;; addition, we may have :indirect submodule deps (example:
  ;; src/lib_protocol_environment/sigs)

  ;; FIXME: pkg-modules will not contain modules completed by dune
  ;; select. E.g. js_of_ocaml/compiler/lib:Sourc_map_io, where the .ml
  ;; file is selected.

  ;; FIXME: pkg-structs will contain "selectable" structs, i.e.
  ;; structs listed as apodoses of select fields. TODO: move them from
  ;; (:structures :static) to (:structures :conditional).

  (let* ((pkg-modules (if-let ((ms (assoc-in '(:modules) pkg)))
                              (cdr ms) '()))
         (pkg-structs (pkg->structs pkg))
         ;; (structs-static (if-let ((structs (assoc-in
         ;;                                    '(:structures :static) pkg)))
         ;;                         (cdr structs) '()))
         ;; (pkg-modules (concatenate modules-static pkg-structs)) ;; structs-static))

         (pkg-sigs (pkg->sigs pkg))
         ;; (sigs-static (if-let ((sigs (assoc-in
         ;;                              '(:signatures :static) pkg)))
         ;;                      (cdr sigs) '()))
         ;; (sigs sigs-static))
         )

    (if (or *mibl-debug-emit* *mibl-debug-s7*)
        (begin
          (format #t "~A: ~A\n" (ucyan "pkg-modules") pkg-modules)
          (format #t "~A: ~A\n" (ucyan "pkg-structs") pkg-structs)
          (format #t "~A: ~A\n" (ucyan "pkg-sigs") pkg-sigs)))

    (if (or (not (null? pkg-modules))
            (not (null? pkg-structs))
            (not (null? pkg-sigs)))
        (begin
          (format outp "######################## Modules & Signatures ########################")
          (newline outp)))

    (if pkg-modules
        (begin
          (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (bgblue "emitting pkg-modules") pkg-modules))
          (-emit-modules outp ws pkg pkg-modules)))

    (if pkg-structs
        (begin
          (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A~%" (bgblue "emitting pkg-structs")))
          (-emit-modules outp ws pkg pkg-structs)))

    ;; (if (equal? (car pkg :ocamlc))
    ;;       (-emit-modules outp ws pkg pkg-structs)))

    (if pkg-sigs ;;(or pkg-sigs *mibl-build-dyads*)
        (begin
          (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (bgblue "emitting pkg-sigs") pkg-sigs))
          (-emit-signatures outp ws pkg pkg-sigs pkg-modules)))
    ))

(if (or *mibl-debug-emit* *mibl-debug-s7*)
    (format #t "loaded bazel/singletons.scm\n"))
