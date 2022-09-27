(format #t "loading starlark/singletons.scm\n")

;; FIXME FIXME FIXME FIXME FIXME
;; lookup label for this sym
(define (-make-select-condition sym)
  (format #f "//foo/bar:~A" sym))

(define (make-selector module stanza)
  (format #t "~A: ~A, ~A\n" (ublue "make-selector") module stanza)
  (let* ((module-name (car module))
         (filename (let ((pairs (cdr module)))
                     (if-let ((pr (assoc-val :ml_ pairs)))
                             pr
                             (if-let ((pr (assoc-val :mli_ pairs)))
                                     pr
                                     (error 'bad-module
                                            "no generated file")))))

         (_ (format #t "filename: ~A\n" filename))
         (conditionals (assoc-in '(:compile :deps :conditionals) (cdr stanza))))
    (format #t "conditionals: ~A\n" conditionals)
    (let ((the-conditional (find-if (lambda (c)
                                      (format #t "c: ~A\n" c)
                                      (eq? filename
                                           (assoc-val :target c)))
                                    (cdr conditionals))))
      (format #t "the-conditional: ~A\n" the-conditional)
      (error 'stop "STOP make-selector")

      (if the-conditional
          (let ((selectors (assoc-val :selectors the-conditional)))
            (format #t "selectors: ~A\n" selectors)
            (map (lambda (s) (list
                              (-make-select-condition (car s))
                              (cdr s)))
                 selectors))))))

(define (-emit-deps outp this-is-main exec-lib deps-tag stanza agg-deps local-deps selectors testsuite)
  (format #t "~A: ~A~%" (ublue "-emit-deps") deps-tag)
  (format #t "~A: ~A~%" (uwhite "agg-deps") agg-deps)
  (format #t "~A: ~A~%" (uwhite "local-deps") local-deps)
  (format #t "~A: ~A~%" (uwhite "this-is-main") this-is-main)
  (format #t "~A: ~A~%" (uwhite "exec-lib") exec-lib)
  (if (or (number? deps-tag)
          (truthy? local-deps)
          (truthy? agg-deps)
          (and this-is-main (truthy? exec-lib))
          selectors)
      (format outp "    deps          = ")
      )

  (if (and this-is-main (truthy? exec-lib))
      (begin
        (format outp "[\":~A\"]" exec-lib))
      ;; NB: omit trailing comma (and newline) in case select follows
      (if (null? local-deps)
          (if (null? agg-deps)
              (if (number? deps-tag)
                  (format outp "DEPS_~A" (if testsuite testsuite deps-tag))
                  ;; (error 'FIXME
                  ;;        (format
                  ;;         #f "found non-numeric deps-tag ~A but no deps"
                  ;;         deps-tag))
                  ;; else emit nothing
                  )
              ;; agg-deps e.g. (:deps (:resolved @ocaml//compiler-libs/common))
              (if (number? deps-tag)
                  (error 'FIXME
                         (format
                          #f "found both numeric deps-tag ~A and agg-deps ~A"
                          deps-tag agg-deps))
                  (if this-is-main ;; exec-lib
                      (format outp "[\":~A_execlib\"]~%" exec-lib)
                      (format outp "DEPS_~A"
                              (if testsuite testsuite deps-tag)))))
          ;; have local-deps (note: trailing comma+newline added below)
          (if (null? agg-deps)
              (if (number? deps-tag)
                  (if this-is-main ;; exec-lib
                      (format outp "[\":~A_execlib\"]~%" exec-lib)
                      (begin
                        (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                        ;; (format outp "        \":~A_execlib\"~%" exec-lib)
                        (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                        (format outp "    ]")))
                  ;; else
                  (begin
                    ;; (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                    (format outp " [\n")
                    (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                    (format outp "    ]")))
              (begin
                (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                (format outp "    ]")))))

  ;; (if (not (null? local-deps))
  ;;     (if (not (null? agg-deps))
  ;;         (begin
  ;;           (if (equal? :executable (car stanza))
  ;;               (format outp " ~A_EXE_DEPS + [\n" (if testsuite testsuite deps-tag))
  ;;               (format outp "~A_DEPS + [\n" (if testsuite testsuite deps-tag)))
  ;;           (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
  ;;           (format outp "    ]"))
  ;;         (begin
  ;;           (format outp "[~%")
  ;;           (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
  ;;           (format outp "    ]")))
  ;;     ;; else no local-deps
  ;;              ;;     (if (not (null? agg-deps))
  ;;              ;;           (if (equal? :executable (car stanza))
  ;;              ;;               (format outp "    deps          = ~A_EXE_DEPS,~%" (if testsuite testsuite deps-tag))
  ;;              ;;               (format outp "A    deps          = ~A_DEPS,~%" (if testsuite testsuite deps-tag)))))
  ;;     (if (not (null? agg-deps))
  ;;         (format outp "~A_DEPS" (if testsuite testsuite deps-tag))))

  (if selectors
      (begin
        (format #t "~A: ~A~%" (uwhite "emitting selectors") selectors)
        (if (or (truthy? local-deps)
                (truthy? agg-deps)
                (truthy? deps-tag))
            (format outp " + "))
        (format outp " select({~%")
        ;; (format outp "~{        \"//bzl/import:~A?\": \"~A\",~^~%~}~%"
        ;;         src-selectors)
        (format outp "~{        \"//bzl/import:~A?\": [\"~A\"],~^~%~}~%"
                (flatten selectors))
        (format outp "        \"//conditions:default\": []~%")
        (format outp "    }),  ## ~%"))

      ;; else no selectors, finish with comma
      (if (or (number? deps-tag)
              (truthy? local-deps)
              (truthy? agg-deps)
              (and this-is-main (truthy? exec-lib)))
              ;; selectors)
          (format outp ",~%"))
      ))

(define (-get-ppx-args ppx-alist libname)
  (format #t "~A: ~A~%" (ublue "-get-ppx-args") ppx-alist)
  ;; opts come in pairs
  (let* ((opts (assoc-val :opts ppx-alist))
         (_ (format #t "~A: ~A~%" (uyellow "ppx opts") opts))
         ;; (flags (assoc-in '(:runtime-opts :flags) ppx-alist))
         ;; (_ (format #t "~A: ~A~%" (uyellow "ppx flags") flags))
         ;; (options (assoc-in '(:runtime-opts :options) ppx-alist))
         ;; (_ (format #t "~A: ~A~%" (uyellow "ppx options") options))
         (ppx-args (if opts
                       (map (lambda (opt)
                              (format #t "~A: ~A~%" (uyellow "opt") opt)
                              (case opt
                               (($LIBNAME) libname)
                               (else opt)))
                           (flatten opts))
                      #f)))
         ;; (_ (format #t "~A: ~A~%" (uyellow "ppx options") options)))
         ;; (ppx-args (append (if flags (cdr flags) '())
         ;;                   (if options options '()))))
    (format #t "~A: ~A~%" (bgyellow "ppx args") ppx-args)
    ppx-args))

;; WARNING: :modules have form (A (:ml a.ml)(:mli a.mli))
;; but :structures have form (A . a.ml)
(define (-emit-module outp ws module stanza pkg)
  (format #t "~A: ~A [~A]\n" (bgblue "-emit-module") module stanza)
  ;; (format #t "~A: ~A~%" (bgyellow "pkg") pkg)
  (let* ((pkg-name (pkg->pkg-name pkg))
         (pkg-exec-libs-ct (if-let ((pkg-exec-libs (assoc-val :exec-libs pkg)))
                                   (length pkg-exec-libs) 0))

         (stanza-alist (cdr stanza))
         (_ (format #t "~A: ~A\n" (uwhite "stanza-alist") stanza-alist))

         (shared-ppx (if-let ((shppx (assoc-in '(:dune :shared-ppx) pkg)))
                             (cadr shppx) #f))
         (_ (format #t "~A: ~A~%" (bgyellow "shared-ppx") shared-ppx))

         (privname (if-let ((privname (assoc-val :privname stanza-alist)))
                           privname
                           #f))

         ;; executable's main may be a module depending on an execlib,
         ;; or an execlib containing the main module. this is
         ;; controlled by global flag *dune-execlib-includes-main*
         ;; if it is a module, that module must depend on the execlib and also
         ;; -open it.
         ;; multiple executables may share the same execlib

         ;; for each module: if it has an execlib, then we construct
         ;; the -open module name from the execlib, and we list it as
         ;; the sole dep.  otherwise no -open and use the module deps.

         ;; testcase: jsoo/compiler/tests-ocaml/lib-bytes

         ;; is this module the 'main' of an executable?
         (_ (format #t "~A: ~A~%" (green "module") (car module)))
         (this-is-main (if-let ((main (assoc-val :main stanza-alist)))
                               (begin
                                 (format #t "~A: ~A~%" (green "main") main)
                                 (if (string=? (format #f "~A" main) (format #f "~A" (car module)))
                                     main #f))))
         (_ (format #t "~A: ~A~%" (bggreen "this-is-main") this-is-main))

         (exec-lib? (if-let ((exec-lib (assoc-val :exec-lib stanza-alist)))
                            (if (> pkg-exec-libs-ct 0)
                                (format #f "~A_execlib_~A" pkg-name exec-lib)
                                (format #f "~A_execlib" pkg-name))
                            #f))

         (libname (if privname
                      (string-append
                       (string-upcase (stringify privname)))
                      ;; e.g. for (:ocamlc ) stanza
                      #f))
         (_ (format #t "em libname: ~A~%" libname))

         (testsuite (if-let ((ts (assoc-val :in-testsuite (cdr stanza))))
                            (string-upcase (format #f "~A" ts)) #f))

         (ns (assoc-val :ns stanza-alist))
         (_ (format #t "em ns: ~A~%" ns))

         (deps-tag (if-let ((shared (assoc-in '(:deps :resolved) stanza-alist)))
                           (if (number? (cdr shared)) (cdr shared) libname)
                           libname))
         (_ (format #t "~A: ~A~%" (uwhite "deps-tag") deps-tag))

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

         (_ (format #t "~A: ~A~%" (uwhite "Agg-deps") agg-deps))

         ;; (_ (if (equal? (car module) 'Ocaml_protoc_cmdline)
         ;;        (begin
         ;;          (format #t "~A: ~A~%" (uwhite "aggregtor") stanza)
         ;;          (error 'STOP "STOP emit-module"))))

         (local-deps (if (proper-list? module)
                         (if (alist? (cdr module))
                             ;; (A (:ml a.ml) (:mli a.mli) (:ml-deps...) (:mli-deps...))
                             (let* ((ml-locals (if-let ((locals (assoc :ml-deps (cdr module))))
                                                       (cdr locals)
                                                       '()))
                                    (mli-locals (if-let ((locals (assoc :mli-deps (cdr module))))
                                                        (cdr locals)
                                                        '()))
                                    (locals (concatenate ml-locals mli-locals)))
                               (remove-duplicates locals))
                             ;; else (A a.ml Foo Bar ...)
                             (cdr module))
                         ;; else (A . a.ml) from :structures
                         ;; should not happen?
                         (cdr module)))

         (_ (format #t "~A: ~A~%" (uwhite "Local-deps") local-deps))

         (deps-conditional (assoc-in '(:deps :conditionals) stanza-alist))
         (_ (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional))

         (_ (format #t "~A: ~A~%" (uwhite "module") module))
         (target-selector
          (if deps-conditional
              ;;FIXME: deps-conditional may have more than one entry
              (let ((x
                     (find-then (lambda (conditional)
                                  (format #t "~A: ~A~%" (bgred "conditional") conditional)
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
         (_ (format #t "~A: ~A~%" (uwhite "target-selector") target-selector))

         (src-selectors
          (if target-selector
              (flatten
               (map
                (lambda (sel)
                  (cons (car sel)
                        (cadr sel)))
                (assoc-val :selectors target-selector)))
              #f))
         (_ (format #t "~A: ~A~%" (uwhite "src-selectors") src-selectors))

         (dep-selectors
          (if target-selector
              (map
               (lambda (sel)
                 (let* ((protasis (car sel))
                        (apodosis (last sel)))
                 (list protasis apodosis)))
               (assoc-val :selectors target-selector))
              #f))
         (_ (format #t "~A: ~A~%" (uwhite "dep-selectors") dep-selectors))

         (src-default-apodosis
           (if target-selector
               (assoc-val :default target-selector)
               #f))
         (_ (format #t "~A: ~A~%" (uwhite "src-default-apodosis") src-default-apodosis))

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
         (_ (format #t "~A: ~A~%" (uwhite "OPTS") opts))

         (opts-tag (if (number? opts) opts libname))
         (_ (format #t "~A: ~A~%" (ugreen "opts-tag") opts-tag))

         ;; (ocamlc_opts (if opts ;; (null? opts) '()
         ;;                  (if-let ((x (assoc-val :ocamlc (cdr opts))))
         ;;                          (list (apply string-append
         ;;                                       (map stringify x)))
         ;;                          '())
         ;;                  '()))
         ;; (_ (format #t "~A: ~A~%" (uwhite "ocamlc_opts") ocamlc_opts))

         ;; (ocamlopt_opts (if opts ;; (null? opts) '()
         ;;                    (if-let ((flags (assoc-val :ocamlopt (cdr opts))))
         ;;                            (list (apply string-append
         ;;                                         (map stringify flags)))
         ;;                            '())
         ;;                    ()))
         ;; (_ (format #t "~A: ~A~%" (uwhite "ocamlopt_opts") ocamlopt_opts))

         ;; we always use shared ppxes, so we have e.g. (:ppx . 1)
         ;; lookup ppx-alist in :shared-ppx
         (ppx-id (assoc-val :ppx stanza-alist))
         (_ (format #t "~A: ~A~%" (uyellow "ppx-id") ppx-id))

         (ppx-alist (if ppx-id (assoc-val ppx-id shared-ppx) #f))
         (_ (format #t "~A: ~A~%" (bgyellow "ppx-alist") ppx-alist))

         (ppx-args (if ppx-alist (-get-ppx-args ppx-alist libname) #f))
         (_ (format #t "~A: ~A~%" (bgyellow "ppx-args") ppx-args))

         ;; (ppx-alist (if-let ((ppx (assoc :ppx stanza-alist)))
         ;;                    (cdr ppx) #f))
         ;;  ;; (module->ppx-alist fs-path mname stanzas))
         ;; (_ (format #t "~A: ~A~%" (uwhite "ppx-alist") ppx-alist))

         ;; (ppx-name (if ppx-alist (format #f "~A.ppx" libname)))
         ;; (_ (format #t "~A: ~A~%" (uwhite "ppx-name") ppx-name))

         ;; (ppx-id (if (number? ppx-alist) ppx-alist (-get-ppx-id ws stanza-alist)))
         ;; (_ (format #t "~A: ~A~%" (uwhite "ppx-id") ppx-id))

         (ppx-pkg (if *local-ppx-driver* "" (format #f "//~A" *shared-ppx-pkg*)))
         (_ (format #t "~A: ~A~%" (uwhite "ppx-pkg") ppx-pkg))
         )
    (format #t "~A: ~A~%" (uwhite "ppx id") ppx-id)
    (format #t "module libname: ~A~%" libname)
    (format #t "module ns: ~A~%" ns)
    ;; (if ppx-id (error 'stop "STOP ppx id"))

    ;; (if (proper-list? module)
        (if (alist? (cdr module))
            ;; proper alist (A (:ml a.ml)(:mli a.mli)) (or :ml_, :mli_)
            (let* ((_ (format #t "~A: ~A~%"
                              (red "emitting module (proper assoc-list)") module))
                   (modname (car module))
                   (srcs    (cdr module))
                   (select-sigfile #f)
                   ;; (select-sigfile (assoc-val :mli_ srcs))
                   ;; (_ (format #t "~A: ~A~%" (red "select-sigfile") select-sigfile))
                   ;; (sigfile (if select-sigfile
                   ;;              (make-selector module stanza)
                   ;;              (assoc-val :mli srcs)))
                   (select-structfile #f) ;; (assoc-val :ml_ srcs))
                   ;; (_ (format #t "~A: ~A~%" (red "select-structfile") select-structfile))
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
                                    mli
                                    (assoc-val :ml_ srcs)))
                   )
              ;; (error 'STOP "STOP selmod")
              ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
              ;;         ;;               (cdr opts) '())))
              (format #t "emitting module (proper): ~A: ~A\n" modname srcs)

              (format #t "## this-is-main ~A~%" this-is-main)
              (format outp "## this-is-main ~A~%" this-is-main)
              (if this-is-main
                  (format outp "ocaml_exec_module(\n")
                  (format outp "ocaml_module(\n"))
              (format outp "    name          = \"~A\",\n" modname)
              (if (and ns (not *ns-topdown*))
                  (format outp "    ns_resolver   = \":ns.~A\",\n" ns))

              (if (or select-structfile select-sigfile)
                  (format outp "    module        = \"~A\",\n" modname))

              (if (truthy? target-selector)
                  (begin
                    (format #t "~A: ~A~%" (bgred "IS TGT MLFILE?") target-selector)
                    (format #t "~A: ~A~%" (bgred "TARGET?") (assoc-val :target target-selector))
                    ))

              (if (truthy? target-selector)
                  (if (eq? (fnmatch "*.ml" (format #f "~A" (assoc-val :target target-selector)) 0) 0)
                      (begin
                        (format #t "~A: ~A~%" (uwhite "src-selectors")
                                src-selectors)
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

              (if *build-dyads*
                  (format outp "    sig           = \":~A.cmi\",\n" modname)
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
                  (if this-is-main
                      (if exec-lib?
                          (format outp "    opts          = [\"-open\", \"~A\"] + OPTS_~A,\n"
                                  (normalize-module-name exec-lib?) opts-tag)
                          ;; (format outp "X0    opts          = [\"-open\", \"~A_execlib\"] + OPTS_~A,\n" this-is-main opts-tag)
                          (format outp "    opts          = OPTS_~A,\n" opts-tag))
                      (if exec-lib?
                          ;; should not happen?
                          (format outp "    opts          = OPTS_~A,\n" opts-tag)
                          ;; else no main-module, no exec-lib
                          (format outp "    opts          = OPTS_~A,\n" opts-tag)))
                  ;; else no opts
                  (if this-is-main
                      (if exec-lib?
                          (format outp "X1    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)
                          ;; else should not happen
                          ;; (format outp "    opts          = OPTS_~A,\n" opts-tag))
                          )
                      ;; no opts, no main-module
                      (if exec-lib?
                          ;; should not happen?
                          (format outp "X2    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)
                          ;; else no opts, no main-module, no exec-lib
                          )))
                  ;; (if exec-lib?
                  ;;     (format outp "    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)))

              ;; (if (not (null? ocamlc_opts))
              ;;     (format outp "    opts_ocamlc   = ~A_OCAMLC_OPTS,\n"
              ;;             libname))

              ;; (if (not (null? ocamlopt_opts))
              ;;     (format outp "    opts_ocamlopt = ~A_OCAMLOPT_OPTS,\n"
              ;;             libname))

              ;; (if (not (null? agg-deps))
              ;;     (format outp "    deps          = ~A_DEPS,\n" libname))

              (format #t "~A: ~A~%" (blue "emitting deps A") deps-tag)
              ;; (format outp "## emitting deps A: ~A~%" deps-tag)
              (-emit-deps outp this-is-main exec-lib? deps-tag stanza agg-deps local-deps dep-selectors testsuite)

              ;; (if (not (null? local-deps))
              ;;     (format outp "    local-deps          = ~A,\n" local-deps))

              (if ppx-alist
                  (begin
                    (format outp
                            "    ppx           = \"~A:ppx_~A.exe\",\n"
                            ppx-pkg ppx-id)
                    ;; ppx-name)
                    ;; (cadr (assoc :name ppx-alist)))
                    ;; (if (not
                    ;;      (equal? :all (cadr (assoc :scope
                    ;;                                ppx-alist))))

                    ;;FIXME: handle :scope
                    (if ppx-args
                        (format outp
                                "    ppx_args = [~{~S, ~}],\n" ppx-args))))
                                ;; (cadr (assoc :args ppx-alist))))))
              (format outp ")\n")
              )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; proper list, not alist: (Mytest mytest.ml Hello)
            (let* ((_ (format #t "emitting module (proper list): ~A\n" module))
                   (modname (car module))
                   (structfile (if (proper-list? module) (cadr module)  (cdr module)))
                   (local-deps (if (proper-list? module) (cddr module) '())))
                   ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
                   ;;               (cdr opts) '())))

              (if this-is-main
                  (format outp "ocaml_exec_module(\n")
                  (format outp "ocaml_module(\n"))
               (format outp "    name          = \"~A\",\n" modname)
               (if (and ns (not *ns-topdown*))
                   (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
               ;; (format outp "    struct        = \"~A\",\n" structfile)
               (if (truthy? target-selector)
                   (if (eq? (fnmatch "*.ml" (format #f "~A" (assoc-val :target target-selector)) 0) 0)
                       (begin
                         (format #t "~A: ~A~%" (uwhite "src-selectors")
                                 src-selectors)
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
               (format #t "~A: ~A~%" (umagenta "emitting opts") opts)
               (format #t "~A: ~A~%" (umagenta "this-is-main") this-is-main)
               (format #t "~A: ~A~%" (umagenta "exec-lib?") exec-lib?)
               (if opts ;; (not (null? opts))
                   (if this-is-main
                       (if exec-lib?
                           (format outp "    opts          = [\"-open\", \"~A\"] + OPTS_~A,\n"
                                   (normalize-module-name exec-lib?) opts-tag)
                           (format outp "    opts          = OPTS_~A,\n" opts-tag))
                       (if exec-lib?
                           ;; should not happen?
                           (format outp "    opts          = OPTS_~A,\n" opts-tag)
                           ;; else no main-module, no exec-lib
                           (format outp "    opts          = OPTS_~A,\n" opts-tag)))
                  ;; else no opts
                  (if this-is-main
                      (if exec-lib?
                          (format outp "X4    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)
                          ;; else should not happen
                          ;; (format outp "    opts          = OPTS_~A,\n" opts-tag))
                          )
                      ;; no opts, no main-module
                      (if exec-lib?
                          ;; should not happen?
                          (format outp "X5    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)
                          ;; else no opts, no main-module, no exec-lib
                          )))

               (format #t "~A: ~A~%" (blue "emitting deps B") deps-tag)
               ;; (format outp "## emitting deps B: ~A~%" deps-tag)
               (-emit-deps outp this-is-main exec-lib? deps-tag stanza agg-deps local-deps dep-selectors testsuite)
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

               (if ppx-alist
                   (begin
                     (format outp
                            "    ppx           = \"~A:ppx_~A.exe\",\n" ppx-pkg ppx-id) ;; ppx-name)
                            ;; "    ppx           = \":~A\",\n" ppx-name)
                     (if (not
                          (equal? :all (cadr (assoc :scope
                                                    ppx-alist))))
                         (format outp
                                 "    ppx_args = [~{~S, ~}],\n"
                                 (cadr (assoc :args ppx-alist))))))

               (if exec-lib?
                  (format outp "    visibility    = [\"//visibility:private\"]~%"))

               (format outp ")\n")
               ))
        ;; not proper-list? - improper pair (M . ml) from :structures
        ;; (let* ((_ (format #t "emitting module (cons pair): ~A\n" module))
        ;;        (modname (car module))
        ;;        (structfile (cdr module))
        ;;        (local-deps '()))
        ;;        ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
        ;;        ;;               (cdr opts) '())))

        ;;   (format outp "ocaml_module(\n")
        ;;   (format outp "    name          = \"~A\",\n" modname)
        ;;   (if (and ns (not *ns-topdown*))
        ;;       (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
        ;;   ;; (format outp "    struct        = \"~A\",\n" structfile)
        ;;   (if (truthy? target-selector)
        ;;       (if (eq? (fnmatch "*.ml" (format #f "~A" (assoc-val :target target-selector)) 0) 0)
        ;;           (begin
        ;;             (format #t "~A: ~A~%" (uwhite "src-selectors")
        ;;                     src-selectors)
        ;;             (format outp "    struct        = select({~%")
        ;;             (format outp "~{        \"//bzl/import:~A?\": \"~A\",~^~%~}~%"
        ;;                     src-selectors)
        ;;             (format outp "        \"//conditions:default\": \"~A\"~%"
        ;;                     src-default-apodosis)
        ;;             ;; (format outp "        }, no_match_error=\"no file selected\"\n")
        ;;             (format outp "    }),~%")

        ;;             ))
        ;;       ;; else no 'select'
        ;;       (begin
        ;;         (format outp "    struct        = \"~A\",~%" structfile)
        ;;         ))

        ;;   (if opts ;; (not (null? opts))
        ;;       (if this-is-main
        ;;           (if exec-lib?
        ;;               (format outp "X6    opts          = [\"-open\", \"~A_execlib\"] + OPTS_~A,\n"
        ;;                       (normalize-module-name exec-lib?) opts-tag)
        ;;               (format outp "    opts          = OPTS_~A,\n" opts-tag))
        ;;           (if exec-lib?
        ;;               ;; should not happen?
        ;;               (format outp "    opts          = OPTS_~A,\n" opts-tag)
        ;;               ;; else no main-module, no exec-lib
        ;;               (format outp "    opts          = OPTS_~A,\n" opts-tag)))
        ;;       ;; (if exec-lib?
        ;;       ;;     (format outp "    opts          = [\"-open\", \"~A_execlib\"] + OPTS_~A,\n" this-is-main opts-tag)
        ;;       ;;     (format outp "    opts          = OPTS_~A,\n" opts-tag))
        ;;           (if this-is-main
        ;;               (if exec-lib?
        ;;                   (format outp "X7    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)
        ;;                   ;; else should not happen
        ;;                   ;; (format outp "    opts          = OPTS_~A,\n" opts-tag))
        ;;                   )
        ;;               ;; no opts, no main-module
        ;;               (if exec-lib?
        ;;                   ;; should not happen?
        ;;                   (format outp "X8    opts          = [\"-open\", \"~A_execlib\"],\n"
        ;;                           (assoc-val :exec-lib stanza-alist))
        ;;                           ;; this-is-main)
        ;;                   ;; else no opts, no main-module, no exec-lib
        ;;                   )))
        ;;       ;; (if exec-lib?
        ;;       ;;         (format outp "    opts          = [\"-open\", \"~A_execlib\"],\n" this-is-main)))
        ;;   ;; (if (not (null? ocamlc_opts))
        ;;   ;;     (format outp "    opts_ocamlc   = ~A_OCAMLC_OPTS,\n"
        ;;   ;;             libname))

        ;;   ;; (if (not (null? ocamlopt_opts))
        ;;   ;;     (format outp "    opts_ocamlopt = ~A_OCAMLC_OPTS,\n"
        ;;   ;;             libname))

        ;;   (format #t "~A: ~A~%" (blue "emitting deps C") deps-tag)
        ;;   (format outp "## emitting deps C: ~A~%" deps-tag)
        ;;   (-emit-deps outp exec-lib? deps-tag stanza agg-deps local-deps dep-selectors testsuite)
        ;;   ;; (format #t "~A: ~A~%" (red "local-deps") local-deps)
        ;;   ;; (if (not (null? local-deps))
        ;;   ;;     (if (not (null? agg-deps))
        ;;   ;;         (begin
        ;;   ;;           (if (equal? :executable (car stanza))
        ;;   ;;               (format outp "    deps          = ~A_EXE_DEPS + [\n" (if testsuite testsuite libname))
        ;;   ;;               (format outp "P    deps          = ~A_DEPS + [\n" (if testsuite testsuite libname)))
        ;;   ;;           (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
        ;;   ;;           (format outp "    ],\n"))
        ;;   ;;         (begin
        ;;   ;;           (format outp "    deps          = [\n" libname)
        ;;   ;;           (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
        ;;   ;;           (format outp "    ],\n")))

        ;;   ;;     (if (not (null? agg-deps))
        ;;   ;;         (format outp "Q    deps          = ~A_DEPS,\n" (if testsuite testsuite libname))))

        ;;   (if ppx-alist
        ;;       (begin
        ;;         (format #t "~A: ~A~%" (uyellow "ppx-alist") ppx-alist)
        ;;         (format outp
        ;;                 "    ppx           = \"~A:ppx_~A.exe\",\n"
        ;;                 ppx-pkg ppx-id) ;; ppx-name)
        ;;                 ;; "    ppx           = \":~A\",\n" ppx-name)
        ;;         (if ppx-args
        ;;             (begin
        ;;               (format outp "    ppx_args           = [~%")
        ;;               (format outp "~{        \"~A\"~^,~%~}~%" ppx-args)
        ;;               (format outp "    ],~%")))

        ;;         ;; (if (not
        ;;         ;;      (equal? :all (cadr (assoc :scope
        ;;         ;;                                ppx-alist))))
        ;;         ;;     (format outp
        ;;         ;;             "    ppx_args = [~{~S, ~}],\n"
        ;;         ;;             (cadr (assoc :args ppx-alist))))
        ;;         ))

        ;;   (format outp ")\n")
        ;;   ) ;; let*
        ;; )
    ;; (if deps-conditional
    ;;     (error 'STOP
    ;;            (format #f "STOP cond module: ~A" stanza)))
    )
  stanza)

(define (-emit-modules outp ws pkg modules)
  (format #t "~A: ~A\n" (bgblue "-emit-modules") modules)

  (for-each
   (lambda (module)
     (format #t "~%~A: ~A  ~A\n" (ublue "next module")
             (ured (car module)) module)
     ;; (flush-output-port)
     (let* ((modname (car module))
            ;; (_ (format #t "~A: ~A~%" (blue "exec-lib") exec-lib))
            (aggregator (find-if
                         (lambda (stanza)
                           (format #t "~A: ~A~%" (uwhite "searching stanza:") stanza)
                           (begin
                             ;; (format #t "~A: ~A~%" ( "exec-lib") exec-lib)
                             (case (car stanza)
                               ((:ns-archive :ns-library)
                                (if-let ((submods
                                          (cdr (assoc-in '(:manifest :modules)
                                                         (cdr stanza)))))
                                        (begin
                                          (format #t "submods: ~A\n" submods)
                                          (if (member modname submods)
                                              ;;(-emit-module outp ws module stanza)
                                              #t #f))))

                               ((:archive :library)
                                (if-let ((submods
                                          (cdr (assoc-in '(:manifest :modules)
                                                         (cdr stanza)))))
                                        (begin
                                          (format #t "submods: ~A\n" submods)
                                          (if (member modname submods)
                                              ;;(-emit-module outp ws module stanza)
                                              #t #f))))

                               ((:executable :test)
                                (format #t "~A: ~A for ~A~%" (uwhite "checking :executable") stanza (bgcyan modname))
                                (if (equal? modname (assoc-val :main (cdr stanza)))
                                    #t
                                    (if-let ((exec-lib (assoc-val :exec-lib (cdr stanza))))
                                            (let* ((pkg-exec-libs (assoc-in '(:dune :exec-libs) pkg))
                                                   (_ (format #t "~A: ~A~%" (ucyan "pkg-exec-libs") pkg-exec-libs))
                                                   (exec-lib-alist (assoc-val exec-lib (cdr pkg-exec-libs)))
                                                   (_ (format #t "~A: ~A~%" (ucyan "exec-lib alist") exec-lib-alist))
                                                   (exec-modules (assoc-val :modules exec-lib-alist)))
                                              (format #t "~A: ~A~%" (cyan "stanza exec-lib") exec-lib)
                                              (format #t "~A: ~A~%" (cyan "pkg-exec-libs") pkg-exec-libs)
                                              (format #t "~A: ~A~%" (cyan "exec-modules") exec-modules)
                                              (format #t "~A: ~A~%" (cyan "modname") modname)
                                              (format #t "~A: ~A~%" (cyan "huh?") (member modname exec-modules))
                                              (member modname exec-modules))
                                            #f)))

                               ((:ocamlc) ;; from (rule (action (run %{bin:ocamlc} ...)))
                                (if-let ((srcs (assoc-val :srcs (cdr stanza))))
                                        (begin
                                          (format #t "~A: ~A\n" (uwhite "ocamlc srcs") srcs)
                                          (find-if (lambda (src)
                                                     (format #t "~A: ~A\n" (uwhite "ocamlc src") src)
                                                     (format #t "~A: ~A\n" (uwhite "module file") (cdr module))
                                                     (string=? (format #f "~A" (assoc-val :tgt (cdr src)))
                                                               (format #f "~A" (cdr module))))
                                                   srcs))))

                               ((:install :ocamllex :ocamlyacc :menhir
                                          :shared-compile-opts :shared-deps
                                          :sh-test :testsuite :node :exec-libs) #f)

                               ((:rule)
                                (format #t "~A: ~A~%" (bgred "handle :rule for -emit-modules") stanza)
                                #f)

                               (else
                                (error 'UNHANDLED
                                       (format #f "unhandled kind: ~A" stanza))))))
                         (assoc-val :dune pkg)))
            )
       ;; (if (equal? modname 'Pb_codegen_backend)
       ;;     (begin
       ;;       (format #t "~A: ~A~%" (bgred "aggregator") aggregator)
       ;;       (error 'STOP "STOP agg")))
       (if aggregator
           (begin
             (format #t "~A ~A: ~A\n"
                     (uwhite "Found containing aggregator for")
                     modname aggregator)
             (if-let ((main (assoc-val :main (cdr aggregator))))
                     (if (not (equal? modname main))
                         (begin
                           (-emit-module outp ws module aggregator pkg)
                           (format outp "\n")))
                     ;; else not an executable
                     (begin
                       (-emit-module outp ws module aggregator pkg)
                       (format outp "\n"))))
           (begin
             (format #t "~A ~A; excluding\n"
                     (uwhite "No aggregator found for") modname)))
       ))
   (sort! modules (lambda (a b) (sym<? (car a) (car b)))))
  (newline outp))

(define (-module-record->sigfile module)
  (format #t "-module-record->sigfile ~A\n" module)
  ;; module form: ((:mli a.mli) (:ml a.ml))
  ;; where either :mli or :ml may have trailing _
  (if-let ((sig (assoc-val :mli module)))
          sig
          (if-let ((sig (assoc-val :mli_ module)))
                  sig
                  (error 'missing-mli "module record missing :mli, :mli_"))))

(define (-emit-sig outp sig stanza)
  (format #t "~A: ~A, ~A\n" (blue "-emit-sig") sig stanza)
  (let* ((libname (string-append
                   (string-upcase
                    (stringify
                     (assoc-val :privname (cdr stanza))))))
         (ns (assoc-val :ns (cdr stanza)))

         (ppx-alist (if-let ((ppx (assoc-val :ppx (cdr stanza))))
                            ppx #f))
         ;; (module->ppx-alist fs-path mname stanzas))
         (_ (format #t "ppx-alist: ~A\n" ppx-alist))

         (ppx-name (if ppx-alist (format #f "~A.ppx" libname)))

         (ppx-id (-get-ppx-id ws (cdr stanza)))
         )

    (let* ((modname (car sig))
           (sigfile (if (proper-list? sig)
                        (-module-record->sigfile (cdr sig))
                        (cdr sig))))

      (format #t "emitting signature: ~A\n" modname)

      (format outp "ocaml_signature(\n")
      (format outp "    name          = \"~A.cmi\",\n" modname)
      (if (and ns (not *ns-topdown*))
          (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
      (format outp "    sig           = \"~A\",\n" sigfile)
      (format outp "    opts          = ~A_COMPILE_OPTS,\n" libname)
      (format outp "    deps          = DEPS_~A,\n" libname)

      (if ppx-alist
          (begin
            (format outp
                    "    ppx           = \"~A:ppx_~A.exe\",\n"
                    ppx-pkg ppx-id) ;; ppx-name)
                    ;; "    ppx           = \":~A\",\n" ppx-name)
                    ;; (cadr (assoc :name ppx-alist)))
            (if ppx-args
                (format outp
                        "    ppx_args = [~{~S, ~}],\n" ppx-args))

            ;; (if (not
            ;;      (equal? :all (cadr (assoc :scope
            ;;                                ppx-alist))))
            ;;     (format outp
            ;;             "    ppx_args = [~{~S, ~}],\n"
            ;;             (cadr (assoc :args ppx-alist))))
            ))
      (format outp ")\n\n")
      )))

(define (-emit-sig-freestanding outp sig)
  (format #t "~A: ~A~%" (blue "-emit-sig-freestanding") sig)
  (let* (;; (libname (string-append
         ;;           (string-upcase
         ;;            (stringify
         ;;             (assoc-val :privname (cdr stanza))))))
         ;; (ns (assoc-val :ns (cdr stanza)))

         ;; (ppx-alist (if-let ((ppx (assoc-val :ppx (cdr stanza))))
         ;;                    ppx #f))
         ;; ;; (module->ppx-alist fs-path mname stanzas))
         ;; (_ (format #t "ppx-alist: ~A\n" ppx-alist))

         ;; (ppx-name (if ppx-alist (format #f "~A.ppx" libname)))
         )

    (let* ((modname (car sig))
           (sigfile (if (proper-list? sig)
                        (-module-record->sigfile (cdr sig))
                        (cdr sig))))

      (format #t "emitting signature: ~A\n" modname)

      (format outp "ocaml_signature(\n")
      (format outp "    name          = \"~A.cmi\",\n" modname)
      (format outp "    src           = \"~A\",\n" sigfile)
      ;; (format outp "    opts          = ~A_OPTS,\n" libname)
      ;; (format outp "    deps          = ~A_DEPS,\n" libname)

      ;; (if ppx-alist
      ;;     (begin
      ;;       (format outp
      ;;              "    ppx           = \":~A\",\n" ppx-name)
      ;;               ;; (cadr (assoc :name ppx-alist)))
      ;;       (if (not
      ;;            (equal? :all (cadr (assoc :scope
      ;;                                      ppx-alist))))
      ;;           (format outp
      ;;                   "    ppx_args = [~{~S, ~}],\n"
      ;;                   (cadr (assoc :args ppx-alist))))))
      (format outp ")\n\n")
      )))

(define (-emit-sigs-hdr outp sigs pkg-modules)
  (format #t "~A: ~A~%" (ublue "-emit-sigs-hdr") sigs)
  (format #t "~A: ~A~%" (blue "pkg-modules") pkg-modules)
  (format #t "~A: ~A~%" (blue "*build-dyads*") *build-dyads*)
  (if *build-dyads*
      (if (or (not (null? sigs))
              (find-if (lambda (m)
                         (if (proper-list? m)
                             (or (assoc :mli (cdr m)) (assoc :mli_ (cdr m)))
                             (eq? 0 (fnmatch "*.mli"
                                             (format #f "~A" (cdr m)) 0))))
                       pkg-modules))
          (begin
            (format outp "#############################\n")
            (format outp "####  Signature Targets  ####\n")
            (newline outp)))))
      ;; (if (not (null? sigs))
      ;;     (begin
      ;;       (format outp "#############################\n")
      ;;       (format outp "####  Signature Targets  ####\n")
      ;;       (newline outp)))))

(define (-emit-signatures outp pkg sigs pkg-modules)
  (format #t "~A: ~A\n" (bgblue "-emit-signatures") sigs)
  (format #t "*build-dyads*: ~A\n" *build-dyads*)
  ;; (format #t "pkg: ~A\n" pkg)

  (-emit-sigs-hdr outp sigs pkg-modules)

  (if *build-dyads*
      (for-each
       (lambda (module)
         (format #t "dyad: : ~A\n" module)
         (if (proper-list? module)
             (let ((modname (car module))
                   ;; (mli (if-let ((mli (assoc-val :mli (cdr module))))
                   ;;              mli
                   ;;              (if-let ((mli (assoc-val :mli_ (cdr module))))
                   ;;                      mli
                   ;;                      #f)))
                   )
               (format #t "sig for: ~A\n" modname)
               (let* ((aggregator
                       (find-if
                        (lambda (stanza)
                          (format #t "checking stanza for msig ~A\n" stanza)
                          (case (car stanza)
                            ((:archive :library :ns-archive :ns-library)
                             ;; (if (eq? :library (car stanza))
                             (if-let ((submods
                                       (assoc-in '(:manifest :modules)
                                                  (cdr stanza))))
                                     (begin
                                       (format #t "submods: ~A\n" submods)
                                       (if (member modname (cdr submods))
                                           #t
                                           #f))))
                            (else #f)))
                        (assoc-val :dune pkg)))
                      )
                 (if aggregator
                     (-emit-sig outp module aggregator)))
                 ;; (format #t "aggregator: ~A\n" aggregator)
               ;; (-emit-sig outp mli stanza)
               )
             ;; else improper list - ignore for *build-dyads*
             ))
       pkg-modules)
      ;; else just free-standing sigs
      (for-each
       (lambda (sig)
         (format #t "~A: ~A\n" (uwhite "free-standing sig") sig)
         (let* ((modname (car sig))
                (aggregator (find-if
                             (lambda (stanza)
                               (format #t "~A: ~A\n" (uwhite "checking stanza") stanza)
                               (case (car stanza)
                                 ((:archive :library)
                                  ;; (if (eq? :library (car stanza))
                                  (if-let ((subsigs
                                            (assoc-val :subsigs
                                                       (cdr stanza))))
                                          (begin
                                            (format #t "~A: ~A\n" (red "subsigs") subsigs)
                                            (if (member modname subsigs)
                                                (-emit-sig outp sig stanza)
                                                #f))))
                                 (else #f)))
                             (assoc-val :dune pkg)))
                )
           (if (not aggregator)
               (-emit-sig-freestanding outp sig))))
       sigs)
      ))

;; (define (starlark-emit-singleton-targets outp fs-path stanzas dune-pkg)

(define (starlark-emit-singleton-targets outp ws pkg)
  (format #t "~A: ~A\n" (blue "starlark-emit-singleton-targets") pkg)

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

    (format #t "~A: ~A\n" (ucyan "pkg-modules") pkg-modules)
    (format #t "~A: ~A\n" (ucyan "pkg-structs") pkg-structs)
    (format #t "~A: ~A\n" (ucyan "pkg-sigs") pkg-sigs)

    (if (or (not (null? pkg-modules))
            (not (null? pkg-structs))
            (not (null? pkg-sigs)))
        (begin
          (format outp "######################## Modules & Signatures ########################")
          (newline outp)))

    (if pkg-modules
        (begin
          (newline)
          (format #t "~A~%" (bgblue "emitting pkg-modules"))
          (-emit-modules outp ws pkg pkg-modules)))

    (if pkg-structs
        (begin
          (newline)
          (format #t "~A~%" (bgblue "emitting pkg-structs"))
          (-emit-modules outp ws pkg pkg-structs)))

    ;; (if (equal? (car pkg :ocamlc))
    ;;       (-emit-modules outp ws pkg pkg-structs)))

    (if (or pkg-sigs *build-dyads*)
        (begin
          (newline)
          (format #t "~A~%" (bgblue "emitting pkg-sigs"))
          (-emit-signatures outp pkg pkg-sigs pkg-modules)))
    ))

;;(format #t "loaded starlark/singletons.scm\n")
