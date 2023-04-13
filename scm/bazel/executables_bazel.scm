(if (or *mibl-debug-executables* *mibl-debug-s7*)
    (format #t "loading bazel/executables_bazel.scm\n"))

;; (define (bazel-emit-executable-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "bazel-emit-executable-target") stanza))


;; emit-exe-ns: exec prologues must be namespaced; we use bottom-up
;; FIXME: should :main deps also be namespaced?
;; we need one ns per executable
;; ocaml_ns_resolver(
;;     name = "Main_ns",
;;     ns   = "Main_ns",
;;     manifest = ["A", "B",...]
;; )
;; not currently used
(define (-emit-local-prologue-ns outp tgtname pkg prologue)
  (if (or *mibl-debug-executables* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "-emit-local-prologue-ns") tgtname)
        (format #t "~A: ~A~%" (ublue "prologue") prologue)))
  ;; (error 'x "X")
  (let* ((prologue-ns (format #f "nsPrologue_~A" tgtname)))

    (format outp "ocaml_ns_resolver(\n")
    (format outp "    name     = ~S,\n" prologue-ns)
    (format outp "    ns       = ~S,\n" prologue-ns)
    (format outp "    manifest = [\n")
    (format outp "~{        \"~A\"~^,~%~}\n" prologue)
    (format outp "    ]\n")
    (format outp ")\n")
    (newline outp)))

;; not currently used
(define (-emit-main-module-ns outp tgtname stanza pkg)
  (if (or *mibl-debug-executables* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "-emit-main-module-ns") tgtname)))

  (if-let ((main-module (assoc-val :main (cdr stanza)))
           (main-deps (assoc-val :main-deps (cdr stanza))))
          (begin
            (format outp "ocaml_ns_resolver(\n")
            (format outp "    name     = \"ns.~A\",\n" main-module)
            (format outp "    ns       = \"ns.~A\",\n" main-module)
            (format outp "    manifest = [\n")
            (format outp "~{        \"~A\"~^,~%~}\n" (cons main-module main-deps))
            (format outp "    ]\n")
            (format outp ")\n")
            (newline outp))))

;; one ns for all modules in executable pkg
;; includes all :main-deps and prologues,
;; excludes deps ('(modules...)') of library stanzas
(define (-emit-executables-ns outp pkg)
  (if (or *mibl-debug-executables* *mibl-debug-s7*)
      (begin
        (format #t "~A pkg: ~A~%" (ublue "-emit-executables-ns") pkg)))

  (let* ((modules (fold (lambda (stanza accum)
                          (format #t "stanza: ~A\n" stanza)
                          (case (car stanza)
                            ((:executable)
                             (let ((main-module (assoc-val :main (cdr stanza)))
                                   (main-deps (assoc-val* :main-deps (cdr stanza)))
                                   (prologue (assoc-val* :prologue (cdr stanza))))
                               (cons main-module (append accum prologue main-deps))))
                            ((:executables) (error 'bad-arg "unexpected :executables stanza"))
                            (else accum)))
                        '() (assoc-val :mibl pkg))))
    (if (truthy? modules)
        (let ((modules (remove-duplicates (sort! modules sym<?)))
              (len (fold (lambda (m sum) (+ sum 1 (string-length (format #f "~A" m)))) 0 modules)))

          ;; (format #t "modules len: ~A\n" len)
          ;; (error 'x "X")

          (format outp "##################\n")
          (format outp "# namespaces all exec modules\n")
          (format outp "ocaml_ns_resolver(\n")
          (format outp "    name     = \"Exe_ns\",\n")
          (format outp "    ns       = \"Exe_ns\",\n")
          (format outp "    manifest = [")
          (if (> len 48)
              (begin
                (newline outp)
                (format outp "~{        \"~A\"~^,~%~}\n" modules)
                (newline outp))
              (format outp "~{\"~A\"~^, ~}" modules))
          (format outp "]\n")
          (format outp ")\n")
          (newline outp)))))

;; local-deps: direct, listed in :modules, :structures
;; deps-tag: id of :shared-deps entry
;; agg-deps: derived from aggregate containing module (N/A for main module?)
(define (-emit-main-deps outp deps-tag
                         agg-deps local-deps selectors testsuite)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (bgblue "-emit-main-deps") deps-tag)
        (format #t "~A: ~A~%" (uwhite "agg-deps") agg-deps)
        (format #t "~A: ~A~%" (uwhite "local-deps") local-deps)
        (format #t "~A: ~A~%" (uwhite "deps-tag") deps-tag)
        ))
  (if (or (number? deps-tag)
          (truthy? local-deps)
          (truthy? agg-deps)
          selectors)
      (format outp "    deps          = ")
      )

  ;;NB: prologue is technically not a main dep, so we attach it to
  ;;executable target, not main module

  ;; debugging
  ;; (newline outp)
  ;; (format outp "local-deps: ~A\n" local-deps)
  ;; (format outp "agg-deps: ~A\n" agg-deps)
  ;; (format outp "deps-tag: ~A\n" deps-tag)
  ;; (format outp "testsuite: ~A\n" testsuite)
  ;; (newline outp)
  ;; end debugging

  (if (truthy? local-deps)
      (let ((local-deps-len
              (fold (lambda (m sum) (+ sum 1 (string-length (format #f "~A" m)))) 0 local-deps)))
        (if (truthy? agg-deps)
            (begin
              (format outp "1: DEPS_~A + [\n" (if testsuite testsuite deps-tag))
              (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
              (format outp "    ]"))
            ;; else local deps but no agg deps
            (if (number? deps-tag)
                (begin
                  (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                  (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                  (format outp "    ]"))

                ;; else deps-tag is ??
                (if (> local-deps-len 48)
                    (begin
                      ;; (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                      (format outp "3: [\n")
                      (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                      (format outp "    ]"))
                    (format outp "[~{\":~A\"~^, ~}]" local-deps)))))
      ;; else no local-deps
      (if (truthy? agg-deps)
          ;; agg-deps e.g. ((:resolved @ocaml//compiler-libs/common))
          (if (number? deps-tag)
              (error 'FIXME
                     (format #f "found both numeric deps-tag ~A and agg-deps ~A"
                      deps-tag agg-deps))
              (format outp "4: DEPS_~A"
                      (if testsuite testsuite deps-tag)))
          ;; else no agg-deps, no local-deps
          (if (number? deps-tag)
              (format outp "5: DEPS_~A" (if testsuite testsuite deps-tag))
              ;; (error 'FIXME
              ;;        (format
              ;;         #f "found non-numeric deps-tag ~A but no deps"
              ;;         deps-tag))
              ;; else emit nothing
              )))

  ;; (error 'x "X")

  (if selectors
      (begin
        (if (or *mibl-debug-emit* *mibl-debug-s7*)
            (format #t "~A: ~A~%" (uwhite "emitting selectors") selectors))
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
              )
              ;; selectors)
          (format outp ",~%"))
      ))

;; We need namespacing for exe module deps to avoid name clashes, same
;; as for lib submodules. This goes for main module too?

;; We follow dune's example: dune namespaces exes with names
;; like dune__exe__Test_io.cmo and -open Dune__exe and ns
;; resolver dune__exe.ml-gen; it then links the namespace
;; submodules with -o main.exe (main.ml is compiled as
;; dune__exe__Main and linked last). (see
;; jsoo/compiler/tests-num)

;; testcases: jsoo/compiler/tests-ocaml/lib-bytes,


;; main module: (:main . Foo)
;; local deps are in :main-deps
;; :prologue is ignored, should not contain deps of :main
(define (-emit-main-module outp ws stanza pkg)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A\n" (bgblue "-emit-main-module") stanza)
        (format #t "~A: ~A~%" (blue "pkg") pkg)))
  ;; (if (not (equal? (car stanza) :executable)) (error 'bad-stanza "-emit-main-module got non-executable stanza"))

  (let* ((stanza-alist (cdr stanza))
         (main (assoc-val :main stanza-alist))
         (module-spec (module-name->tagged-label main pkg))
         ;; module-spec: (Foo (:ml foo.ml DepA) (:mli foo.mli))
         ;;   or :structure (Foo foo.ml DepA DepB)

         (pkg-name (pkg->pkg-name pkg))
         ;; (_ (format #t "~A: ~A~%" (blue "pkg-name") pkg-name))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A\n" (blue "stanza-alist") stanza-alist)))

         ;; (shared-ppx (if-let ((shppx (assoc-in '(:mibl :shared-ppx) pkg)))
         ;;                     (cadr shppx) #f))
         (pkg-shared-ppx-alist (if-let ((shppx (assoc-in '(:mibl :shared-ppx) pkg)))
                             (cadr shppx) #f))
         (mibl-trace-let "pkg-shared-ppx-alist" pkg-shared-ppx-alist *mibl-debug-emit*)

         (privname (if-let ((privname (assoc-val :privname stanza-alist)))
                           privname
                           #f))

         (executable-resolver "Main_ns")

         (libname (if privname
                      (string-append
                       (string-upcase (stringify privname)))
                      ;; e.g. for (:ocamlc ) stanza
                      #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "main libname") libname)))

         (testsuite (if-let ((ts (assoc-val :in-testsuite (cdr stanza))))
                            (string-upcase (format #f "~A" ts)) #f))

         (ns (assoc-val :ns stanza-alist))
         (mibl-trace-let "ns" ns *mibl-debug-emit*)

         ;; :deps are attached to the aggregate. may contain :conditionals.
         ;; FIXME: deps-tag and agg-deps redundant? the latter contains unresolved deps?
         (deps-tag (if-let ((shared (assoc-in '(:deps :resolved) stanza-alist)))
                           (if (number? (cdr shared)) (cdr shared) libname)
                           libname))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "deps-tag") deps-tag)))

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

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "Agg-deps") agg-deps)))

         ;; (_ (if (equal? (car module) 'Ocaml_protoc_cmdline)
         ;;        (begin
         ;;          (format #t "~A: ~A~%" (uwhite "aggregtor") stanza)
         ;;          (error 'STOP "STOP emit-module"))))

         ;; (local-deps (if (proper-list? module)
         ;;                 (if (alist? (cdr module))
         ;;                     ;; (A (:ml a.ml DepX DepY) (:mli a.mli DepA))
         ;;                     (let* ((ml-locals (if-let ((locals (assoc-val :ml (cdr module))))
         ;;                                               (cdr locals)
         ;;                                               (if-let ((locals (assoc-val :ml_ (cdr module))))
         ;;                                                       (cdr locals)
         ;;                                                       '())))
         ;;                            ;; (_ (format #t "ml-locals: ~A\n" ml-locals))
         ;;                            (mli-locals (if-let ((locals (assoc-val :mli (cdr module))))
         ;;                                                (cdr locals)
         ;;                                                (if-let ((locals (assoc-val :mli (cdr module))))
         ;;                                                        (cdr locals)
         ;;                                                        '())))
         ;;                            ;; (_ (format #t "mli-locals: ~A\n" mli-locals))
         ;;                            (locals (concatenate ml-locals mli-locals)))
         ;;                       (remove-duplicates locals))
         ;;                     ;; else (A a.ml Foo Bar ...)
         ;;                     (cddr module))
         ;;                 ;; else (A . a.ml) from :structures
         ;;                 ;; should not happen?
         ;;                 (cdr module)))

         (local-deps (assoc-val :main-deps stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "Local-deps") local-deps)))

         (deps-conditional (assoc-in '(:deps :conditionals) stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "deps-conditional") deps-conditional)))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "module-spec") module-spec)))

         (target-selector
          (if deps-conditional
              ;;FIXME: deps-conditional may have more than one entry
              (let ((x
                     (find-then (lambda (conditional)
                                  (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                      (format #t "~A: ~A~%" (bgred "conditional") conditional))
                                  (let ((ctarget (assoc-val :target conditional)))
                                    (if (alist? (cdr module-spec))
                                        (find-then (lambda (msrc)
                                                     (if (equal? ctarget (cdr msrc))
                                                         conditional #f))
                                                 (cdr module-spec))
                                        (if (equal? ctarget (cdr module-spec))
                                            conditional #f))))
                                (cdr deps-conditional))))
                x)
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "target-selector") target-selector)))

         (src-selectors
          (if target-selector
              (flatten
               (map
                (lambda (sel)
                  (cons (car sel)
                        (cadr sel)))
                (assoc-val :selectors target-selector)))
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "src-selectors") src-selectors)))

         (dep-selectors
          (if target-selector
              (map
               (lambda (sel)
                 (let* ((protasis (car sel))
                        (apodosis (last sel)))
                 (list protasis apodosis)))
               (assoc-val :selectors target-selector))
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "dep-selectors") dep-selectors)))

         (src-default-apodosis
           (if target-selector
               (assoc-val :default target-selector)
               #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "src-default-apodosis") src-default-apodosis)))

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
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "OPTS") opts)))

         (opts-tag (if (number? opts) opts libname))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "opts-tag") opts-tag)))

         ;; link-opts, ocamlc-opts, ocamlopt-opts???

         ;; we always use shared ppxes (pkg scope), so we have e.g. (:ppx . 1)
         ;; lookup ppx-alist in :pkg-shared-ppx-alist
         ;; (ppx-id (get-ppx-id ws (cdr stanza)))
         (ppx-id (if-let ((ppx (assoc-val :ppx stanza-alist))) ppx #f))
         (mibl-trace-let "module ppx-id" ppx-id *mibl-debug-emit*)

         (ppx-label (if ppx-id
                        (if (> (length pkg-shared-ppx-alist) 1)
                            (format #f ":ppx_~A.exe" ppx-id)
                            (format #f ":ppx.exe"))
                        #f))
         (mibl-trace-let "main module ppx-label" ppx-label *mibl-debug-emit*)

         ;; (ppx-alist (if ppx-id (assoc-val ppx-id pkg-shared-ppx-alist) #f))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "ppx-alist") ppx-alist)))
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
         (mibl-trace-let "main ppx-args" ppx-args *mibl-debug-emit*)

         (ppx-codeps (if-let ((codeps (assoc-val :ppx-codeps stanza-alist))) codeps #f))
         (mibl-trace-let "main ppx-codeps" ppx-codeps *mibl-debug-emit*)

         ;; (ppx-pkg (if *mibl-local-ppx-driver* "" (format #f "//~A" *mibl-shared-ppx-pkg*)))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "ppx-pkg") ppx-pkg)))
         )

    ;; FIXME: if *mibl-build-dyads* then emit both ocaml_module and ocaml_signature

    ;; (if (proper-list? module)
    (if (alist? (cdr module-spec))
        ;; proper alist (A (:ml a.ml)(:mli a.mli)) (or :ml_, :mli_)
        (let* ((_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%"
                                                            (red "emitting main module (proper assoc-list)") module-spec)))
               (modname (car module-spec))
               (srcs    (cdr module-spec))
               (select-sigfile #f)
               ;; (select-sigfile (assoc-val :mli_ srcs))
               ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "select-sigfile") select-sigfile)))
               ;; (sigfile (if select-sigfile
               ;;              (make-selector module-spec stanza)
               ;;              (assoc-val :mli srcs)))
               (select-structfile #f) ;; (assoc-val :ml_ srcs))
               ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (red "select-structfile") select-structfile)))
               ;; (structfile (if select-structfile
               ;;                 (make-selector module-spec stanza)
               ;;                 (assoc-val :ml srcs)))
               ;; ;; for case deps/dynamic
               ;; (structfile (if structfile structfile
               ;;                 (assoc-val :ml_ srcs)))

               (sigfile (if-let ((mli (assoc-val :mli srcs)))
                                mli
                                (assoc-val :mli_ srcs)))
               (structfile (if-let ((mli (assoc-val :ml srcs)))
                                   (car mli)
                                   (car (assoc-val :ml_ srcs))))
               )
          ;; (error 'STOP "STOP selmod")
          ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
          ;;         ;;               (cdr opts) '())))
          (if (or *mibl-debug-emit* *mibl-debug-s7*)
              (begin
                (format #t "emitting main module (proper): ~A: ~A\n" modname srcs)))

          (if (or (assoc :ppx-rewriter stanza-alist)
                  (assoc :ppx-deriver stanza-alist)
                  ppx-codeps)
              (format outp "ppx_exec_module(\n")
              (format outp "ocaml_exec_module(\n"))

          (format outp "    name          = \"~A\",\n" modname)

          (if *mibl-namespace-executables*
              ;; (if *mibl-executables-ns-split*
              ;;     (begin ;; not implemented
              ;;     (format outp "    ns_resolver   = \":ns.~A\",\n" modname))
              (format outp "    ns_resolver   = \":Exe_ns\",\n"))
          ;; )

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

          ;;FIXME: link-opts, ocamlc-opt, ocamlopt-opts
          (if opts ;; (truthy? opts))
              (format outp "    opts          = OPTS_~A,\n" opts-tag))

              ;; (if prologue-nm
              ;;         (format outp "    opts          = [\"-open\", \"~A\"] + OPTS_~A,\n"
              ;;                 ;;(normalize-module-name prologue-nm)
              ;;                 executable-resolver
              ;;                 opts-tag)
              ;; else no opts
              ;; (if prologue-nm
              ;;     (format outp "X1    opts          = [\"-open\", \"~A\"], ## Y0\n" prologue-nm)
              ;;     ;; else should not happen
              ;;     ;; (format outp "    opts          = OPTS_~A,\n" opts-tag))
              ;;     ))

          (if (or *mibl-debug-emit* *mibl-debug-s7*)
              (format #t "~A: ~A~%" (blue "emitting main deps A") deps-tag))

          (-emit-main-deps outp deps-tag
                           agg-deps local-deps dep-selectors testsuite)

          (if ppx-label
              (begin
                (mibl-trace "emitting main ppx" ppx-alist *mibl-debug-emit*)
                (format outp "    ppx           = \"~A\",\n" ppx-label)
                (mibl-trace "emitting main ppx-args" ppx-args *mibl-debug-emit*)
                ;;FIXME: handle :scope
                (if (truthy? ppx-args)
                    (format outp "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args))))

                ;; (if (> (length pkg-shared-ppx-alist) 1)
                ;;     (format outp "    ppx           = \"~A:Appx_~A.exe\",\n" ;; #X2
                ;;             ppx-pkg ppx-id)
                ;;     (format outp "    ppx           = \"~A:ppx.exe\",\n" ppx-pkg))

                ;; ppx-name)
                ;; (cadr (assoc :name ppx-alist)))
                ;; (if (not
                ;;      (equal? :all (cadr (assoc :scope
                ;;                                ppx-alist))))

                ;;FIXME: handle :scope
                ;; (if ppx-args
                ;;     (let* ((mod (assoc-in `(:modules ,modname) pkg))
                ;;            (mfile (car (assoc-val :ml (cdr mod))))
                ;;            )
                ;;       (format outp
                ;;               "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n"
                ;;               (append (list "-loc-filename"
                ;;                             (format #f "~A" mfile))
                ;;                       ppx-args))))
                ;; (if-let ((codeps (assoc-val :ppx-codeps stanza-alist)))
                ;;         (format outp "    ppx_codeps    = [~{\"~S\"~^, ~}],\n" codeps))
                ;; ))
          (if ppx-codeps
              (format outp "    ppx_codeps    = [~{\"~S\"~^, ~}]\n" ppx-codeps))

          (format outp ")\n")
          (newline outp)
          )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; proper list but not alist: (Mytest mytest.ml Hello) - no sig
        (let* ((mibl-trace-let "emitting main module (w/o sig)" module-spec *mibl-debug-emit*)
               (modname (car module-spec))
               (structfile (if (proper-list? module-spec) (cadr module-spec)  (cdr module-spec)))
               (local-deps (if (proper-list? module-spec) (cddr module-spec) '())))
          ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
          ;;               (cdr opts) '())))

          (if ppx-codeps
              (format outp "ppx_module(\n")
              (format outp "ocaml_exec_module(\n"))

          (format outp "    name          = \"~A\",\n" modname)

          (if (and ns (not *mibl-ns-topdown*))
              (format outp "    ns_resolver   = \":ns.~A\",\n" ns))

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
          (mibl-trace "emitting main opts" opts *mibl-debug-emit*)
          (if opts ;; (not (null? opts))
              (format outp "    opts          = OPTS_~A,\n" opts-tag))

              ;; (if prologue-nm
              ;;     (format outp "    opts          = [\"-open\", \"~A\"] + OPTS_~A,\n"
              ;;             (normalize-module-name prologue-nm) opts-tag)
              ;;     (format outp "    opts          = OPTS_~A,\n" opts-tag))
              ;; else no opts
              ;; (if prologue-nm
              ;;         (format outp "    opts          = [\"-open\", \"~A\"],~%" prologue-nm) ;; this-is-main)
              ;;         ;; else should not happen
              ;;         ;; (format outp "    opts          = OPTS_~A,\n" opts-tag))
              ;;         ))

          (if (or *mibl-debug-emit* *mibl-debug-s7*)
              (format #t "~A: ~A~%" (blue "emitting main deps B") deps-tag))

          (emit-deps outp
                      #t ;; this-is-main
                      #f ;; prologue-nm
                      deps-tag
                      ;; stanza
                      agg-deps local-deps dep-selectors testsuite)

          (if ppx-label
              (begin
                (mibl-trace "emitting main ppx" ppx-alist *mibl-debug-emit*)
                (format outp "    ppx           = \"~A\",\n" ppx-label)
                (mibl-trace "emitting main ppx-args" ppx-args *mibl-debug-emit*)
                ;;FIXME: handle :scope
                (if (truthy? ppx-args)
                    (format outp "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args))))

          ;; (if ppx-alist
          ;;     (begin
          ;;       (if (> (length pkg-shared-ppx-alist) 1)
          ;;           (format outp "    ppx           = \"~A:ppx_~A.exe\",\n" ppx-pkg ppx-id)
          ;;           (format outp "    ppx           = \"~A:ppx.exe\",\n" ppx-pkg))

          ;;       (if ppx-args
          ;;           (begin
          ;;             ;; (format #t "~A: ~A~%" "pkg" pkg)
          ;;             ;; (format #t "~A: ~A~%" "modname" modname)
          ;;             ;; (format outp
          ;;             ;;       "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args)
          ;;           (let* ((mod (assoc-in `(:modules ,modname) pkg))
          ;;                  (mfile (if mod
          ;;                             (car (assoc-val :ml (cdr mod)))
          ;;                             (let* ((mstruct (assoc-in `(:structures :static ,modname) pkg)))
          ;;                               (cadr mstruct))))
          ;;                  )
          ;;             (format outp
          ;;                     "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n"
          ;;                     (append (list "-loc-filename"
          ;;                                   (format #f "~A" mfile))
          ;;                             ppx-args))))
          ;;           )
          ;;       ;; (if ppx-args
          ;;       ;; ;; (if (not  ## why?
          ;;       ;; ;;      (equal? :all (cadr (assoc :scope
          ;;       ;; ;;                                ppx-alist))))
          ;;       ;;     (format outp
          ;;       ;;             "    ppx_args      = [~{~S, ~}], #A1\n" ppx-args
          ;;       ;;             ;;(cadr (assoc :args ppx-alist))
          ;;       ;;             ))
          ;;       ))

          (if ppx-codeps
              (format outp "    ppx_codeps    = [~{\"~S\"~^, ~}]\n" ppx-codeps))

          ;; (if prologue-nm
          ;;     (format outp "    visibility    = [\"//visibility:private\"]~%"))

          (format outp ")\n")
          ))

    (if *mibl-build-dyads*
        (if (alist? (cdr module-spec))
            (if (assoc :mli (cdr module-spec))
                (-emit-sig outp ws pkg module-spec stanza)))))
  stanza)

(define (bazel-emit-executable-target outp ws kind pkg stanza) ;; prologue)
  (if (or *mibl-debug-executables* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "bazel-emit-executable-target") stanza)
        (format #t "~A: ~A~%" (blue "kind") kind)))
        ;; (format #t "~A: ~A~%" (blue "prologue") prologue)))
  (let* ((stanza-alist (cdr stanza))

         (pkg-name (pkg->pkg-name pkg))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "~A: ~A~%" (blue "pkg-name") pkg-name)))

         (privname (assoc-val :privname stanza-alist))
         ;; (mainname (normalize-module-name privname))
         (findlib-name (if-let ((findlib-name (assoc-val :findlib-name stanza-alist)))
                          findlib-name
                          privname))
         (tgtname (format #f "~A" privname))
         (exename privname)

         ;; (modes (if-let ((ms (assoc-in '(:link :modes) stanza-alist)))
         ;;                (cdr ms) ms))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "MODES: ~A~%" (assoc-val :modes stanza-alist))))
         (modes (if-let ((ms (assoc-val :modes stanza-alist)))
                        ms '()))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "~A: ~A~%" (bgred "modes") modes)))
         ;; (_ (if (truthy? modes) (error 'X "X")))

         ;; 'name', i.e. main, is required by dune so we always have it
         ;; (main (cadr (assoc-in '(:link :main) stanza-alist)))
         (main (assoc-val :main stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "main: ~A~%" main)))

         ;; TODO: support *mibl-dune-prologue-includes-main*
         ;; (prologue (if *mibl-dune-prologue-includes-main* prologue (append prologue (list main))))
         (prologue (if-let ((p (assoc-val :prologue stanza-alist)))
                                  p #f))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "~A: ~A~%" (bgred "prologue") prologue)))

         (libdeps (if-let ((deps (assoc-in '(:deps :resolved) stanza-alist)))
                          (cdr deps) '()))
         (libdeps (if (number? libdeps)
                      (let ((shared-deps (assoc-in '(:mibl :shared-deps) pkg)))
                        (if (or *mibl-debug-executables* *mibl-debug-s7*)
                            (format #t "~A: ~A~%" (bggreen "share-deps")
                                    shared-deps))
                        libdeps)
                      libdeps))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "libdeps: ~A~%" libdeps)))

         ;; local module deps
         ;; (deps (assoc-in '(:compile :deps)))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "compile deps: ~A~%" deps)))
         (deps (sort! (if-let ((deps (assoc-in '(:deps :modules) stanza-alist)))
                             ;; (assoc-in '(:link :manifest :modules)
                             ;;           stanza-alist)))
                              (cdr deps) '())
                      sym<?))
         (deps (append deps (list main)))
         (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "local deps: ~A~%" deps)))
         ;; (deps (remove main deps))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debug-s7*) (format #t "deps: ~A~%" deps)))
         )
    (let-values (((link-std link-opts)
                  (link-flags->mibl stanza)))
      (if (or *mibl-debug-executables* *mibl-debug-s7*)
          (begin
            (format #t "link opts: ~A~%" link-opts)
            (format #t "link std: ~A~%" link-std)
            (format #t "TARGET: ~A\n" tgtname)
            ;; (format #t "MAIN: ~A\n" main)
            (format #t "DEPS: ~A\n" deps)
            ;; (format #t "SUBMs: ~A\n" submodules)
            ;; (format #t "DEPS: ~A\n" deps)
            ;; (error 'STOP "STOP exec linkflags")
            ))

      ;; (let-values (((flags opens) (stanza-opts stanza-alist)))
      ;;   (if (or flags opens
      ;;           (assoc-in '(:opts :raw) stanza-alist))
      ;;       (begin
      ;;         ;; (format #t "FLAGS: ~A\n" flags)
      ;;         ;; (format #t "OPENS: ~A\n" opens)

      ;;         (format outp "~A = [\n" (name->opts-sym findlib-name))
      ;;         (if flags
      ;;             (for-each (lambda (flag)
      ;;                         (format outp "    \"~A\",\n" flag))
      ;;                       (cadr flags)))
      ;;         (if opens
      ;;             (for-each (lambda (open)
      ;;                         (format outp "    \"-open\", \"~A\",\n" open))
      ;;                       (cadr opens)))

      ;;         (if (assoc-in '(:opts :raw :standard) stanza-alist)
      ;;             (format outp "    ##FIXME: dune (:standard)\n"))

      ;;         (format outp "]\n")
      ;;         (newline outp))))

      ;; for executables, 'deps' are libdeps ('libraries' fld), and
      ;; 'modules' are module (src) deps
      ;; (if (not (null? deps))
      ;; (begin
      ;;   (format outp "~A = [\n" (name->deps-sym findlib-name))
      ;;   (for-each (lambda (dep)
      ;;               (format outp "    \"~A\",\n" dep)
      ;;               )
      ;;             deps)
      ;;   (format outp "]\n")
      ;;   (newline outp))
      ;; ;; )

      (if (member 'js modes)
          (begin
            (if *mibl-js-emit-rules-js*
                (begin
                  (format outp "#############\n")
                  (case kind
                    ((:executable) (format outp "js_binary(\n"))
                    ((:test) (format outp "js_test(\n"))
                    (else (error 'fixme "unexpected kind for executable")))
                  (format outp "    name        = \"~A.bc.js\",\n" tgtname)

                  ;; (if (eq? kind :executable)
                  ;;     ;; do we always want this?
                  ;;     (begin
                  ;;       ;; attr 'exe': string name of outfile excluding extension,
                  ;;       ;; not a dependency
                  ;;       (format outp "    exe      = \"~A.exe.js\",\n" exename)
                  ;;       ;; (format outp "    main    = \":~A\",\n" mainname)
                  ;;       ))

                  (if (not (null? link-opts))
                      (format outp "    opts        = [~{\"~A\"~^, ~}],\n" link-opts))

                  (format outp "    entry_point = \":~A.exe.jsoo\",\n" tgtname)
                  (format outp "    visibility  = [\"//visibility:public\"],\n")
                  (format outp ")\n")
                  (newline outp))
                ;; else
                (if *mibl-js-emit-rules-swc*
                    (begin)
                    (if *mibl-js-emit-rules-closure*
                        (begin))))

            (if (or *mibl-js-emit-rules-jsoo*
                    *mibl-js-emit-rules-js*
                    *mibl-js-emit-rules-swc*
                    *mibl-js-emit-rules-closure*)
                (begin
                  ;; (format outp "#############\n")
                  (case kind
                    ((:executable) (format outp "jsoo_binary(\n"))
                    ((:test) (format outp "jsoo_test(\n"))
                    (else (error 'fixme "unexpected kind for executable")))
                  (format outp "    name     = \"~A.exe.jsoo\",\n" tgtname)

                  ;; (if (eq? kind :executable)
                  ;;     ;; do we always want this?
                  ;;     (begin
                  ;;       ;; attr 'exe': string name of outfile excluding extension,
                  ;;       ;; not a dependency
                  ;;       (format outp "    exe      = \"~A.exe.js\",\n" exename)
                  ;;       ;; (format outp "    main    = \":~A\",\n" mainname)
                  ;;       ))

                  (if (not (null? link-opts))
                      (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

                  (format outp "    main   = \":~A.exe\",\n" tgtname)
                  (format outp "    visibility = [\"//visibility:public\"],\n")
                  (format outp ")\n")
                  (newline outp)))))

      (begin
        (begin ;; (-emit-ocaml-binary ...)
          (case kind
            ((:executable)
             (format outp "##############\n")
             (format outp "ocaml_binary(\n"))
            ((:test)
             (format outp "###########\n")
             (format outp "ocaml_test(\n"))
            (else (error 'fixme "unexpected kind for executable")))

          (format outp "    name     = \"~A.exe\",\n" tgtname)

          (if (eq? kind :executable)
              (begin
                (if prologue
                    (begin
                      ;; (format outp "## DEBUG: prologue = ~A\n" prologue)
                      (if (number? prologue) ;; shared
                          (format outp "    prologue = [\":libPrologue_~A\"],\n" prologue)
                          ;; else local prologue, use :main name as id
                          (format outp "    prologue = [\":libPrologue_~A\"],\n" tgtname))))
                (if *mibl-dune-prologue-includes-main* ;; default: #f
                    (format outp "    main     = [\":__Lib_~A__\"],\n" tgtname)
                    (format outp "    main     = \":~A\",\n" main) ;;(normalize-module-name tgtname))
                    ;; else
                    )))

          (if (not (null? link-opts))
              (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

          ;; (format outp "    deps       = [\":__lib_~A__\"],\n" tgtname)
          (format outp "    visibility = [\"//visibility:public\"],\n")

          (if modes
              (if (equal? '(byte) modes)
                  (format outp "    target_compatible_with = [\"@ocaml//platforms:vm?\"]~%")))

          (format outp ")\n")) ;; end ocaml_binary

        (newline outp)

        ;; (-emit-main-module-ns outp tgtname stanza pkg)
        (-emit-main-module    outp ws stanza pkg)

                ;; (newline outp))
              ;; ;; else no main module?
              ;; (error 'No-main-module "No main module")
              ;; )

        (if prologue
            (begin
              (emit-local-prologue-lib outp tgtname pkg prologue)))
                ;; (-emit-local-prologue-ns outp tgtname pkg prologue)))
        )
      )))

(define (bazel-emit-shared-prologues outp ws pkg)
  (let* ((stanzas (assoc-val :mibl pkg))
         (pkg-name (pkg->pkg-name pkg))
         (shared-prologues (assoc-val :shared-prologues stanzas)))
    (if (or *mibl-debug-executables* *mibl-debug-s7*)
        (format #t "~A: ~A~%" (bgcyan "shared-prologues") shared-prologues))

    (if (truthy? shared-prologues)
        (begin
          (format outp "############################ Prologues ###############################~%")
          (for-each (lambda (prologue)
                      (if (or *mibl-debug-executables* *mibl-debug-s7*)
                          (begin
                            (format #t "~A: ~A~%" (ucyan "emitting prologue for pkg") pkg)
                            (format #t "~A: ~A~%" (ucyan "emitting prologue") prologue)))
                      ;; deps must be namespaced, or at least have unique names

                      (format outp "ocaml_ns_library(~%")
                      ;; (format outp "## prologue~%")
                      (format outp "    name = \"~A_prologue\",~%"
                              (normalize-module-name pkg-name))
                      ;; (format outp "    ns   = \"~A_prologue\",~%" tgtname)

                      ;; (if (not (null? deps))
                      ;;     (begin
                      (format outp "    manifest = [~%")
                      (if (or *mibl-debug-executables* *mibl-debug-s7*)
                          (format #t "~A: ~A~%" (bgcyan "prologue") prologue))
                      (format outp "~{        \":~A\"~^,~%~}~%" (assoc-val :modules (cdr prologue)))
                      (format outp "    ],\n")
                      ;; ))
                      (format outp "    visibility = [\"//visibility:private\"],~%")
                      (format outp ")")
                      (newline outp)
                      (newline outp))
                    shared-prologues)))))

;;FIXME: rename.  emit:mibl-pkg-executables->bazel?
;; iterate over :executable stanzas, emitting:
;;   a. ocaml_binary (or ocaml_test?) for :main
;;   b. ocaml_ns_resolver for :main-deps
;;   c. ocaml_ns_resolver for :prologue IF not shared
(define (bazel-emit-executable-targets outp ws pkg) ;;fs-path stanzas)
  (if (or *mibl-debug-executables* *mibl-debug-s7*)
      (format #t "~A\n" (ublue "bazel-emit-executable-targets")))

  ;; Multiple executables may share the same prologue. for example we
  ;; may have two (executables), each with n executables and a list
  ;; of (modules). Each (named) executable serves as main
  ;; exec-module; the list of (modules) constitute the prologue. In
  ;; this case we emit two ocaml_ns_library targets, one for each
  ;; prologue. Rather than one per main exec-module.

  ;; NB: all of this should have been resolved by mibl processing, so
  ;; here we just have to deal with :executable stanzas.

  ;; OUTDATED: Task: iterate the executables to collect the unique prologues.
  ;; Also: determine name for the prologue, each main exec-module
  ;; needs it (above). Since we may have n prologues for m main
  ;; exec-modules, we cannot use the main module names. We use the
  ;; directory name of the pkg with a suffixed serial number.

  ;; FIXME: support *mibl-dune-prologue-includes-main*, wherein each each
  ;; executable uses a prologue containing the main exec-module,
  ;; which means each gets its own prologue, with none shared.

  (let* ((stanzas (assoc-val :mibl pkg))
         (pkg-name (pkg->pkg-name pkg)))
    ;; (prologue (assoc-val :prologue stanzas)))
    ;; (if (or *mibl-debug-executables* *mibl-debug-s7*)
    ;;     (format #t "~A: ~A~%" (bgcyan "pkg prologue") prologue))

    ;; emit one ocaml_ns_resolver for all modules used by executables
    ;; including :main-deps and :prologue
    (-emit-executables-ns outp pkg)

    ;; now process :executable stanzas, passing the prologue
    (let* ((hdr-flag #t))
      (for-each (lambda (stanza)
                  ;; (format #t "stanza x: ~A ~A\n" fs-path (car stanza))
                  (case (car stanza)

                    ((:executable)
                     ;; (if hdr-flag
                     ;;     (begin
                     ;;       (format outp "############  Executable Targets  ############\n")
                     ;;       (set! hdr-flag #f)))

                     (bazel-emit-executable-target outp ws :executable pkg stanza))

                    ((:executables) (error 'bad-arg "unexpected :executables stanza"))
                    (else))) ;; ignore others
                stanzas))))

(if (or *mibl-debug-executables* *mibl-debug-s7*)
    (format #t "loaded bazel/executables_bazel.scm\n"))
