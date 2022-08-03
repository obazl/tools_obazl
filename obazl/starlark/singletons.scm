(format #t "loading starlark/singletons.scm\n")

;; FIXME FIXME FIXME FIXME FIXME
;; lookup label for this sym
(define (-make-select-condition sym)
  (format #f "//foo/bar:~A" sym))

(define (make-selector module stanza)
  (format #t "make-selector: ~A, ~A\n" module stanza)
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
                                           (car (assoc-val :target c))))
                                    (cdr conditionals))))
      (format #t "the-conditional: ~A\n" the-conditional)
      (if the-conditional
          (let ((selectors (assoc-val :selectors the-conditional)))
            (format #t "selectors: ~A\n" selectors)
            (map (lambda (s) (list
                              (-make-select-condition (car s))
                              (cdr s)))
                 selectors))))))

;; WARNING: :modules have form (A (:ml a.ml)(:mli a.mli))
;; but :structures have form (A . a.ml)
(define (-emit-module outp module stanza)
  (format #t "~A: ~A [~A]\n" (blue "-emit-module") module stanza)

  (let* ((stanza-alist (cdr stanza))
         (_ (format #t "~A: ~A\n" "stanza-alist" stanza-alist))
         (libname (string-append
                   (string-upcase
                    (stringify
                     (assoc-val :privname stanza-alist)))))
         (_ (format #t "em libname: ~A~%" libname))

         (ns (assoc-val :ns stanza-alist))
         (_ (format #t "em ns: ~A~%" ns))

         (agg-deps (if-let ((deps (assoc-val :deps stanza-alist)))
                       deps
                       ;; else :executable or :test
                       ;; FIXME: should not be any deps in :compile ?
                       (if-let ((deps (assoc-in '(:compile :deps)
                                                stanza-alist)))
                               deps
                               '())))
         (_ (format #t "agg-deps: ~A~%" agg-deps))

         (local-deps (if (proper-list? module)
                         (if (alist? (cdr module))
                             ;; (A (:ml a.ml) (:mli a.mli) (:ml-deps...))
                             (if-let ((locals (assoc :ml-deps (cdr module))))
                                     (cdr locals)
                                     '())
                             ;; else (A a.ml Foo Bar ...)
                             (cdr module))
                         ;; else (A . a.ml) from :structures
                         ;; should not happen?
                         (cdr module)))

         (_ (format #t "~A: ~A~%" (red "local-deps") local-deps))

         (opts (if-let ((opts (assoc-val :opts stanza-alist)))
                       ;; aggregate
                       opts
                       ;; else executable
                       (if-let ((opts (assoc-in '(:compile :opts)
                                                stanza-alist)))
                               opts
                               '())))

         (_ (format #t "OPTS: ~A\n" opts))

         (ocamlc_opts (if (null? opts) '()
                          (if-let ((x (assoc-val :ocamlc (cdr opts))))
                                  (list (apply string-append
                                               (map stringify x)))
                                  '())))
         (_ (format #t "ocamlc_opts: ~A\n" ocamlc_opts))

         (ocamlopt_opts (if (null? opts) '()
                            (if-let ((flags (assoc-val :ocamlopt (cdr opts))))
                                    (list (apply string-append
                                                 (map stringify flags)))
                                    '())))
         (_ (format #t "ocamlopt_opts: ~A\n" ocamlopt_opts))

         (ppx-alist (if-let ((ppx (assoc :ppx stanza-alist)))
                            (cdr ppx) #f))
          ;; (module->ppx-alist fs-path mname stanzas))
         (_ (format #t "ppx-alist: ~A\n" ppx-alist))

         (ppx-name (if ppx-alist (format #f "~A.ppx" libname)))

         )

    (format #t "module libname: ~A~%" libname)
    (format #t "module ns: ~A~%" ns)

    (if (proper-list? module)
        (if (alist? (cdr module)) ;; :modules (A (:ml a.ml)(:mli a.mli))
            (let* ((_ (format #t "~A~%" (red "proper, alist")))
                   (modname (car module))
                   (srcs    (cdr module))
                   (select-sigfile (assoc-val :mli_ srcs))
                   (sigfile (if select-sigfile
                                (make-selector module stanza)
                                (assoc-val :mli srcs)))

                   (select-structfile (assoc-val :ml_ srcs))
                   (structfile (if select-structfile
                                   (make-selector module stanza)
                                   (assoc-val :ml srcs))))

              ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
              ;;         ;;               (cdr opts) '())))
              (format #t "emitting module: ~A: ~A\n" modname srcs)

              (format outp "ocaml_module(\n")
              (format outp "    name          = \"~A\",\n" modname)
              (if (and ns (not *ns-topdown*))
                  (format outp "    ns_resolver   = \":ns.~A\",\n" ns))

              (if (or select-structfile select-sigfile)
                  (format outp "    module        = \"~A\",\n" modname))

              (if select-structfile
                  (begin
                    (format outp "    struct        = select(~%")
                    (format outp "        {~%")
                    (format outp "~{~{~8T~S: \"~S\",~%~}~}" structfile)
                    (format outp "        },~%")
                    (format outp "        no_match_error=\"no file selected\"),\n")
                    )
                  (begin
                    (format outp "    struct        = \"~A\",\n" structfile)
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
              (if (not (null? opts))
                  (format outp "    opts          = ~A_OPTS,\n" libname))

              (if (not (null? ocamlc_opts))
                  (format outp "    opts_ocamlc   = ~A_OCAMLC_OPTS,\n"
                          libname))

              (if (not (null? ocamlopt_opts))
                  (format outp "    opts_ocamlopt = ~A_OCAMLOPT_OPTS,\n"
                          libname))

              ;; (if (not (null? agg-deps))
              ;;     (format outp "    deps          = ~A_DEPS,\n" libname))
               (format #t "~A: ~A~%" (red "local-deps") local-deps)
               (if (not (null? local-deps))
                   (if (not (null? agg-deps))
                       (begin
                         (format outp "    deps          = ~A_DEPS + [\n" libname)
                         (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                         (format outp "    ],\n"))
                       (begin
                         (format outp "    deps          = [\n" libname)
                         (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
                         (format outp "    ],\n")))

                   (if (not (null? agg-deps))
                         (format outp "    deps          = ~A_DEPS,\n" libname)))

              ;; (if (not (null? local-deps))
              ;;     (format outp "    local-deps          = ~A,\n" local-deps))

              (if ppx-alist
                  (begin
                    (format outp
                            "    ppx           = \":~A\",\n" ppx-name)
                    ;; (cadr (assoc :name ppx-alist)))
                    (if (not
                         (equal? :all (cadr (assoc :scope
                                                   ppx-alist))))
                        (format outp
                                "    ppx_args = [~{~S, ~}],\n"
                                (cadr (assoc :args ppx-alist))))))
              (format outp ")\n")
              )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; pkg dep: (Mytest mytest.ml Hello)
            (let* ((_ (format #t "~A~%" (red "proper, non-alist")))
                   (modname (car module))
                   (structfile (cadr module))
                   (local-deps (cddr module)))
               ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
               ;;               (cdr opts) '())))
               (format #t "Emitting module: ~A\n" modname)

               (format outp "ocaml_module(\n")
               (format outp "    name          = \"~A\",\n" modname)
               (if (and ns (not *ns-topdown*))
                   (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
               (format outp "    struct        = \"~A\",\n" structfile)

               (if (not (null? opts))
                   (format outp "    opts          = ~A_OPTS,\n" libname))

               (if (not (null? ocamlc_opts))
                   (format outp "    opts_ocamlc   = ~A_OCAMLC_OPTS,\n"
                           libname))

               (if (not (null? ocamlopt_opts))
                   (format outp "    opts_ocamlopt = ~A_OCAMLC_OPTS,\n"
                           libname))

               (format #t "~A: ~A~%" (red "local-deps") local-deps)
               (if (not (null? local-deps))
                   (if (not (null? agg-deps))
                       (begin
                         (format outp "    deps          = ~A_DEPS + [\n" libname)
                         (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
                         (format outp "    ],\n"))
                       (begin
                         (format outp "    deps          = [\n" libname)
                         (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
                         (format outp "    ],\n")))

                   (if (not (null? agg-deps))
                         (format outp "    deps          = ~A_DEPS,\n" libname)))

               (if ppx-alist
                   (begin
                     (format outp
                            "    ppx           = \":~A\",\n" ppx-name)
                     (if (not
                          (equal? :all (cadr (assoc :scope
                                                    ppx-alist))))
                         (format outp
                                 "    ppx_args = [~{~S, ~}],\n"
                                 (cadr (assoc :args ppx-alist))))))

               (format outp ")\n")
               ))
        ;; else (M . ml) from :structures
        (let* ((_ (format #t "~A~%" (red "improper")))
               (modname (car module))
               (structfile (cdr module))
               (local-deps '()))
          ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
          ;;               (cdr opts) '())))
          (format #t "Emitting module: ~A\n" modname)

          (format outp "ocaml_module(\n")
          (format outp "    name          = \"~A\",\n" modname)
          (if (and ns (not *ns-topdown*))
              (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
          (format outp "    struct        = \"~A\",\n" structfile)

          (if (not (null? opts))
              (format outp "    opts          = ~A_OPTS,\n" libname))

          (if (not (null? ocamlc_opts))
              (format outp "    opts_ocamlc   = ~A_OCAMLC_OPTS,\n"
                      libname))

          (if (not (null? ocamlopt_opts))
              (format outp "    opts_ocamlopt = ~A_OCAMLC_OPTS,\n"
                      libname))

          (format #t "~A: ~A~%" (red "local-deps") local-deps)
          (if (not (null? local-deps))
              (if (not (null? agg-deps))
                  (begin
                    (format outp "    deps          = ~A_DEPS + [\n" libname)
                    (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
                    (format outp "    ],\n"))
                  (begin
                    (format outp "    deps          = [\n" libname)
                    (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
                    (format outp "    ],\n")))

              (if (not (null? agg-deps))
                  (format outp "    deps          = ~A_DEPS,\n" libname)))

          (if ppx-alist
              (begin
                (format outp
                        "    ppx           = \":~A\",\n" ppx-name)
                (if (not
                     (equal? :all (cadr (assoc :scope
                                               ppx-alist))))
                    (format outp
                            "    ppx_args = [~{~S, ~}],\n"
                            (cadr (assoc :args ppx-alist))))))

          (format outp ")\n")
          ) ;; let*
        ))
  stanza)

(define (-emit-modules outp pkg modules)
  (format #t "~A: ~A\n" (blue "-emit-modules") modules)

  (for-each
   (lambda (module)
     (format #t "-emit-modules module: ~A\n" module)
     ;; (flush-output-port)
     (let* ((modname (car module))
            (aggregator (find-if
                         (lambda (stanza)
                           (format #t "checking stanza ~A\n" stanza)
                           (case (car stanza)
                             ((:ns-archive :ns-library)
                              (if-let ((submods
                                        (cdr (assoc-in '(:manifest :modules)
                                                       (cdr stanza)))))
                                      (begin
                                        (format #t "submods: ~A\n" submods)
                                        (if (member modname submods)
                                            ;;(-emit-module outp module stanza)
                                            #t #f))))
                             ((:archive :library)
                              (if-let ((submods
                                        (cdr (assoc-in '(:manifest :modules)
                                                  (cdr stanza)))))
                                      (begin
                                        (format #t "submods: ~A\n" submods)
                                        (if (member modname submods)
                                            ;;(-emit-module outp module stanza)
                                            #t #f))))
                             ((:executable :test)
                              (if-let ((submods
                                        (cdr (assoc-in '(:compile :manifest :modules) (cdr stanza)))))
                                      (begin
                                        (format #t "~A submods: ~A\n" (green (car stanza)) submods)
                                        (if (member modname submods)
                                            #t #f))))
                             (else #f)))
                         (assoc-val :dune pkg)))
            )
       (if aggregator
           (begin
             (format #t "Found containing aggregator for ~A: ~A\n"
                     modname aggregator)
             (-emit-module outp module aggregator)
             (format outp "\n"))
           (begin
             (format #t "No aggregator found for ~A; excluding\n" modname)))
       ))
   (sort! modules (lambda (a b) (sym<? (car a) (car b))))))

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
      (format outp "    opts          = ~A_OPTS,\n" libname)
      (format outp "    deps          = ~A_DEPS,\n" libname)

      (if ppx-alist
          (begin
            (format outp
                   "    ppx           = \":~A\",\n" ppx-name)
                    ;; (cadr (assoc :name ppx-alist)))
            (if (not
                 (equal? :all (cadr (assoc :scope
                                           ppx-alist))))
                (format outp
                        "    ppx_args = [~{~S, ~}],\n"
                        (cadr (assoc :args ppx-alist))))))
      (format outp ")\n\n")
      )))

(define (-emit-sigs-hdr outp sigs pkg-modules)
  (format #t "~A: sigs: ~A, pkg-modules: ~A\n" (blue "-emit-sigs-hdr")
          sigs pkg-modules)
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
  (format #t "-emit-signatures: ~A\n" sigs)
  (format #t "*build-dyads*: ~A\n" *build-dyads*)
  (format #t "pkg: ~A\n" pkg)

  (-emit-sigs-hdr outp sigs pkg-modules)

  (if *build-dyads*
      ;; WARNING: pkg-modules includes pkg-structs. Forms:
      ;; pkg-module: (A (:mli a.mli)(:ml a.ml))
      ;; pkg-struct: (A . a.ml)
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
       pkg-modules))

  (if sigs
      (for-each
       (lambda (sig)
         (format #t "sig: ~A\n" sig)
         (let* ((modname (car sig))
                (aggregator (find-if
                             (lambda (stanza)
                               (format #t "checking stanza ~A\n" stanza)
                               (case (car stanza)
                                 ((:archive :library)
                                  ;; (if (eq? :library (car stanza))
                                  (if-let ((subsigs
                                            (assoc-val :subsigs
                                                       (cdr stanza))))
                                          (begin
                                            (format #t "subsigs: ~A\n" subsigs)
                                            (if (member modname subsigs)
                                                (-emit-sig outp sig stanza)
                                                #f))))
                                 (else #f)))
                             (assoc-val :dune pkg)))
                )
           (format #t "aggregator: ~A\n" aggregator)
           ;; (format outp ")\n\n")
           ))
       sigs)))

;; (define (starlark-emit-singleton-targets outp fs-path stanzas dune-pkg)

(define (starlark-emit-singleton-targets outp pkg)
  (format #t "~A: ~A\n" (blue "starlark-emit-singleton-targets") pkg)

  ;; we emit targets for both static and generated source files; in
  ;; addition, we may have :indirect submodule deps (example:
  ;; src/lib_protocol_environment/sigs)

  (let* ((modules-static (if-let ((ms (assoc-in '(:modules) pkg)))
                                 (cdr ms) '()))
         (structs-static (if-let ((structs (assoc-in
                                            '(:structures :static) pkg)))
                                 (cdr structs) '()))
         (pkg-modules (concatenate modules-static structs-static))
         (sigs-static (if-let ((sigs (assoc-in
                                      '(:signatures :static) pkg)))
                              (cdr sigs) '()))
         (sigs sigs-static))

    (format #t "pkg-modules: ~A\n" pkg-modules)
    (format #t "sigs:    ~A\n" sigs)
    (format #t "structs-static: ~A\n" structs-static)

    (if (or (not (null? pkg-modules))
            (not (null? sigs)))
        (begin
          (format outp "#############################\n")
          (format outp "####  Singleton Targets  ####\n")
          (newline outp)))

    (if pkg-modules (-emit-modules outp pkg pkg-modules))
    (if (or sigs *build-dyads*)
        (-emit-signatures outp pkg sigs pkg-modules))
    ))

    ;; (let ((lib-stanzas (assoc+ :library stanzas)))
    ;;   (if (not (null? lib-stanzas))
    ;;       (for-each (lambda (stanza)
    ;;                   (newline outp)
    ;;                   (format outp "lib stanza: ~A\n"
    ;;                           (assoc 'name (cdr stanza)))
    ;;                   (let ((modules (assoc 'modules (cdr stanza)))
    ;;                         (flags (if-let
    ;;                                 ((flags (assoc 'flags (cdr stanza))))
    ;;                                 (cadr flags)
    ;;                                 '()))
    ;;                         (ocamlopt-flags
    ;;                          (if-let
    ;;                           ((flags (assoc 'ocamlopt_flags (cdr stanza))))
    ;;                           (cadr ocamlopt-flags)
    ;;                           '())))
    ;;                     (format outp "modules: ~A\n" modules)
    ;;                     (format outp "flags: ~A\n" flags)
    ;;                     (format outp "ocamlopt_flags: ~A\n" ocamlopt-flags)
    ;;                     ))
    ;;                 ;;(emit-lib-args fs-path stanza srcfiles out-port))
    ;;                 lib-stanzas))
    ;;   )
    ;; ))

(format #t "loaded starlark/singletons.scm\n")
