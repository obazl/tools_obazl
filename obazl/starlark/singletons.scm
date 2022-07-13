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
         )
    (format #t "module libname: ~A~%" libname)
    (format #t "module ns: ~A~%" ns)

    (if (proper-list? module) ;; (A (:ml a.ml)(:mli a.mli)), from :modules
        (let* ((modname (car module))
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
                ))

          ;; (format outp "    ## sig      = \":~A_cmi\",\n" modname)
          (if (not (null? opts))
              (format outp "    opts          = ~A_OPTS,\n" libname))

          (if (not (null? ocamlc_opts))
              (format outp "    opts_ocamlc   = ~A_OCAMLC_OPTS,\n"
                      libname))

          (if (not (null? ocamlopt_opts))
              (format outp "    opts_ocamlopt = ~A_OCAMLOPT_OPTS,\n"
                      libname))

          (format outp "    deps          = ~A_DEPS,\n" libname)

  ;;         ;; (if ppx-alist
  ;;         ;;     (begin
  ;;         ;;       (format outp
  ;;         ;;               "    ppx      = \":~A\",\n"
  ;;         ;;               (cadr (assoc :name ppx-alist)))
  ;;         ;;       (if (not
  ;;         ;;            (equal? :all (cadr (assoc :scope
  ;;         ;;                                      ppx-alist))))
  ;;         ;;           (format outp
  ;;         ;;                   "    ppx_args = [~{~S, ~}],\n"
  ;;         ;;                   (cadr (assoc :args ppx-alist))))))
          (format outp ")\n")

          )
        ;; else (M . ml) from :structures
        (let* ((modname (car module))
               (structfile (cdr module)))

               ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
               ;;               (cdr opts) '())))
               (format #t "emitting module: ~A\n" modname)

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

               (format outp "    deps          = ~A_DEPS,\n" libname)

               ;; (if ppx-alist
               ;;     (begin
               ;;       (format outp
               ;;               "    ppx      = \":~A\",\n"
               ;;               (cadr (assoc :name ppx-alist)))
               ;;       (if (not
               ;;            (equal? :all (cadr (assoc :scope
               ;;                                      ppx-alist))))
               ;;           (format outp
               ;;                   "    ppx_args = [~{~S, ~}],\n"
               ;;                   (cadr (assoc :args ppx-alist))))))
               (format outp ")\n")
               ) ;; let*
        ))
  stanza)

(define (-emit-modules outp modules)
  (format #t "~A: ~A\n" (blue "-emit-modules") modules)

  (for-each
   (lambda (module)
     (format #t "module: ~A\n" module)
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
                             ((:executable)
                              (if-let ((submods
                                        (cdr (assoc-in '(:compile :manifest :modules) (cdr stanza)))))
                                      (begin
                                        (format #t "submods: ~A\n" submods)
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

(define (-emit-sig outp sig stanza)
  (format #t "~A: ~A, ~A\n" (blue "-emit-sig") sig stanza)
  (let ((libname (string-append
                  (string-upcase
                   (stringify
                    (car (assoc-val :privname (cdr stanza)))))))
        (ns (assoc-val :ns (cdr stanza))))

    (let* ((modname (car sig))
           (sigfile (cdr sig)))

      (format #t "emitting signature: ~A\n" modname)

      (format outp "ocaml_signature(\n")
      (format outp "    name          = \"~A\",\n" modname)
      (if (and ns (not *ns-topdown*))
          (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
      (format outp "    sig           = \"~A\",\n" sigfile)
      (format outp "    opts          = ~A_OPTS,\n" libname)
      (format outp "    deps          = ~A_DEPS,\n" libname)

      ;; (if ppx-alist
      ;;     (begin
      ;;       (format outp
      ;;               "    ppx      = \":~A\",\n"
      ;;               (cadr (assoc :name ppx-alist)))
      ;;       (if (not
      ;;            (equal? :all (cadr (assoc :scope
      ;;                                      ppx-alist))))
      ;;           (format outp
      ;;                   "    ppx_args = [~{~S, ~}],\n"
      ;;                   (cadr (assoc :args ppx-alist))))))

      )))

(define (-emit-signatures outp sigs)
  (format #t "-emit-signatures: ~A\n" sigs)
  (format outp "#############################\n")
  (format outp "####  Signature Targets  ####\n")
  (newline outp)

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
       (format outp ")\n\n")))
   sigs))

;; (define (starlark-emit-singleton-targets outp fs-path stanzas dune-pkg)

(define (starlark-emit-singleton-targets outp pkg)
  (format #t "~A: ~A\n" (blue "STARLARK-EMIT-singleton-targets") pkg)

  ;; we emit targets for both static and generated source files; in
  ;; addition, we may have :indirect submodule deps (example:
  ;; src/lib_protocol_environment/sigs)

  (let* ((modules-static (if-let ((ms (assoc-in '(:modules :static) pkg)))
                                 (cdr ms) '()))
         (structs-static (if-let ((structs (assoc-in
                                            '(:structures :static) pkg)))
                                 (cdr structs) '()))
         (modules (concatenate modules-static structs-static))
         (sigs-static    (if-let ((sigs (assoc-in
                                         '(:signatures :static) pkg)))
                                 (cdr sigs) #f))
         (sigs sigs-static))

    (format #t "modules: ~A\n" modules)
    (format #t "sigs:    ~A\n" sigs)
    (format #t "structs-static: ~A\n" structs-static)

    (if (or modules sigs)
        (begin
          (format outp "#############################\n")
          (format outp "####  Singleton Targets  ####\n")
          (newline outp)))

    (if modules (-emit-modules outp modules))
    (if sigs (-emit-signatures outp sigs))
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
