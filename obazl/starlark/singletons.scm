(format #t "loading starlark/singletons.scm\n")

;; optimizations: memoize opts and deps

(define (-emit-module outp module stanza)
  (format #t "-emit-module: ~A\n" stanza)
  (let* ((modname (car module))
         (srcs    (cdr module))
         (sigfile (car (assoc-val :mli srcs)))
         (structfile (car (assoc-val :ml srcs)))
         (libname (string-append
                   (string-upcase
                    (stringify
                     (car (assoc-val :privname (cdr stanza))))))))
         ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
         ;;               (cdr opts) '())))
    (format #t "emitting module: ~A: ~A\n" modname srcs)

    (format outp "ocaml_module(\n")
    (format outp "    name     = \"~A\",\n" modname)
    (format outp "    struct   = \"~A\",\n" structfile)
    ;; if direct sigdeps
    (format outp "    sig      = \"~A\",\n" sigfile)
    ;; else
    ;; (format outp "    ## sig      = \":~A_cmi\",\n" modname)
    (format outp "    opts      = ~A_OPTS,\n" libname)
    (format outp "    deps      = ~A_DEPS,\n" libname)

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
    ))

(define (-emit-modules outp modules)
  (format #t "-emit-modules: ~A\n" modules)
  (format outp "#############################\n")
  (format outp "####  Module Targets  ####\n")
  (newline outp)

  (for-each
   (lambda (module)
     (format #t "module: ~A\n" module)
     ;; (flush-output-port)
     (let* ((modname (car module))
            (aggregator (find-if
                         (lambda (stanza)
                           (format #t "checking stanza ~A\n" stanza)
                           (case (car stanza)
                             ((:library)
                              ;; (if (eq? :library (car stanza))
                              (if-let ((submods
                                        (assoc-val :submodules
                                                   (cdr stanza))))
                                      (begin
                                        (format #t "submods: ~A\n" submods)
                                        (if (member modname submods)
                                            (-emit-module outp module stanza)
                                            #f))))
                             (else #f)))
                         (assoc-val :dune pkg)))
            )
       (format #t "aggregator: ~A\n" aggregator)
       (format outp ")\n\n")))
   modules))

(define (-emit-sig outp sig stanza)
  (format #t "-emit-sig: ~A, ~A\n" sig stanza)
  (let* ((modname (car sig))
         (srcs    (cdr sig))
         (sigfile (car (assoc-val :mli srcs)))
         (libname (string-append
                   (string-upcase
                    (stringify
                     (car (assoc-val :privname (cdr stanza))))))))
         ;; (optsvar (string-append (string-upcase (stringify libname))
         ;;                         "_OPTS")))
    (format #t "emitting sig: ~A: ~A\n" modname srcs)

    (format outp "ocaml_signature(\n")
    (format outp "    name     = \"~A\",\n" modname)
    (format outp "    sig      = \"~A\",\n" sigfile)

    (format outp "    opts      = ~A_OPTS,\n" libname)
    (format outp "    deps      = ~A_DEPS,\n" libname)

    ;; (let* ((opens (if-let ((opens (assoc-val :opens opts)))
    ;;                       (apply append (map (lambda (o)
    ;;                                            (list "-open" (stringify o)))
    ;;                                          opens))
    ;;                       '()))
    ;;        (flags (if-let ((flags (assoc-val :flags opts)))
    ;;                       (list (apply string-append
    ;;                                    (map stringify flags)))
    ;;                       '()))
    ;;        (options (apply append (list opens flags)))
    ;;        (_ (format #t "options: ~A\n" options))
    ;;        (standard (if (assoc :standard opts) #t #f)))
    ;;   (format outp "    opts      = [~{\"~A\"~^, ~}],\n" options))



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
    ))

(define (-emit-signatures outp sigs)
  (format #t "-emit-sigs: ~A\n" sigs)
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
                             ((:library)
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
  (format #t "starlark-emit-singleton-targets: ~A\n" pkg)

  ;; we emit targets for both static and generated source files; in
  ;; addition, we may have :indirect submodule deps (example:
  ;; src/lib_protocol_environment/sigs)

  (let* ((modules-static (if-let ((ms (assoc-in '(:modules :static) pkg)))
                                 (cdr ms) '()))
         (structs-static (if-let ((structs (assoc-in
                                            '(:structures :static) pkg)))
                                 (cdr structs) '()))
         (modules (apply append modules-static structs-static))
         (sigs-static    (if-let ((sigs (assoc-in
                                         '(:signatures :static) pkg)))
                                 (cdr sigs) #f))
         (sigs sigs-static))

    (format #t "modules: ~A\n" modules)
    (format #t "sigs:    ~A\n" sigs)
    (format #t "structs-static: ~A\n" structs-static)

    (-emit-modules outp modules)
    (-emit-signatures outp sigs)
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
