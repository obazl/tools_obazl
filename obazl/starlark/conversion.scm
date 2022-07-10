(format #t "loading starlark/conversions.scm\n")

(load "starlark/templates.scm")

(define (starlark-emit-global-vars outp pkg)
  (format #t "starlark-emit-global-vars: ~A\n" pkg)
  (for-each
   (lambda (stanza)
     (let* ((libname (string-upcase
                      (stringify (car (assoc-val :privname (cdr stanza))))))
            (opts (if-let ((opts (assoc :opts (cdr stanza))))
                          (cdr opts) '()))
            (opens (if-let ((opens (assoc-val :opens opts)))
                           (apply append (map (lambda (o)
                                                (list "-open" (stringify o)))
                                              opens))
                           '()))
            (flags (if-let ((flags (assoc-val :flags opts)))
                           (list (apply string-append
                                        (map stringify flags)))
                           '()))
            (options (apply append (list opens flags)))
            (_ (format #t "options: ~A\n" options))
            (standard (if (assoc :standard opts) #t #f))

            (deps-fixed (cdr (assoc-in '(:deps :fixed) (cdr stanza)))))

       (format outp "~A_DEPS = [~{\"~A\"~^, ~}]\n\n" libname deps-fixed)
       (format outp "~A_OPTS = [~{\"~A\"~^, ~}]\n\n" libname options)))

   (assoc-val :dune pkg)))

(define (-pkg->obazl-rules pkg)
  (let* ((stanzas (assoc-val :dune pkg))
         (_ (format #t "stanzas: ~A\n" stanzas))
         (-rules (fold (lambda (s accum)
                        (format #t "stanza: ~A\n" s)
                        (let* ((s-alist (cdr s))
                               (rule (case (car s)
                                       ((:library)
                                        (if (assoc :namespaced s-alist)
                                            :ns-archive :library))
                                       (else :unknown)))
                               (accum (if rule (cons rule accum) accum))
                               ;; (accum (if (assoc :namespaced s-alist)
                               ;;            (cons :namespaced accum)
                               ;;            accum))
                               )
                          accum))
                      '() stanzas)))
    (let* ((rules (if (assoc :modules pkg) (cons :module -rules) -rules))
           (rules (if (assoc :structures pkg) (cons :module rules) rules))
           (rules (if (assoc :signatures pkg) (cons :sig rules) rules)))
    rules)))

(define (mibl-pkg->starlark pkg)
  (format #t "mibl-pkg->starlark: ~A\n" pkg)
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (dunefile (assoc :dune pkg)))
      (if dunefile
          (let* ((stanzas (cadr dunefile))
                 (obazl-rules (-pkg->obazl-rules pkg))
                 (_ (format #t "obazl rules: ~A\n" obazl-rules))
                 (build-file (string-append pkg-path "/BUILD.bazel"))

                 (outp
                  (catch #t
                         (lambda ()
                           (open-output-file build-file))
                         (lambda args
                           (format #t "OPEN ERROR"))
                         )))
            (format #t "\nEmitting ~A, port ~A\n" build-file outp)

            (starlark-emit-buildfile-hdr outp obazl-rules)
            ;; (newline outp)

            (starlark-emit-global-vars outp pkg)

            ;; (format #t "emitting executables\n")
            ;; (starlark-emit-executable-targets outp fs-path stanzas)

            ;; (format #t "emitting aggregates - 'library' stanzas\n")
            ;; (starlark-emit-aggregate-targets outp fs-path stanzas)

            (format #t "emitting module targets\n")
            (starlark-emit-singleton-targets outp pkg)
            ;; (starlark-emit-singleton-targets outp pkg-path stanzas
            ;;                                  (cdr pkg-kv))

            ;; (format #t "emitting file generators\n")
            ;; ocamllex, ocamlyacc, etc.
            ;; (starlark-emit-file-generators outp fs-path stanzas)

            ;; (format #t "emitting ppxes\n")
            ;; (starlark-emit-ppxes outp fs-path stanzas)

            ;; (format #t "emitting rules\n")
            ;; (starlark-emit-rule-targets outp fs-path stanzas)

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

            )
          ;; else null :stanzas
          ;; (begin
          ;;   (format #t "NULL stanzas: ~A\n" fs-path)
          ;;   (let* ((build-file (string-append fs-path "/BUILD.bazel"))
          ;;          (outp
          ;;           (catch #t
          ;;                  (lambda ()
          ;;                    (open-output-file build-file))
          ;;                  (lambda args
          ;;                    (apply format #t (cadr args)))
          ;;                  )))
          ;;     (starlark-emit-null-stanzas outp fs-path pkg-kv)
          ;;     (close-output-port outp)))
          )))

(format #t "loaded starlark/conversions.scm\n")
