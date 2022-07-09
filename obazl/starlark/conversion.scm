(define (mibl->starlark pkg)
  (format #t "mibl-starlark\n")
  (let* ((fs-path (car (assoc-val :pkg-path pkg)))
         (dunefile (assoc :dune pkg)))
      (if (cadr dunefile)
          (let* ((stanzas (cadr dunefile))
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

            (starlark-emit-build-file-hdr outp pkg-kv)
            ;; (newline outp)

            ;; (format #t "emitting executables\n")
            (starlark-emit-executable-targets outp fs-path stanzas)

            (format #t "emitting aggregates - 'library' stanzas\n")
            (starlark-emit-aggregate-targets outp fs-path stanzas)

            ;; (format #t "emitting module files\n")
            (starlark-emit-file-targets outp fs-path stanzas (cdr pkg-kv))

            ;; (format #t "emitting file generators\n")
            ;; ocamllex, ocamlyacc, etc.
            (starlark-emit-file-generators outp fs-path stanzas)

            ;; (format #t "emitting ppxes\n")
            (starlark-emit-ppxes outp fs-path stanzas)

            ;; (format #t "emitting rules\n")
            (starlark-emit-rule-targets outp fs-path stanzas)

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
          (begin
            (format #t "NULL stanzas: ~A\n" fs-path)
            (let* ((build-file (string-append fs-path "/BUILD.bazel"))
                   (outp
                    (catch #t
                           (lambda ()
                             (open-output-file build-file))
                           (lambda args
                             (apply format #t (cadr args)))
                           )))
              (starlark-emit-null-stanzas outp fs-path pkg-kv)
              (close-output-port outp)))
          )))
