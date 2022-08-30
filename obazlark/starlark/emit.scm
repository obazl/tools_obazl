(format #t "loading starlark/conversions.scm\n")

(load "starlark/headers.scm")
(load "starlark/rules.scm")

;; FIXME: rename emit-starlark
(define (mibl-pkg->build-bazel ws pkg)
  (format #t "~A: ~A\n" (bgblue "mibl-pkg->build-bazel") pkg)
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (dunefile (assoc :dune pkg)))
    (if dunefile
        (let* ((stanzas (cadr dunefile))
               (obazl-rules (pkg->obazl-rules pkg))
               (_ (format #t "~A: ~A\n" (uwhite "obazl rules") obazl-rules))
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

          (format #t "emitting exports_files\n")
          (starlark-emit-exports-files outp pkg)

          (starlark-emit-global-vars outp pkg)

          (format #t "emitting executables\n")
          (starlark-emit-executable-targets outp pkg)

          (format #t "emitting aggregate targets (archive, library)\n")
          (starlark-emit-aggregate-targets outp pkg) ;;fs-path stanzas)

          (format #t "emitting singleton targets\n")
          (starlark-emit-singleton-targets outp pkg)
          ;; (starlark-emit-singleton-targets outp pkg-path stanzas
          ;;                                  (cdr pkg-kv))

          (format #t "emitting test targets\n")
          (starlark-emit-test-targets outp ws pkg)

          (format #t "emitting file generators\n")
          ;; ocamllex, ocamlyacc, etc.
          (starlark-emit-file-generators outp pkg)

          (format #t "emitting ppxes\n")
          (starlark-emit-ppxes outp pkg) ;;fs-path stanzas)

          (format #t "emitting rules\n")
          (starlark-emit-rule-targets outp pkg) ;; fs-path stanzas)

          ;; (format #t "emitting conditional deps\n")
          ;; (starlark-emit-conditionals outp ws pkg)

          (format #t "emitting filegroups\n")
          (starlark-emit-filegroups outp ws pkg)

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
        ;; no :dune, emit filegroups
        (let* ((build-file (string-append pkg-path "/BUILD.bazel"))
               (outp
                (catch #t
                       (lambda ()
                         (open-output-file build-file))
                       (lambda args
                         (format #t "OPEN ERROR"))
                       )))
          (format #t "emitting filegroups\n")
          (starlark-emit-filegroups outp ws pkg)
          (close-output-port outp)))))

(define (ws->starlark ws)
  (format #t "~A: ~A~%" (bgblue "ws->starlark") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    ;; if this is the root dunefile (w/sibling dune-project file)
    ;; and we have :env stanza, emit //profiles/BUILD.bazel

    (for-each (lambda (kv)
                (format #t "~A: ~S~%" (blue "emitting pkg") (car kv))
                (if (assoc 'dune-project (cdr kv))
                    (if (assoc-in '(:dune :env) (cdr kv))
                        (emit-profiles ws (cdr kv))))
                (if (not (null? (cdr kv)))
                    (mibl-pkg->build-bazel ws (cdr kv))
                    (format #t "~A: ~A~%" (blue "skipping") (car kv)))
                )
              pkgs)))


