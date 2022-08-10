(format #t "loading starlark/conversions.scm\n")

(load "starlark/headers.scm")
(load "starlark/rules.scm")

(define (starlark-emit-global-vars outp pkg)
  (format #t "starlark-emit-global-vars: ~A\n" pkg)
  (for-each
   (lambda (stanza)
     (format #t "stanza: ~A~%" stanza)
     (case (car stanza)
       ((:archive :library :ns-archive ns-library)
        (let* ((libname (string-upcase
                         ;; privname or pubname?
                         (stringify (assoc-val :privname (cdr stanza)))))
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
               (_ (format #t "agg options: ~A\n" options))
               (standard (if (assoc :standard opts) #t #f))

               (deps-fixed (if-let ((df
                                     (assoc-in '(:deps :fixed) (cdr stanza))))
                                   (cdr df) #f)))

          (if deps-fixed
              (begin
                (format outp "~A_DEPS = [\n" libname)
                (format outp "~{        \"~A\"~^,\n~}\n" deps-fixed)
                (format outp "]\n")
                (format outp "\n")
              ))

          (if (not (null? options))
              (begin
                (format outp "~A_OPTS = [\n" libname)
                (format outp "~{        \"~A\"~^,\n~}\n" options)
                (format outp "]\n")
                (format outp "\n")
                )

              )))
       ((:executable :test)
        (format #t "exec globals\n")
        (let* ((libname (string-upcase
                         ;; privname or pubname?
                         (stringify (assoc-val :privname (cdr stanza)))))
               (_ (format #t "libname: ~A~%" libname))
               (opts (if-let ((opts (assoc-in '(:compile :opts)
                                              (cdr stanza))))
                             (cdr opts) '()))
               (_ (format #t "opts: ~A~%" opts))
               (opens (if-let ((opens (assoc-val :opens opts)))
                              (apply append (map (lambda (o)
                                                   (list "-open" (stringify o)))
                                                 opens))
                              '()))
               ;; (_ (format #t "opens: ~A~%" opens))
               (flags (if-let ((flags (assoc-val :flags opts)))
                              (list (apply string-append
                                           (map stringify flags)))
                              '()))

               (ocamlc_opts (if-let ((flags (assoc-val :ocamlc opts)))
                              (list (apply string-append
                                           (map stringify flags)))
                              '()))
               (_ (format #t "g ocamlc_opts: ~A\n" ocamlc_opts))

               (ocamlopt_opts (if-let ((flags (assoc-val :ocamlopt opts)))
                              (list (apply string-append
                                           (map stringify flags)))
                              '()))
               (_ (format #t "g ocamlopt_opts: ~A\n" ocamlopt_opts))

               (options (apply append (list opens flags)))
               (_ (format #t "exe options: ~A\n" options))
               (standard (if (assoc :standard opts) #t #f))

               (deps-fixed (if-let ((df
                                     ;;(assoc-in '(:link :deps :fixed)
                                     (assoc-in '(:compile :deps :fixed)
                                               (cdr stanza))))
                                   (cdr df) #f)))

          (if deps-fixed
              (format outp "~A_DEPS = [~{\"~A\"~^, ~}]\n\n"
                      libname deps-fixed))
          (if (not (null? options))
              (format outp "~A_OPTS = [~{\"~A\"~^, ~}]\n\n"
                      libname options))
          (if (not (null? ocamlc_opts))
              (format outp "~A_OCAMLC_OPTS = [~{\"~A\"~^, ~}]\n\n"
                      libname ocamlc_opts))
          (if (not (null? ocamlopt_opts))
              (format outp "~A_OCAMLOPT_OPTS = [~{\"~A\"~^, ~}]\n\n"
                      libname ocamlopt_opts))))
       ))
   (assoc-val :dune pkg)))

(define (-pkg->obazl-rules pkg)
  (format #t "~A~%" (blue "-pkg->obazl-rules"))
  (let* ((stanzas (assoc-val :dune pkg))
         ;; (_ (format #t "stanzas: ~A\n" stanzas))
         (-rules (fold (lambda (stanza accum)
                         (format #t "  ~A: ~A\n" (blue "stanza") stanza)
                         (let* ((stanza-alist (cdr stanza))
                                (_ (format #t "~A: ~A~%" (yellow "stanza-alist") stanza-alist))
                                (dune-rule (car stanza))
                                (_ (format #t "~A: ~A~%" (yellow "dune-rule") dune-rule))
                                (rule (case dune-rule
                                        ((:rule)
                                         (let* ((actions (assoc :actions stanza-alist))
                                                (cmd-list (assoc-in* '(:actions :cmd) stanza-alist))
                                                (_ (format #t "~A: ~A~%" (green "cmd-list") cmd-list))
                                                (cmd-ct (length cmd-list)))
                                           (if (> cmd-ct 1)
                                               (fold (lambda (cmd accum)
                                                       (format #t "~A: ~A~%" (red "cmd") cmd)
                                                       (let ((tool (car (assoc-val :tool (cdr cmd)))))
                                                         (case tool
                                                           ((:write-file)
                                                            (if (member :write-file accum)
                                                                accum (cons :write-file accum)))
                                                           (else accum))))
                                                     '() cmd-list)
                                               (list dune-rule))))
                                        ((:write-file) (cons :write-file accum))
                                        (else
                                         (list dune-rule))))
                                (accum (if rule (append rule accum) accum))
                                ;; (accum (if (assoc :namespaced s-alist)
                                ;;            (cons :namespaced accum)
                                ;;            accum))
                                (accum (if (assoc :ppx stanza-alist)
                                           (cons :ppx accum)
                                           accum))
                                )
                           accum))
                       '() stanzas)))
    (let* ((rules (if (assoc :modules pkg) (cons :module -rules) -rules))
           (rules (if (assoc :structures pkg) (cons :module rules) rules))
           (rules (if (assoc :signatures pkg) (cons :sig rules) rules)))
      (format #t "~A: ~A~%" (red "obazlrules") rules)
      rules)))

;; FIXME: rename emit-starlark
(define (mibl-pkg->build-bazel pkg)
  (format #t "mibl-pkg->build-bazel: ~A\n" pkg)
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

          (format #t "emitting executables\n")
          (starlark-emit-executable-targets outp pkg)

          (format #t "emitting aggregate targets (archive, library)\n")
          (starlark-emit-aggregate-targets outp pkg) ;;fs-path stanzas)

          (format #t "emitting singleton targets\n")
          (starlark-emit-singleton-targets outp pkg)
          ;; (starlark-emit-singleton-targets outp pkg-path stanzas
          ;;                                  (cdr pkg-kv))

          (format #t "emitting test targets\n")
          (starlark-emit-test-targets outp pkg)

          ;; (format #t "emitting file generators\n")
          ;; ocamllex, ocamlyacc, etc.
          ;; (starlark-emit-file-generators outp fs-path stanzas)

          (format #t "emitting ppxes\n")
          (starlark-emit-ppxes outp pkg) ;;fs-path stanzas)

          (format #t "emitting rules\n")
          (starlark-emit-rule-targets outp pkg) ;; fs-path stanzas)

          (format #t "emitting filegroups\n")
          (starlark-emit-filegroups outp pkg)

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
          (starlark-emit-filegroups outp pkg)
          (close-output-port outp)))))
