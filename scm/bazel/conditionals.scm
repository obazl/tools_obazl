(define (starlark-emit-select-flags outp ws pkg)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "starlark-emit-select-flags") pkg))

  (if (not (null? *select-protases*))
      (begin
        (system "mkdir -p bzl/import")
        (let ((outp
               (catch #t
                      (lambda ()
                        (open-output-file "bzl/import/BUILD.bazel"))
                      (lambda args
                        (error 'OPEN_ERR_COND "OPEN ERROR COND"))
                      )))

          (format outp
                  "load(\"@bazel_skylib//rules:common_settings.bzl\", \"bool_flag\")")
          (newline outp)
          (format outp "package(default_visibility = [\"//visibility:public\"])")
          (newline outp)
          (newline outp)

          (for-each
           (lambda (key)
             (format outp
                     "bool_flag( name = \"~A\", build_setting_default = False )"
                     key)
             (newline outp)
             (format outp "config_setting(name = \"~A?\"," key)
             (newline outp)
             (format outp "               flag_values = {\":~A\": str(True)})" key)
             (newline outp)
             (newline outp))
           (remove-duplicates *select-protases*))
          (close-output-port outp)))))

  ;; (format #t "~A: ~A~%" (red "emitting keys") *select-protases*)
  ;; (for-each
  ;;  (lambda (key)
  ;;    (format #t "~A: ~A~%" (red "key") key)
  ;;    (let* ((builddir (format #f "bzl/import/~A" key))
  ;;           (buildfile (format #f "bzl/import/~A/BUILD.bazel" key))
  ;;           (_ (format #t "~A: ~A~%" (red "buildfile") buildfile))
  ;;           (_ (system (format #f "mkdir -p ~A" builddir)))
  ;;           (outp
  ;;            (catch #t
  ;;                   (lambda ()
  ;;                     (open-output-file buildfile))
  ;;                   (lambda args
  ;;                     (format #t
  ;;                             "OPEN ERROR: ~A" buildfile)))))
  ;;   (format outp
  ;;           "load(\"@bazel_skylib//rules:common_settings.bzl\", \"bool_flag\")")
  ;;   (newline outp)
  ;;   (format outp "package(default_visibility = [\"//visibility:public\"])")
  ;;   (newline outp)
  ;;   (newline outp)
  ;;   (format outp
  ;;              "bool_flag( name = \"~A\", build_setting_default = False )"
  ;;              key)
  ;;   (close-output-port outp)))
  ;;  (remove-duplicates *select-protases*))
  ;; )

(define (starlark-emit-conditionals outp ws pkg)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (bgred "starlark-emit-conditionals") pkg))
  (for-each
   (lambda (stanza)
     (if *mibl-debug-s7*
         (format #t "~A: ~A~%" (uwhite "stanza") stanza))
     (case (car stanza)
       ((:archive :library :ns-archive :ns-library)
        (let* ((hdr-flag #t)
               (libname (string-upcase
                         ;; privname or findlib-name?
                         (stringify (assoc-val :privname (cdr stanza)))))
               ;; compile-options is an alist,
               ;; keys :generic, :ocamlc, :ocamlopt
               (archive-options (-get-archive-opts (cdr stanza)))
               (_ (if *mibl-debug-s7*
                      (format #t "~A: ~A~%" (bgyellow "archive-options")
                          archive-options)))
               (compile-options (get-compile-opts (cdr stanza) pkg))
               (_ (if *mibl-debug-s7*
                      (format #t "~A: ~A~%" (bgyellow "compile-options")
                              compile-options)))
               (exec-options (-get-exec-opts (cdr stanza)))
               (_ (if *mibl-debug-s7*
                      (format #t "~A: ~A~%" (bgyellow "exec-options")
                              exec-options)))

               (deps-conditional (if-let ((dc
                                           (assoc-in '(:deps :conditionals)
                                                     (cdr stanza))))
                                         dc #f))
               )
          (if *mibl-debug-s7*
              (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional))

          (if deps-conditional
              (let ((outfile (assoc-val :target (cadr deps-conditional)))
                    (selectors (flatten
                                (map
                                 (lambda (sel)
                                   (cons (format #f "~A_pred" (car sel))
                                         (cdr sel)))
                                 (assoc-val :selectors (cadr deps-conditional))))))
                ;; (format #t "~A: ~A~%" (bgred "selectors") selectors)
                ;; (error 'stop "STOP agg conditionals")

                (if hdr-flag
                    (format outp
                            "load(\"@bazel_skylib//rules:copy_file.bzl\", \"copy_file\")~%"))
                (format outp "copy_file = [~%")
                (format outp "    name = \"select_~S\",~%" outfile)
                (format outp "    src = select({~%")
                (format outp "~{        \":~A\": \"~A\"~^,\n~}\n"
                        selectors)
                        ;; (map car selectors)
                        ;; (map cdr selectors))
                (format outp "    }),~%")
                (format outp "    out = \"~A\"~%" outfile)
                (format outp ")~%")))
          ))

         ((:executable :test)
          (if *mibl-debug-s7* (format #t "exec globals\n"))
          (let* ((libname (string-upcase
                           ;; privname or findlib-name?
                           (stringify (assoc-val :privname (cdr stanza)))))
                 (_ (if *mibl-debug-s7* (format #t "libname: ~A~%" libname)))
                 (opts (if-let ((opts (assoc-in '(:compile :opts)
                                                (cdr stanza))))
                               (cdr opts) '()))
                 (_ (if *mibl-debug-s7* (format #t "opts: ~A~%" opts)))
                 (opens (if-let ((opens (assoc-val :opens opts)))
                                (apply append (map (lambda (o)
                                                     (list "-open" (stringify o)))
                                                   opens))
                                '()))
                 ;; (_ (if *mibl-debug-s7* (format #t "opens: ~A~%" opens)))
                 (flags (if-let ((flags (assoc-val :flags opts)))
                                (list (apply string-append
                                             (map stringify flags)))
                                '()))

                 (ocamlc_opts (if-let ((flags (assoc-val :ocamlc opts)))
                                      (list (apply string-append
                                                   (map stringify flags)))
                                      '()))
                 (_ (if *mibl-debug-s7* (format #t "g ocamlc_opts: ~A\n" ocamlc_opts)))

                 (ocamlopt_opts (if-let ((flags (assoc-val :ocamlopt opts)))
                                        (list (apply string-append
                                                     (map stringify flags)))
                                        '()))
                 (_ (if *mibl-debug-s7* (format #t "g ocamlopt_opts: ~A\n" ocamlopt_opts)))

                 (options (apply append (list opens flags)))
                 (_ (if *mibl-debug-s7* (format #t "exe options: ~A\n" options)))
                 (standard (if (assoc :standard opts) #t #f))

                 (deps-fixed (if-let ((df
                                       ;;(assoc-in '(:link :deps :remote)
                                       (assoc-in '(:compile :deps :resolved)
                                                 (cdr stanza))))
                                     (cdr df) #f))
                 (deps-conditional (if-let ((dc
                                             (assoc-in '(:deps :conditionals)
                                                       (cdr stanza))))
                                           dc #f))
                 )
            (if deps-conditional
                (begin
                  (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional)
                  (error 'stop "STOP exec conditionals")))
            ))

         ((:env :lex :yacc :testsuite) (values))

         (else
          (error 'UNHANDLED "unhandled stanza for conditionals"))
         ))
   (assoc-val :mibl pkg)))
