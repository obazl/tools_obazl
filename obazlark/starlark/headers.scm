(format #t "loading starlark/headers.scm\n")

(define (pkg->obazl-rules pkg)
  (format #t "~A~%" (ublue "-pkg->obazl-rules"))
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
                                           (format #t "~A: ~A~%" (green "cmd-ct") cmd-ct)
                                           ;; (if (> cmd-ct 1)
                                           (fold (lambda (cmd accum)
                                                   (format #t "~A: ~A~%" (red "cmd") cmd)
                                                   (let ((tool (car (assoc-val :tool (cdr cmd)))))
                                                     (case tool
                                                       ((:copy ::copy)
                                                        (if (member :copy accum)
                                                            accum (cons :copy accum)))
                                                       ((:write-file)
                                                        (if (member :write-file accum)
                                                            accum (cons :write-file accum)))
                                                       (else accum))))
                                                 '() cmd-list)))
                                        ((:ocamllex) (cons :ocamllex accum))
                                        ((:ocamlyacc) (cons :ocamlyacc accum))
                                        ((:write-file) (cons :write-file accum))
                                        (else
                                         (list dune-rule))))
                                (accum (if rule (append rule accum) accum))
                                ;; (accum (if (assoc :namespaced s-alist)
                                ;;            (cons :namespaced accum)
                                ;;            accum))
                                (accum (if (alist? stanza-alist)
                                           (if (assoc :ppx stanza-alist)
                                               (cons :ppx accum)
                                               accum)
                                           accum))
                                )
                           accum))
                       '() stanzas)))
    (let* ((rules (if (assoc :modules pkg) (cons :module -rules) -rules))
           (rules (if (assoc :structures pkg) (cons :module rules) rules))
           (rules (if (assoc :signatures pkg) (cons :sig rules) rules))
           (rules (if (assoc :cc pkg)  (cons :cc-deps rules) rules))
           (_ (format #t "~A: ~A~%" (red "obazlrules") rules))
           ;; dedup
           (rules (fold (lambda (x accum)
                          (if (member x accum) accum
                              (cons x accum)))
                        '() rules))
           )
      (format #t "~A: ~A~%" (red "obazlrules") rules)
      rules)))

(define (starlark-emit-buildfile-hdr outp obazl-rules)
  (format #t "~A: ~A\n" (blue "starlark-emit-buildfile-hdr") obazl-rules)

  (format outp "package(default_visibility = [\"//visibility:public\"])")
  (newline outp)
  (newline outp)

  (if (member :write-file obazl-rules)
      (begin
        (format outp "load(\"@bazel_skylib//rules:write_file.bzl\", \"write_file\")\n")
        (format outp "\n")))

  (if (member :copy obazl-rules)
      (begin
        (format outp "load(\"@bazel_skylib//rules:copy_file.bzl\", \"copy_file\")\n")
        (format outp "\n")))

  (if (find-if (lambda (rule)
                 (member rule '(:archive
                                :library
                                :module
                                :ns-archive
                                :ns-library
                                :ocamllex
                                :ocamlyacc
                                :sig
                                :executable
                                :test
                                :cc-deps)))
               obazl-rules)
      (begin
        (format #t "writing buildfile header\n")
        ;; if write_file, copy_file, etc, emit:
        ;; load("@bazel_skylib//lib:paths.bzl", "write_file") ;; etc.

        (format outp "load(\"@rules_ocaml//build:rules.bzl\",\n")

        (if (member :executable obazl-rules)
            (format outp "     \"ocaml_binary\",\n"))

        ;; 'library' with wrapped false:
        (if (member :archive obazl-rules)
            (format outp "     \"ocaml_archive\",\n"))

        (if (member :library obazl-rules)
            (format outp "     \"ocaml_library\",\n"))

        ;; 'library' with wrapped true:
        (if (member :ns-archive obazl-rules)
            (format outp "     \"ocaml_ns_archive\",\n"))

        (if (or (member :ns-library obazl-rules)
                (member :executable obazl-rules))
              (format outp "     \"ocaml_ns_library\",\n"))

        (if (not *ns-topdown*)
            (format outp "     \"ocaml_ns_resolver\",\n"))


        ;; obazl-style libraries not supported by dune; 'library' stanza
        ;; always means archive.
        ;; (if (member :library obazl-rules)
        ;;         (format outp "     \"ocaml_library\",\n"))
        ;; (if (member :ns-library obazl-rules)
        ;;         (format outp "     \"ocaml_ns_library\",\n")



        (if (member :ocamllex obazl-rules)
            (format outp "     \"ocamllex\",\n"))

        ;; (if (or (assoc-in '(:stanzas :executable) (cdr obazl-rules))
        ;;         (assoc-in '(:stanzas :executables) (cdr obazl-rules)))
        ;;     (format outp "     \"ocaml_executable\",\n"))

        (if (member :module obazl-rules)
            (format outp "     \"ocaml_module\",\n"))

        (if (member :sig obazl-rules)
            ;; (if *build-dyads*
            (format outp "     \"ocaml_signature\",\n")) ;;)

        ;; (if (pkg-has-archive? obazl-rules)
        ;;     (if (pkg-namespaced? obazl-rules)
        ;;         (format outp "     \"ocaml_ns_archive\",\n")))

        ;; ;; (if (assoc-in '(:stanzas :signature) (cdr obazl-rules))
        ;; (if (pkg-has-signature? obazl-rules)
        ;;     (format outp "     \"ocaml_signature\",\n"))

        (if (member :test obazl-rules)
            (format outp "     \"ocaml_test\",\n"))

        (if (member :ocamlyacc obazl-rules)
            (format outp "     \"ocamlyacc\",\n"))

        (if (member :ppx obazl-rules)
            (format outp "     \"ppx_executable\",\n"))

        (if (member :cc-deps obazl-rules)
            (format outp "     \"cc_selection_proxy\",\n"))

        (format outp ")\n")

        (newline outp)
        ))

  (if (member :menhir obazl-rules)
      (begin
        (format outp "load(\"@obazl//build:rules_ocaml.bzl\", \"menhir\")")
        (newline outp)
        (newline outp)
        ))
  )

;; multiple options classes:
;;   archive options
;;   exec options
;;   compilation: ocamlc, ocamlopt, or both (toolchain-dependent)
(define (-get-archive-opts stanza)
  (format #t "~A: ~A~%" (ublue "-get-archive-opts") stanza)
  ;; :archive-opts, from 'library-flags field
  (if-let ((opts (assoc-val :archive-opts stanza)))
          (let* ((flags (if-let ((flags (assoc-val :flags opts)))
                                (list (apply string-append
                                             (map stringify flags)))
                                '()))
                 (_ (format #t "~A: ~A~%" (uwhite "archive flags") flags))

                 (options (if-let ((options (assoc-val :options opts)))
                                  (flatten
                                   (map (lambda (opt)
                                          (list (format #f "~A" (car opt))
                                                (format #f "~A" (cdr opt))))
                                        options))
                                  '()))
                 (_ (format #t "~A: ~A~%" (uwhite "archive options")
                            options)))
            (concatenate flags options))
          '()))

(define (-opts->attrs options)
  (format #t "~A: ~A~%" (ublue "-opts->attrs") options)
  (let* ((gopens (if-let ((opens (assoc-val :opens options)))
                         (apply append (map (lambda (o)
                                              (list "-open" (stringify o)))
                                            opens))
                         '()))
         (_ (format #t "~A: ~A~%" (uwhite "gopens") gopens))

         (gflags (if-let ((flags (assoc-val :flags options)))
                         (list (apply string-append
                                      (map stringify flags)))
                         '()))
         (_ (format #t "~A: ~A~%" (uwhite "gflags") gflags))

         (goptions (if-let ((goptions (assoc-val :options options)))
                           (flatten
                            (map (lambda (opt)
                                   (list (format #f "~A" (car opt))
                                         (format #f "~A" (cdr opt))))
                                 goptions))
                           '()))
         (_ (format #t "~A: ~A~%" (uwhite "goptions") goptions))

         ;; (g-all-options (apply append (list gopens gflags goptions)))
         (g-all-options (let ((opts (apply append
                                           (list gopens gflags goptions))))
                          (if (null? opts)
                              '()
                              `((:generic ,@opts)))))
         (_ (format #t "~A: ~A\n" (bgcyan "generic compile options") g-all-options))
         (g-standard (if (assoc :standard options)
                         '((:generic-std))
                         '())))
    g-all-options))

(define (-mibl->compile-opts gopts stanza)
  (format #t "~A: ~A~%" (ublue "-mibl->compile-opts") gopts)
  (let* ((gopens (if-let ((opens (assoc-val :opens gopts)))
                         (apply append (map (lambda (o)
                                              (list "-open" (stringify o)))
                                            opens))
                         '()))
         (_ (format #t "~A: ~A~%" (uwhite "gopens") gopens))

         (gflags (if-let ((flags (assoc-val :flags gopts)))
                         (list (apply string-append
                                      (map stringify flags)))
                         '()))
         (_ (format #t "~A: ~A~%" (uwhite "gflags") gflags))

         (goptions (if-let ((goptions (assoc-val :options gopts)))
                           (flatten
                            (map (lambda (opt)
                                   (list (format #f "~A" (car opt))
                                         (format #f "~A" (cdr opt))))
                                 goptions))
                           '()))
         (_ (format #t "~A: ~A~%" (uwhite "goptions") goptions))

         ;; (g-all-options (apply append (list gopens gflags goptions)))
         (g-all-options (let ((opts (apply append
                                           (list gopens gflags goptions))))
                          (if (null? opts)
                              '()
                              `((:generic ,@opts)))))
         (_ (format #t "~A: ~A\n" (bgcyan "generic compile options") g-all-options))
         (g-standard (if (assoc :standard gopts)
                         '((:generic-std))
                         '()))
         ;;;;;;;;;;;;;;;; :ocamlc-opts ;;;;;;;;;;;;;;;;
         (bc-opts (if-let ((opts (assoc :ocamlc-opts (cdr stanza))))
                          (cdr opts) '()))
         (_ (format #t "~A: ~A~%" (uwhite "bc-opts") bc-opts))
         (bc-opens (if-let ((opens (assoc-val :opens bc-opts)))
                           (apply append (map (lambda (o)
                                                (list "-open" (stringify o)))
                                              opens))
                           '()))
         (_ (format #t "~A: ~A~%" (uwhite "bc-opens") bc-opens))

         (bc-flags (if-let ((flags (assoc-val :flags bc-opts)))
                           (list (apply string-append
                                        (map stringify flags)))
                           '()))
         (_ (format #t "~A: ~A~%" (uwhite "bc-flags") bc-flags))

         (bc-options (if-let ((bc-options (assoc-val :options bc-opts)))
                             (flatten
                              (map (lambda (opt)
                                     (list (format #f "~A" (car opt))
                                           (format #f "~A" (cdr opt))))
                                   bc-options))
                             '()))
         (_ (format #t "~A: ~A~%" (uwhite "bc-options") bc-options))

         (bc-all-options (let ((opts (apply append
                                            (list bc-opens bc-flags bc-options))))
                           (if (null? opts)
                               '()
                               `((:ocamlc ,@opts)))))
         (_ (format #t "~A: ~A\n" (bgcyan "ocamlc options") bc-all-options))
         (bc-standard (if (assoc :standard bc-opts)
                          '((:ocamlc-std))
                          '()))

         ;;;;;;;;;;;;;;;; :ocamlopt-opts ;;;;;;;;;;;;;;;;
         (nc-opts (if-let ((opts (assoc :ocamlopt-opts (cdr stanza))))
                          (cdr opts) '()))
         (_ (format #t "~A: ~A~%" (uwhite "nc-opts") nc-opts))
         (nc-opens (if-let ((opens (assoc-val :opens nc-opts)))
                           (apply append (map (lambda (o)
                                                (list "-open" (stringify o)))
                                              opens))
                           '()))
         (_ (format #t "~A: ~A~%" (uwhite "nc-opens") nc-opens))

         (nc-flags (if-let ((flags (assoc-val :flags nc-opts)))
                           (list (apply string-append
                                        (map stringify flags)))
                           '()))
         (_ (format #t "~A: ~A~%" (uwhite "nc-flags") nc-flags))

         (nc-options (if-let ((nc-options (assoc-val :options nc-opts)))
                             (flatten
                              (map (lambda (opt)
                                     (list (format #f "~A" (car opt))
                                           (format #f "~A" (cdr opt))))
                                   nc-options))
                             '()))
         (_ (format #t "~A: ~A~%" (uwhite "nc-options") nc-options))

         ;; (nc-all-options (apply append (list nc-opens nc-flags nc-options)))
         (nc-all-options (let ((opts (apply append
                                            (list nc-opens nc-flags nc-options))))
                           (if (null? opts)
                               '()
                               `((:ocamlopt ,@opts)))))
         (_ (format #t "~A: ~A\n" (bgcyan "ocamlopt options")
                    nc-all-options))
         (nc-standard (if (assoc :standard nc-opts)
                          '((:ocamlopt-std))
                          '()))
         )
      ;; if either of bc and nc define, ensure both defined
      (let ((bc-all-options (if (null? bc-all-options)
                                (if (null? nc-all-options)
                                    '()
                                    '((:ocamlc)))
                                bc-all-options))
            (nc-all-options (if (null? nc-all-options)
                                (if (null? bc-all-options)
                                    '()
                                    '((:ocamlopt)))
                                nc-all-options)))
      `(,@g-all-options
        ,@g-standard
        ,@bc-all-options
        ,@bc-standard
        ,@nc-all-options
        ,@nc-standard))))

;; three sets: :compile-opts (flags), :ocamlc-opts (ocamlc_flags),
;; and :ocamlopt_opts (ocamlopt_flags)
(define (get-compile-opts stanza pkg)
  (format #t "~A: ~A~%" (ublue "-get-compile-opts") stanza)
  (format #t "~A: ~A~%" (ublue "pkg") pkg)
  (let* ((dune (assoc-val :dune pkg))
         (gopts (if-let ((opts (assoc :compile-opts (cdr stanza))))
                       (cdr opts) '()))
         (_ (format #t "~A: ~A~%" (uwhite "gopts") gopts)))
    ;; (format #t "~A: ~A~%" (ugreen "dune") dune)
    (if (number? gopts)
        #f
        ;; (let* ((shared-compile-opts (assoc-val :shared-compile-opts dune))
        ;;        (_ (format #t "~A: ~A~%" (bggreen "shared-compile-opts") (car shared-compile-opts))))
        ;;   (if shared-compile-opts
        ;;       (let ((opts (assoc-val gopts (car shared-compile-opts))))
        ;;         (format #t "~A: ~A~%" (ugreen "opts") opts)
        ;;         (let ((xopts (-mibl->compile-opts opts stanza)))
        ;;           (format #t "~A: ~A~%" (ugreen "xopts") xopts)
        ;;           ;;(error 'STOP "STOP shared-compile-opts")
        ;;           xopts))
        ;;       (error 'STOP "Missing shared-compile-opts alist")))
        (-mibl->compile-opts gopts stanza))))

(define (-get-exec-opts stanza)
  (format #t "~A: ~A~%" (ublue "-get-exec-opts") stanza)
  '())

(define (-get-testsuite-deps name pkg)
  (format #t "~A: ~A~%" (ublue "-get-testsuite-deps") name)
  (let ((tdeps (find-then (lambda (stanza)
                (if (assoc :in-testsuite (cdr stanza))
                    (begin
                      (format #t "~A: ~A~%" (blue "in-testsuite") stanza)
                      (if-let ((deps (assoc-in '(:compile :deps)
                                               (cdr stanza))))
                              (begin
                                (format #t "~A: ~A~%" (blue "deps") deps)
                                (assoc-val :resolved (cdr deps)))
                              #f))
                    #f))
                         (assoc-val :dune pkg))))
    (format #t "~A: ~A~%" (bgblue "tdeps") tdeps)
    tdeps))

(define (-emit-shared-deps pkg)
  (format #t "~A: ~A~%" (ublue "emit-shared-deps") pkg)
  ;; (for-each
  ;;  (lambda (stanza)
  ;;    (format #t "~A: ~A~%" (uwhite "stanza") stanza)
  ;;    )
  ;;  (assoc-val :dune pkg))
  )

(define (starlark-emit-global-vars outp pkg)
  (format #t "~A: ~A\n" (bgred "starlark-emit-global-vars") pkg)
  (for-each
   (lambda (stanza)
     (format #t "~A: ~A~%" (uwhite "stanza") stanza)
     (if (not (equal? :install (car stanza)))
         (let ((testsuite (assoc-val :in-testsuite (cdr stanza))))
           (case (car stanza)
             ((:archive :library :ns-archive :ns-library)
              (let* ((libname (string-upcase
                               ;; privname or pubname?
                               (stringify (assoc-val :privname (cdr stanza)))))

                     ;; compile-options is an alist,
                     ;; keys :generic, :ocamlc, :ocamlopt
                     (archive-options (-get-archive-opts (cdr stanza)))
                     (_ (format #t "~A: ~A~%" (bgyellow "archive-options")
                                archive-options))
                     (compile-options (get-compile-opts (cdr stanza) pkg))
                     (_ (format #t "~A: ~A~%" (bgyellow "compile-options")
                                compile-options))

                     (exec-options (-get-exec-opts (cdr stanza)))
                     (_ (format #t "~A: ~A~%" (bgyellow "exec-options")
                                exec-options))

                     (deps-fixed (if-let ((df (assoc-in '(:deps :resolved) (cdr stanza))))
                                         ;; (assoc-in '(:deps :fixed) (cdr stanza))))
                                         (cdr df) #f))

                     (deps-conditional (if-let ((dc
                                                 (assoc-in '(:deps :conditionals)
                                                           (cdr stanza))))
                                               dc #f))
                     )

                (format #t "~A: ~A~%" (uwhite "deps-fixed") deps-fixed)
                (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional)
                ;; (error 'stop "STOP globals")

                (if (null? *shared-deps*)
                    (if deps-fixed
                        (begin
                          (format outp "## *shared-deps*: ~{~A ~}" *shared-deps*)
                          (format outp "DEPS_~A = [\n" libname)
                          (format outp "~{        \"~A\"~^,\n~}\n" deps-fixed)
                          (format outp "]\n")
                          (format outp "\n")
                          ))
                    (begin
                      (format #t "~A: ~A~%" (bgred "shared-deps") *shared-deps*)
                      (if (member (car (assoc-val :pkg-path pkg))
                                  *shared-deps*)
                          (begin)
                          ;; (-emit-shared-deps pkg)
                          ;; (begin
                          ;;   (format outp "## SHARED PPX ##")
                          ;;   (newline outp)
                          ;;   (format outp "## ~{~A, ~} ##" *shared-deps*)
                          ;;   (newline outp)
                          ;;   (newline outp))
                          ;; else not shared
                          (if deps-fixed
                              (begin
                                ;; (format outp "## *SHARED-deps*: ~{~A ~}" *shared-deps*)
                                ;; (newline outp)
                                ;; (newline outp)
                                (format outp "DEPS_~A = [\n" libname)
                                (format outp "~{        \"~A\"~^,\n~}\n" deps-fixed)
                                (format outp "]\n")
                                (format outp "\n")
                                )))))


                (if (not (null? archive-options))
                    (begin
                      (format outp "~A_ARCHIVE_OPTS = [\n" libname)
                      (format outp "~{        \"~A\"~^,\n~}\n" archive-options)
                      (format outp "]\n")
                      (format outp "\n")
                      )
                    )

                ;; (if (not (null? compile-options))
                (if compile-options
                    (if (assoc :generic compile-options)
                        (begin
                          (format outp "OPTS_~A = [\n" libname)
                          (format outp "~{        \"~A\"~^,\n~}\n"
                                  (assoc-val :generic compile-options))
                          (format outp "]")
                          (if (or (assoc :ocamlc compile-options)
                                  (assoc :ocamlopt compile-options))
                              (begin
                                (format outp " + select({\n")
                                (format outp "    \"@ocaml//host/target:vm\": ")
                                (format outp "[~{\"~A\"~^,~%~}],~%" (assoc-val :ocamlc compile-options))
                                (format outp "    \"@ocaml//host/target:sys\": ")
                                (format outp "[~{\"~A\"~^, ~}],~%" (assoc-val :ocamlopt compile-options))
                                (format outp "    \"//conditions:default\": ")
                                (format outp "[~{\"~A\"~^, ~}]~%" (assoc-val :ocamlopt compile-options))
                                (format outp "})\n")
                                (newline outp)
                                )
                              ;; else
                              (begin
                                (newline outp)
                                (newline outp))))
                        ;; else toolchain-specific only
                        (begin
                          (if (or (assoc :ocamlc compile-options)
                                  (assoc :ocamlopt compile-options))
                              (begin
                                (format outp "OPTS_~A = select({\n" libname)
                                (format outp "    \"@ocaml//host/target:vm?\": ")
                                (format outp "[~{\"~A\"~^,~%~}],~%" (assoc-val :ocamlc compile-options))
                                (format outp "    \"@ocaml//host/target:sys?\": ")
                                (format outp "[~{\"~A\"~^, ~}],~%" (assoc-val :ocamlopt compile-options))
                                (format outp "    \"//conditions:default\": ")
                                (format outp "[~{\"~A\"~^, ~}]~%" (assoc-val :ocamlopt compile-options))
                                (format outp "})\n")
                                (newline outp))
                              ;; else
                              ;; (begin
                              ;;   (newline outp)
                              ;;   (newline outp))
                              ))))))

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
                     (_ (format #t "opens: ~A~%" opens))
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
                                           (assoc-in '(:compile :deps :resolved)
                                                     (cdr stanza))))
                                         (cdr df) #f))
                     (deps-conditional (if-let ((dc
                                                 (assoc-in '(:deps :conditionals)
                                                           (cdr stanza))))
                                               dc #f))
                     )
                ;; (error 'STOP "STOP exec")
                (if deps-fixed
                    (if (not testsuite)
                        (begin
                          (format outp "DEPS_~A = [~%" libname)
                          (format outp "~{    \"~A\"~^,~%~}~%" deps-fixed)
                          (format outp "]~%"))))

                (if (not (null? options))
                    ;; (format outp "~A_EXE_OPTS = [~{\"~A\"~^, ~}]\n\n"
                    (format outp "OPTS_~A = [~{\"~A\"~^, ~}]\n\n"
                            libname options))
                ;; (if (not (null? ocamlc_opts))
                ;;     (format outp "~A_EXE_OCAMLC_OPTS = [~{\"~A\"~^, ~}]\n\n"
                ;;             libname ocamlc_opts))
                ;; (if (not (null? ocamlopt_opts))
                ;;     (format outp "~A_EXE_OCAMLOPT_OPTS = [~{\"~A\"~^, ~}]\n\n"
                ;;             libname ocamlopt_opts))
                ))

             ((:testsuite)
              (format #t "~A: ~A~%" (bgred "testsuite") (assoc-val :name (cdr stanza)))
              (let* ((name (assoc-val :name (cdr stanza)))
                     (deps (-get-testsuite-deps name pkg)))
                (format outp "DEPS_~A = [~{\"~A\"~^, ~}]\n\n"
                        (string-upcase
                         (format #f "~A" (assoc-val :name (cdr stanza))))
                        deps)))

             ((:rule)
              (format #t "~A: ~A~%" (bgred "FIXME")
                      "global hdrs for :rule stanzas"))

             ((:shared-deps)
              (format #t "~A: ~A~%" (bgred "shared-deps") stanza)
              (for-each (lambda (deplist)
                          (format #t "~A: ~A~%" (ured "deplist") deplist)
                          (format outp "DEPS_~A = [~%" (car deplist))
                          (format outp "~{    \"~A\"~^,~%~}~%" (cdr deplist))
                          (format outp "]~%"))
                        (cadr stanza)))

             ((:shared-compile-opts)
              (format #t "~A: ~A~%" (bgred "shared-compile-opts") stanza)
              (if (truthy? (cdr stanza))
                  (for-each (lambda (optlist)
                              (format #t "~A: ~A~%" (ured "shared optlist") optlist)
                              (let ((compile-options (-opts->attrs (cdr optlist))))
                                (format #t "~A: ~A~%" (bggreen "compile-options") compile-options)
                                ;; (error 'stop "STOP sharedopts")
                                (if (assoc :generic compile-options)
                                    (begin
                                      (format outp "OPTS_~A = [~%" (car optlist))
                                      ;; (format outp "X ~A_COMPILE_OPTS = [\n" libname)
                                      (format outp "~{        \"~A\"~^,\n~}\n"
                                              (assoc-val :generic compile-options))
                                      (format outp "]")
                                      (if (or (assoc :ocamlc compile-options)
                                              (assoc :ocamlopt compile-options))
                                          (begin
                                            (format outp " + select({\n")
                                            (format outp "    \"@ocaml//host/target:vm\": ")
                                            (format outp "[~{\"~A\"~^,~%~}],~%" (assoc-val :ocamlc compile-options))
                                            (format outp "    \"@ocaml//host/target:sys\": ")
                                            (format outp "[~{\"~A\"~^, ~}],~%" (assoc-val :ocamlopt compile-options))
                                            (format outp "    \"//conditions:default\": ")
                                            (format outp "[~{\"~A\"~^, ~}]~%" (assoc-val :ocamlopt compile-options))
                                            (format outp "})\n")
                                            (newline outp)
                                            )
                                          ;; else
                                          (begin
                                            (newline outp)
                                            (newline outp))))
                                    ;; else toolchain-specific only
                                    (begin
                                      (if (or (assoc :ocamlc compile-options)
                                              (assoc :ocamlopt compile-options))
                                          (begin
                                            (format outp "OPTS_~A = select({\n" libname)
                                            (format outp "    \"@ocaml//host/target:vm?\": ")
                                            (format outp "[~{\"~A\"~^,~%~}],~%" (assoc-val :ocamlc compile-options))
                                            (format outp "    \"@ocaml//host/target:sys?\": ")
                                            (format outp "[~{\"~A\"~^, ~}],~%" (assoc-val :ocamlopt compile-options))
                                            (format outp "    \"//conditions:default\": ")
                                            (format outp "[~{\"~A\"~^, ~}]~%" (assoc-val :ocamlopt compile-options))
                                            (format outp "})\n")
                                            (newline outp))
                                          ;; else
                                          ;; (begin
                                          ;;   (newline outp)
                                          ;;   (newline outp))
                                          )))))
                            ;; (format outp "OPTS_~A = [~%" (car optlist))
                            ;; (format outp "~{    \"~A\"~^,~%~}~%" (cdr optlist))
                            ;; (format outp "]~%")))
                            (cadr stanza))))

             ((:env :ocamllex :ocamlyacc :tuareg :menhir :sh-test :shared-ppx :alias)
              (values))

             (else
              (error 'UNHANDLED
                     (format #f "unhandled stanza for hdrs: ~A" stanza))))
           )))
   (assoc-val :dune pkg)))

(format #t "loaded starlark/headers.scm\n")
