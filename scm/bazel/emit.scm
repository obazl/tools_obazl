(if *mibl-debug-s7*
    (format #t "loading bazel/conversions.scm\n"))

(load "bazel/headers.scm")
(load "bazel/rules_bazel.scm")
(load "bazel/non_dune_emit.scm")

;; (define (bazel-emit-tuareg outp ws pkg)
;;   (format #t "~A: ~A~%" (red "bazel-emit-tuareg") pkg)
;;   (if (assoc-in '(:mibl :tuareg) pkg)
;;       (begin
;;         (format outp "fail(\"FIXME: tuareg file\")")
;;         (newline outp))))

;; FIXME: rename emit-bazel
(define (mibl-pkg->build-bazel ws pkg)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (bgblue "mibl-pkg->build-bazel") pkg))
  (load "bazel/non-dune-parsers.scm")
  (let* ((pkg-path (assoc-val :pkg-path pkg))
         (dunefile (assoc :mibl pkg)))
    (if *mibl-debug-s7*
        (format #t "~A: ~A~%" (uwhite "dune") dunefile))
    (if *mibl-debug-s7*
        (if dunefile
            (begin
              (format #t "~A: ~A~%" (uwhite "(cdr dune)") (cdr dunefile))
              (format #t "~A: ~A~%" (uwhite "(alist? (cdr dune)") (alist? (cdr dunefile))))))
    (if (and dunefile (not (null? (cdr dunefile))))
        (let* ((stanzas (cadr dunefile))
               (obazl-rules (pkg->obazl-rules pkg))
               (_ (if *mibl-debug-s7*
                      (format #t "~A: ~A\n" (green "obazl rules result") obazl-rules)))
               (build-file (if (eq? ::wsroot pkg-path)
                               "./BUILD.bazel"
                               (string-append pkg-path "/BUILD.bazel")))
               ;; (_ (format #t "build file: ~A~%" build-file))
               ;; (_ (format #t "cwd: ~A~%" ((*libc* 'pwd))))
               (outp
                (catch #t
                       (lambda ()
                         (open-output-file build-file))
                       (lambda args
                         (error 'OPEN_ERROR "OPEN ERROR"))
                       )))
          ;; (_ (format #t "ok so"))

          (if *mibl-debug-s7*
              (format #t "~%~A: ~A~%"
                      (bgred "Emitting buildfile") build-file))

          (bazel-emit-buildfile-hdr outp pkg-path obazl-rules pkg)
          ;; (newline outp)

          (if *mibl-debug-s7* (format #t "emitting exports_files\n"))
          (bazel-emit-exports-files outp pkg)

          (bazel-emit-global-vars outp pkg)

          (if *mibl-debug-s7* (format #t "emitting executables\n"))
          (bazel-emit-executable-targets outp ws pkg)

          (if *mibl-debug-s7* (format #t "emitting shared-prologues\n"))
          (bazel-emit-shared-prologues outp ws pkg)

          (if *mibl-debug-s7*
              (format #t "emitting aggregate targets (archive, library)\n"))
          (bazel-emit-aggregate-targets outp pkg) ;;fs-path stanzas)

          (if *mibl-debug-s7* (format #t "emitting singleton targets\n"))
          (bazel-emit-singleton-targets outp ws pkg)
          ;; (bazel-emit-singleton-targets outp pkg-path stanzas
          ;;                                  (cdr pkg-kv))

          (if *mibl-debug-s7* (format #t "emitting test targets\n"))
          (bazel-emit-test-targets outp ws pkg)

          (if *mibl-debug-s7* (format #t "emitting file generators\n"))
          ;; ocamllex, ocamlyacc, etc.
          (bazel-emit-file-generators outp pkg)

          ;; (if #t ;; *mibl-local-ppx-driver*
          ;;     (begin
          ;;       (if *mibl-debug-s7* (format #t "emitting pkg ppxes\n"))
          (bazel-emit-pkg-ppxes outp ws pkg) ;; ))

          (if *mibl-debug-s7* (format #t "emitting rules\n"))
          (bazel-emit-rule-targets outp pkg) ;; fs-path stanzas)

          (if *mibl-debug-s7* (format #t "emitting conditional deps\n"))
          (bazel-emit-select-flags outp ws pkg)

          (if *mibl-debug-s7* (format #t "emitting filegroups\n"))
          (bazel-emit-filegroups outp ws pkg)

          (if *mibl-debug-s7* (format #t "emitting cc targets\n"))
          (bazel-emit-cc-targets outp ws pkg)

          ;; ignoring local :env stanzas

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
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; no :dune, emit filegroups etc.
        (let ((pkg-path (assoc-val :pkg-path pkg))
              (pkg-filegroups (assoc :filegroups pkg)))
          (if (or *mibl-debug-s7* *mibl-debug-emit*)
              (begin
                (format #t "~A: ~A~%" (uwhite "emitting non-dune pkg") pkg)
                (format #t "~A: ~A~%" (uwhite "pkg-filegroups") pkg-filegroups)))
          ;;FIXME: check that we have something to emit before opening BUILD.bazel
          (if-let ((obazl-rules (non-dune-pkg->obazl-rules pkg)))
                  (let* ((build-file (string-append pkg-path "/BUILD.bazel"))
                         (_ (if *mibl-debug-s7*
                                (format #t "~A: ~A~%" (uwhite "build-file") build-file)))
                         (outp
                          (catch #t
                                 (lambda ()
                                   (open-output-file build-file))
                                 (lambda args
                                   (error 'OPEN_ERROR2 "OPEN ERROR2"))
                                 )))
                    (format outp "## GENERATED FILE - do not edit~%")

                    (bazel-emit-buildfile-hdr outp pkg-path obazl-rules pkg)

                    (emit-non-dune-global-vars outp pkg obazl-rules)

                    (emit-non-dune-aggregate-target outp pkg)

                    ;; (if *mibl-debug-s7* (format #t "emitting exports_files\n"))
                    ;; (bazel-emit-exports-files outp pkg)

                    ;; (bazel-emit-global-vars outp pkg)

                    (if pkg-filegroups
                        (begin
                          (if *mibl-debug-s7* (format #t "emitting filegroups\n"))
                          (bazel-emit-filegroups outp ws pkg)))
                    ;; emit ocaml_library
                    (emit-non-dune-singletons outp ws pkg)
                    ;; emit :sigs
                    ;; emit :structs

                    (if *mibl-debug-s7* (format #t "emitting file generators\n"))
                    ;; ocamllex, ocamlyacc, etc.
                    (emit-non-dune-file-generators outp pkg)

                    (if *mibl-debug-s7* (format #t "emitting cc targets\n"))
                    ;; if pkg has cc files
                    ;; (emit-non-dune-cc-targets outp ws pkg)

                    (close-output-port outp)
                    )))
          )))

;; (define (-emit-import settings ws)
;;   ;; emit one //bzl/import structure per workspace
;;   ;; (to handle dune 'select' flds)
;;   ;; one string_flag/config_setting pair per selector protasis
;;   (let* ((@ws (assoc-val ws *mibl-project*))
;;          (pkgs (car (assoc-val :pkgs @ws))))
;;     ;; 1. accumulate all select protases
;; g    ;; alternative: update global var as we go
;; ;; 2. generate BUILDfiles with config_settings/string_flags

;;     (for-each (lambda (kv)
;;                 )
;;               pkgs)))

(define (ws->bazel ws)
  (if *mibl-debug-s7*
      (format #t "~%~A: ~A~%" (bgred "ws->bazel") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    ;; (-emit-import-settings ws)

    (if *mibl-debug-s7*
        (format #t "~A: ~A~%" (bgred "scan exlusions") *mibl-scan-exclusions*))

    ;; also one //bzl/profiles per workspace
    (if *mibl-debug-s7*
        (format #t "~A: ~A~%" (bgred "*mibl-emit-bazel-pkg*") *mibl-emit-bazel-pkg*))
    (for-each (lambda (kv)
                (if (or (not *mibl-emit-bazel-pkg*)
                        (and (truthy? *mibl-emit-bazel-pkg*)
                             (string-prefix? *mibl-emit-bazel-pkg* (car kv))))
                    (begin
                      (if *mibl-debug-s7*
                          (format #t "~A: ~S~%" (blue "emitting pkg") (car kv)))
                      ;; if this is the root dunefile (w/sibling dune-project file)
                      ;; and we have :env stanza, emit //profiles/BUILD.bazel
                      ;; PROBLEM: what if we have sub-workspaces, i.e.
                      ;; multiple (env ...) dunefiles?
                      ;; e.g. alcotest//test pkgs
                      (if (assoc 'dune-project (cdr kv))
                          (if (assoc-in '(:mibl :env) (cdr kv))
                              (emit-profiles ws (cdr kv))))
                      (if (not (null? (cdr kv)))
                          ;; (if (assoc 'dune (cdr kv))
                          (if (not (member (car kv) *mibl-scan-exclusions*))
                                  (mibl-pkg->build-bazel ws (cdr kv))
                                  (if *mibl-debug-s7*
                                      (format #t "~A: ~A~%" (blue "skipping") (car kv))))
                              ;; (begin
                              ;;   (format #t "~A: ~A~%" (blue "NON-DUNE PKG") (car kv))
                              ;;   (non-dune-pkg->build-bazel ws (cdr kv))))
                              )
                      )))
              pkgs)

    ;; (if (not *mibl-local-ppx-driver*)
    ;;     (call-with-exit
    ;;      (lambda (return)
    ;; NOT YET IMPLMENTED in mibl: workspace :shared-ppx
    ;;        (bazel-emit-global-ppxes ws return))))
    ))
