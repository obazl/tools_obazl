(format #t "loading starlark/conversions.scm\n")

(load "starlark/headers.scm")
(load "starlark/rules.scm")

;; (define (starlark-emit-tuareg outp ws pkg)
;;   (format #t "~A: ~A~%" (red "starlark-emit-tuareg") pkg)
;;   (if (assoc-in '(:dune :tuareg) pkg)
;;       (begin
;;         (format outp "fail(\"FIXME: tuareg file\")")
;;         (newline outp))))

;; FIXME: rename emit-starlark
(define (mibl-pkg->build-bazel ws pkg)
  (format #t "~A: ~A\n" (bgblue "mibl-pkg->build-bazel") pkg)
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (dunefile (assoc :dune pkg)))
    (format #t "~A: ~A~%" (uwhite "dune") dunefile)
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

          (format #t "~%~A: ~A~%"
                  (bgred "Emitting buildfile") build-file)

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

          (format #t "emitting conditional deps\n")
          (starlark-emit-select-flags outp ws pkg)

          (format #t "emitting filegroups\n")
          (starlark-emit-filegroups outp ws pkg)

          ;; (format #t "emitting tuareg\n")
          ;; (starlark-emit-tuareg outp ws pkg)

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
               (_ (format #t "~A: ~A~%" (uwhite "build-file") build-file))
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

;; (define (-emit-import settings ws)
;;   ;; emit one //bzl/import structure per workspace
;;   ;; (to handle dune 'select' flds)
;;   ;; one string_flag/config_setting pair per selector protasis
;;   (let* ((@ws (assoc-val ws -mibl-ws-table))
;;          (pkgs (car (assoc-val :pkgs @ws))))
;;     ;; 1. accumulate all select protases
;; g    ;; alternative: update global var as we go
;; ;; 2. generate BUILDfiles with config_settings/string_flags

;;     (for-each (lambda (kv)
;;                 )
;;               pkgs)))

(define (ws->starlark ws)
  (format #t "~%~A: ~A~%" (bgred "ws->starlark") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    ;; (-emit-import-settings ws)

    ;; also one //bzl/profiles per workspace

    (for-each (lambda (kv)
                (format #t "~A: ~S~%" (blue "emitting pkg") (car kv))
                ;; if this is the root dunefile (w/sibling dune-project file)
                ;; and we have :env stanza, emit //profiles/BUILD.bazel
                ;; PROBLEM: what if we have sub-workspaces, i.e.
                ;; multiple (env ...) dunefiles?
                (if (assoc 'dune-project (cdr kv))
                    (if (assoc-in '(:dune :env) (cdr kv))
                        (emit-profiles ws (cdr kv))))
                (if (not (null? (cdr kv)))
                    (mibl-pkg->build-bazel ws (cdr kv))
                    (format #t "~A: ~A~%" (blue "skipping") (car kv)))
                )
              pkgs)))
