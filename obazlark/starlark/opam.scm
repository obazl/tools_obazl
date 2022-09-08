(define mkdir-permissions (logior S_IRWXU S_IRWXG S_IRWXO))

(define (-emit-opam-bin ws-name bundle-dir binfld)
  (format #t "~A: ~A~%" (ublue "-emit-opam-bin") binfld)
  (for-each (lambda (bin)
              (format #t "~A: ~A~%" (bgyellow "bin") bin)
              (let* ((bindir (format #f "~A/bin/~A" bundle-dir (car bin)))
                     (bin-impl (cadr bin))
                     (impl-pkg (car bin-impl))
                     (impl-tgt (cdr bin-impl)))
                (format #t "~A: ~A~%" (blue "bindir") bindir)
                (format #t "~A: ~A~%" (blue "bin-impl") bin-impl)
                (if (directory? bindir)
                    (begin)
                    (mkdir-recursive bindir mkdir-permissions))
                (let* ((build-file (format #f "~A/bin/~A/BUILD.bazel"
                                           bundle-dir (car bin)))
                       (_ (format #t "~A: ~A~%"
                                  (blue "emitting bld file") build-file))
                       (outp
                        (catch #t
                               (lambda ()
                                 (open-output-file build-file))
                               (lambda args
                                 (format
                                  #t (format #f "OPEN ERROR: ~A" build-file)))
                               )))
                (format outp "package(default_visibility = [\"//visibility:public\"])")
                (newline outp) (newline outp)
                (format outp "alias(~%")
                (format outp "    name = \"~A\"~%" (car bin))
                (format outp "    actual = \"@foo//~A:~A\"~%" impl-pkg impl-tgt)
                (format outp ")~%")
                (close-output-port outp)
                ;; (-emit-opam-sub-pkgs path bundle)
                )))
            (cdr binfld)))

(define (-emit-opam-lib ws-name bundle-dir libfld)
  (format #t "~A: ~A~%" (ublue "-emit-opam-lib") libfld)
  (for-each (lambda (lib)
              (format #t "~A: ~A~%" (bgyellow "lib") lib)
              (let* ((libdir (format #f "~A/lib/~A" bundle-dir ws-name))
                     ;; (lib-impl (cadr lib))
                     (impl-pkg (car lib))
                     (impl-tgt (cdr lib)))
                (format #t "~A: ~A~%" (blue "libdir") libdir)
                (format #t "~A: ~A~%" (blue "impl-pkg") impl-pkg)
                (format #t "~A: ~A~%" (blue "impl-tgt") impl-tgt)
                (if (directory? libdir)
                    (begin)
                    (mkdir-recursive libdir mkdir-permissions))
                (let* ((build-file (format #f "~A/lib/~A/BUILD.bazel"
                                           bundle-dir ws-name))
                       (_ (format #t "~A: ~A~%"
                                  (blue "emitting bld file") build-file))
                       (outp
                        (catch #t
                               (lambda ()
                                 (open-output-file build-file))
                               (lambda args
                                 (format
                                  #t (format #f "OPEN ERROR: ~A" build-file)))
                               )))
                (format outp "package(default_visibility = [\"//visibility:public\"])")
                (newline outp) (newline outp)
                (format outp "alias(~%")
                (format outp "    name = \"~A\"~%" ws-name)
                (format outp "    actual = \"@foo//~A:~A\"~%" impl-pkg impl-tgt)
                (format outp ")~%")
                (close-output-port outp)
                ;; (-emit-opam-sub-pkgs path bundle)
                )))
            (cdr libfld)))
  ;; (let ((libdir (format #f "~A/lib/~A" bundle-dir ws-name)))
  ;;   (format #t "~A: ~A~%" (blue "libdir") libdir)
  ;;   (if (directory? libdir)
  ;;       (begin)
  ;;       (mkdir-recursive libdir mkdir-permissions))
    ;; ))

(define (-emit-opam-bundle ws-name bundle-dir bundle)
  (format #t "~A: ~A~%" (ugreen "-emit-opam-bundle") bundle-dir)
  (format #t "~A: ~A~%" (green "ws-name") ws-name)
  (format #t "~A: ~A~%" (green "bundle") bundle)

  (for-each (lambda (fld)
              (case (car fld)
                ((:path) #| ignore |# )
                ((:bin) ;; multiple bins per opam pkg
                 (-emit-opam-bin ws-name bundle-dir fld)
                 )
                ((:lib) ;; at most one lib per opam pkg?
                 (-emit-opam-lib ws-name bundle-dir fld)
                 )
                (else
                 (if (string? (car fld))
                     (format #t "~A: ~A~%" (red "subdir") fld)
                     (format #t "~A: ~A~%" (red "other") fld)))))
            (cdr bundle))

  (let* ((build-file (format #f "~A/BUILD.bazel" bundle-dir))
         (_ (format #t "~A: ~A~%" (blue "emitting bld file") build-file))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file build-file))
                 (lambda args
                   (format #t (format #f "OPEN ERROR: ~A" build-file)))
                 )))
    (format outp "package(default_visibility = [\"//visibility:public\"])")
    (newline outp)
    (close-output-port outp)
    ;; (-emit-opam-sub-pkgs path bundle)
    )
  )

  ;; (format #t "~A: ~A~%" (blue "-emit-sub-bundle") sub)
  ;; (let* ((build-file (format #f "~A/~A/BUILD.bazel" path (car sub)))
  ;;        (_ (format #t "~A: ~A~%" (blue "emitting bld file") build-file))
  ;;        (outp
  ;;         (catch #t
  ;;                (lambda ()
  ;;                  (open-output-file build-file))
  ;;                (lambda args
  ;;                  (format #t (format #f "OPEN ERROR: ~A" build-file)))
  ;;                )))
  ;;   (format outp "package(default_visibility = [\"//visibility:public\"])")
  ;;   (newline outp)
  ;;   (close-output-port outp))
  ;; )

(define (-emit-opam-sub-pkgs path bundle)
  (if-let ((subs (filter (lambda (x)
                           (string? (car x)))
                         (cdr bundle))))
          (begin
            (format #t "~A: ~A~%" (bgyellow "subs") subs)
            (for-each (lambda (sub)
                        (format #t "~A: ~A~%" (red "sub bundle") sub)
                        (let ((sub-path (format #f "~A/~A" path (car sub))))
                          (format #t "~A: ~A~%" (red "sub-path") sub-path)
                          (if (directory? sub-path)
                              (begin)
                              (mkdir sub-path mkdir-permissions))
                          (-emit-sub-bundle path sub)))
                      subs))))

(define (-emit-opam-pkgs ws-name path bundle)
  (format #t "~A: ~A~%" (ublue "-emit-opam-pkgs") path)
  (format #t "~A: ~A~%" (ublue "ws-name") ws-name)
  (format #t "~A: ~A~%" (ublue "bundle") bundle)

  (let* ((ws-file (format #f "~A/WORKSPACE.bazel" path))
         (_ (format #t "~A: ~A~%" (blue "emitting ws file") ws-file))
         (outp
          (catch #t
                 (lambda ()
                   (open-output-file ws-file))
                 (lambda args
                   (format #t "OPEN ERROR: ~A" ws-file))
                 )))
    (format outp "workspace(name = \"~A\")" ws-name)
    (newline outp)
    (close-output-port outp))
  (-emit-opam-bundle ws-name path (cdr bundle)))

(define (ws->opam-bundles ws)
  (format #t "~%~A: ~A~%" (bgblue "ws->opam-bundles") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (opam (car (assoc-val :opam @ws))))

    (for-each (lambda (bundle)
                (format #t "~A: ~S~%" (ublue "emitting opam-bundle") (car bundle))
                (format #t "~A: ~S~%" (blue "opam-bundle") bundle)

                (let* ((bundle-dir (format #f "~A.opam-bundle" (car bundle)))
                       (bundle-path (format #f "~A/~A"
                                            (assoc-val :path (cdr bundle))
                                            bundle-dir)))
                  ;; (outp
                  ;;  (catch #t
                  ;;         (lambda ()
                  ;;           (open-output-file build-file))
                  ;;         (lambda args
                  ;;           (format #t "OPEN ERROR"))
                  ;;         )))
                  (format #t "~A: ~A~%" (blue "bundle-path") bundle-path)
                  (format #t "~A: ~A~%" (red "(directory? bundle-path)") (directory? bundle-path))
                  (format #t "~A: ~A~%" (red "(file-exists? bundle-path)") (file-exists? bundle-path))

                  (if (directory? bundle-path)
                      (-emit-opam-pkgs (car bundle) bundle-path bundle)
                      (begin
                        (mkdir-recursive bundle-path mkdir-permissions)
                        (-emit-opam-pkgs (car bundle) bundle-path bundle)))))
              opam)))
