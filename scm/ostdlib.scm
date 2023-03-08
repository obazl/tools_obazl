(if *mibl-debugging*
    (format #t "loading new.scm~%"))

(define *stdlib-modules* '())

(define (stdlib->module-names)
  (if *mibl-debugging*
      (format #t "~A~%" (ublue "stdlib->module-names")))
  (let* ((cmd (format #f "opam var lib"))
         ;; (_ (format #t "~A: ~A~%" (green "cmd") cmd))
         (opam-lib (string-trim '(#\newline) (system cmd #t)))
         (ocaml-lib (format #f "~A/ocaml" opam-lib)))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (green "ocaml-lib") ocaml-lib))
    (let* ((stdlib-modules (system (format #f "ls ~A/stdlib__*.cmi" ocaml-lib) #t))
           (stdlib-ms (string-split stdlib-modules #\newline))
           ;; (stdlib-ms (string-split stdlib-modules #\space))
           (stdlib-modules (map basename stdlib-ms))
           (stdlib-mnames (map (lambda (m)
                                 (string->symbol (bname (string-left-trim "stdlib__" m))))
                               stdlib-modules)))
      (cons 'Stdlib stdlib-mnames))))

(define (stdlib->module-names!)
  (let ((stdlib-modules (stdlib->module-names)))
    (set! *stdlib-modules* stdlib-modules)))

;; precondition: *stdlib-modules* has been populated
(define* (srcfile->local-deps pkg tgt)
  (if *mibl-debugging*
      (begin
        (format #t "~A: ~A/~A~%" (ublue "get-local-deps") pkg tgt)
        (format #t "~A: ~A~%" (green "*stdlib-modules*") *stdlib-modules*)))

  (if (truthy? *stdlib-modules*)
    ;; run ocamldep on srcs and filter output against stdlib
    (let* ((cmd (format #f "ocamldep -one-line -modules -I ~A ~A/~A"
                        pkg pkg tgt))
           ;; (_ (format #t "~A: ~A~%" (green "cmd") cmd))
           (deps (string-trim '(#\newline) (system cmd #t)))
           (deps (string-split deps #\newline)))
      (if *mibl-debugging*
          (format #t "~A: ~A~%" (green "deps") deps))
      ;; (error 'x "x")

      (if *mibl-debugging*
          (format #t "~%~A: ~A~%" (uyellow "iter over ocamldeps") deps))
      (let ((segs (string-split (car deps) #\:)))
        (if *mibl-debugging*
            (format #t "~A: ~A~%" (yellow "segs") segs))
        (if (null? (cdr segs))
            (begin)
            (let* ((fpath (car segs))
                   (fname (basename fpath))
                   (mname (filename->module-name fname))
                   (kind (filename->kind fname))
                   (mdeps (string-trim '(#\space) (cadr segs)))
                   (mdeps (string-split mdeps #\space))
                   (mdeps (map string->symbol mdeps))
                   (_ (format #t "~A for ~A: ~A~%" (ugreen "unfiltered mdeps") mname mdeps))

                   ;; (_ (for-each (lambda (m)
                   ;;                (format #t "m: ~A (t: ~A)~%" m (type-of m)))
                   ;;              mdeps))
                   ;; remove dep cycle: file does not depend on itself
                   (mdeps (remove mname mdeps))
                   (_ (format #t "~A: ~A~%" (ugreen "unfiltered mdeps excluding self") mdeps))

                   ;; eliminate mdeps not in this pkg
                   ;; (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))

                   ;; eliminate deps in stdlib
                   (mdeps (filter (lambda (dep)
                                    ;; (format #t "~A: ~A~%" (red "dep") dep)
                                    (not (member dep *stdlib-modules*))) mdeps))
                   )
              ;; (format #t "~A: ~A~%" (ugreen "stdlib modules") *stdlib-modules*)
              (if *mibl-debugging*
                  (format #t "~A: ~A~%" (ugreen "filtered mdeps, excluding stdlib modules") mdeps))
              ;; (format #t "~A: ~A~%" (red "mdeps") mdeps)
              ;; (format #t "~A: ~A~%" (red "fname") fname)
              ;; (format #t "~A: ~A~%" (red "mname") mname)
              ;; (if (string=? "arg.ml" fname) (error 'STOP "STOP ocamldep"))
              ;; (if (truthy? mdeps) ;; (not (null? mdeps))
              ;;     (begin
              ;;       (format #t "~A ~A to ~A~%" (bgyellow "updating stanza :deps") mdeps fname)
              ;;       ;; (format #t "~A: ~A~%" (uyellow "in pkg") pkg)
              ;;       (update-stanza-deps pkg fname mdeps)
              ;;       (format #t "~A: ~A~%" (red "pkg (after)") pkg)
              ;;       ))

              ;; mdeps is list of ocamldeps of fname with corresponding files in this pkg
              ;; we retrieve the pkg-dep for fname and add the mdeps to it
              mdeps)))
                ;; deps)
      ;; (ocaml-lib (format #f "~A/ocaml" deps)))
      )))

;; called by @obazl//toolchain:stdlib
(define* (analyze)
  (format #t "~A~%" (ublue "analyze"))

  (let ((stdlib-modules (stdlib->module-names)))
    ;; (set! *stdlib-modules* stdlib-modules)

    (format #t "~A: ~A~%" (green "stdlib-modules") stdlib-modules)

    ;; run ocamldep on srcs and filter output against stdlib
    (let* ((cmd (format #f "ocamldep -one-line -modules -I ~A ~A/*"
                        "src/cbor", "src/cbor"))
           ;; (_ (format #t "~A: ~A~%" (green "cmd") cmd))
           (deps (string-trim '(#\newline) (system cmd #t)))
           (deps (string-split deps #\newline)))
      (format #t "~A: ~A~%" (green "deps") deps)
      ;; (error 'x "x")

      (for-each (lambda (dep)
                  (format #t "~%~A: ~A~%" (uyellow "iter over ocamldeps") dep)
                  (let ((segs (string-split dep #\:)))
                    (format #t "~A: ~A~%" (yellow "segs") segs)
                    (if (null? (cdr segs))
                        (begin)
                        (let* ((fpath (car segs))
                               (fname (basename fpath))
                               (mname (filename->module-name fname))
                               (kind (filename->kind fname))
                               (mdeps (string-trim '(#\space) (cadr segs)))
                               (mdeps (string-split mdeps #\space))
                               (mdeps (map string->symbol mdeps))
                               (_ (format #t "~A for ~A: ~A~%" (ugreen "unfiltered mdeps") mname mdeps))

                               ;; (_ (for-each (lambda (m)
                               ;;                (format #t "m: ~A (t: ~A)~%" m (type-of m)))
                               ;;              mdeps))
                               ;; remove dep cycle: file does not depend on itself
                               (mdeps (remove mname mdeps))
                               (_ (format #t "~A: ~A~%" (ugreen "unfiltered mdeps excluding self") mdeps))

                               ;; eliminate mdeps not in this pkg
                               ;; (mdeps (filter (lambda (d) (is-module-in-pkg d pkg)) mdeps))

                               ;; eliminate deps in stdlib
                               (mdeps (filter (lambda (dep)
                                                ;; (format #t "~A: ~A~%" (red "dep") dep)
                                                (not (member dep stdlib-modules))) mdeps))
                               )
                          (format #t "~A: ~A~%" (ugreen "stdlib modules") stdlib-modules)
                          (format #t "~A: ~A~%" (ugreen "filtered mdeps, excluding stdlib modules") mdeps)
                          ;; (format #t "~A: ~A~%" (red "mdeps") mdeps)
                          ;; (format #t "~A: ~A~%" (red "fname") fname)
                          ;; (format #t "~A: ~A~%" (red "mname") mname)
                          ;; (if (string=? "arg.ml" fname) (error 'STOP "STOP ocamldep"))
                          ;; (if (truthy? mdeps) ;; (not (null? mdeps))
                          ;;     (begin
                          ;;       (format #t "~A ~A to ~A~%" (bgyellow "updating stanza :deps") mdeps fname)
                          ;;       ;; (format #t "~A: ~A~%" (uyellow "in pkg") pkg)
                          ;;       (update-stanza-deps pkg fname mdeps)
                          ;;       (format #t "~A: ~A~%" (red "pkg (after)") pkg)
                          ;;       ))

                          ;; mdeps is list of ocamldeps of fname with corresponding files in this pkg
                          ;; we retrieve the pkg-dep for fname and add the mdeps to it
                          ))))
                deps)
      ;; (ocaml-lib (format #f "~A/ocaml" deps)))
      '())))
