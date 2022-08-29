(format #t "loading mibl~%")

(define arg
  "deps/literals/cwd"
  ;; "rules/with-stdout-to/cat/literal_deps"
  ;; "rules/with-stdout-to/cat/glob"
  ;; "rules/with-stdout-to/run"

  ;; "rules/install/run"
  ;; "rules/install/run/mypgm"
  )

(define (-list-pkgs ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws)))
         )
    (format #t "~A: ~A~%" (yellow "pkg ct") (length pkgs))
    (for-each (lambda (k)
                (format #t "~A: ~A~%" (blue "pkg") k))
              (sort! (hash-table-keys pkgs) string<?))
    pkgs))

(define (-dump-pkgs ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws)))
         )
    (for-each (lambda (k)
                (let ((pkg (hash-table-ref pkgs k)))
                  (format #t "~A: ~A => ~A~%" (bggreen "pkg") (green k) pkg)
                  (for-each (lambda (m)
                              (format #t "~A: ~A~%" (ugreen "pkg-module") m))
                            (if-let ((ms (assoc-val :modules pkg))) ms '()))
                  ;; (format #t "~A: ~A~%" (ugreen "pkg-modules") (assoc-val :modules pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-structures") (assoc-val :structures pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-signatures") (assoc-val :signatures pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-ocamllex") (assoc-val :ocamllex pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-ocamlyacc") (assoc-val :ocamlyacc pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-files") (assoc-val :files pkg))
                  (if-let ((dune (assoc :dune pkg)))
                          (for-each (lambda (stanza)
                                      (format #t "~A: ~A~%" (ucyan "stanza") stanza))
                                    (cdr dune)))))
              (sort! (hash-table-keys pkgs) string<?))
    pkgs))

(define (-dump-exports ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (exports (car (assoc-val :exports @ws))))
    (format #t "~A: ~A~%" (red "exports keys") (hash-table-keys exports))
    (format #t "~A: ~A~%" (red "exports table") exports)))

(define (-dump-filegroups ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (filegroups (car (assoc-val :filegroups @ws))))
    (format #t "~A: ~A~%" (red "filegroups table") filegroups)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-load-dune path)
  (format #t "~A: ~A (~A)~%" (blue "-load-dune") path (type-of path))
  (let* ((_wss (if path (load-dune path)
                   ;; (catch #t
                   ;;        (lambda () (load-dune path))
                   ;;        (lambda args
                   ;;          (format #t "~A: ~A~%" (bgred "ERROR") args)))
                   (load-dune)))
         )
    (format #t "~A: ~A~%" (green "stacktrace") (stacktrace))
    (format #t "~A: ~A~%" (green "_wss") _wss)
    _wss))

(define (-miblize ws)
  (format #t "~A: ~A~%" (blue "-miblize") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (kv)
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr kv))
                            (let ((mibl-pkg (dune-pkg->mibl :@ (cdr kv))))
                            ;; (format #t "~A: ~A~%" (red "miblized") mibl-pkg)
                             (hash-table-set! pkgs (car kv) mibl-pkg)
                             mibl-pkg))
                         pkgs)))
        ;; (_ (format #t "~A: ~A~%" (blue "mpkg-alist")
        ;;            mpkg-alist))
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist))
    mpkg-alist))

(define (-resolve-labels ws)
  (resolve-labels! (assoc-val ws -mibl-ws-table)))

(define (-miblarkize ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (blue "miblarkizing") kv)
                ;; dir may have dune-project but no dune file:
                (if (not (null? (cdr kv)))
                    (mibl-pkg->miblark (cdr kv)))
                )
              pkgs)))

(define (-emit-mibl ws)
  (format #t "~A: ~A~%" (blue "-emit-mibl") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (blue "-emit-mibl pkg") (cdr kv))
                (if (not (null? (cdr kv)))
                    (emit-mibl-pkg (cdr kv)))
                )
              pkgs)))

(define (-emit-starlark ws)
  (format #t "~A: ~A~%" (blue "-emit-starlark") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (blue "emitting") (car kv))
                (if (not (null? (cdr kv)))
                    (mibl-pkg->build-bazel ws (cdr kv))
                    (format #t "~A: ~A~%" (blue "skipping") (car kv)))
                )
              pkgs)))

(define* (dune->obazl path)
  (format #t "convert.scm::dune->obazl: ~A~%" path)
  ;; (format #t "-mibl-ws-table: ~A~%" -mibl-ws-table)
  ;; (format #t "BYE~%"))

  (set! *build-dyads* #f)
  ;; (set! *wrapped-libs-to-ns-archives* #f)
  ;; (set! *unwrapped-libs-to-archives* #f)

  (let* ((_wss (-load-dune path))

         (mpkgs (-miblize :@))

         (mpkgs (add-filegroups-to-pkgs :@))

         (_ (-resolve-labels :@))

         (_ (resolve-pkg-file-deps :@))

         (_ (-miblarkize :@))

         (_ (ws->starlark :@)) ;; (_ (-emit-starlark :@))

         (_ (format #t "~A~%" (red "PKG DUMP")))
         (_ (-dump-pkgs :@))
         (_ (-dump-exports :@))
         (_ (-dump-filegroups :@))

         )
    '()))
