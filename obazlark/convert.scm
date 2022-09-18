(format #t "loading convert.scm~%")

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
    (format #t "~A: ~A ~A~%" (bggreen "workspace") (assoc :name @ws) (assoc :path @ws))
    (for-each (lambda (k)
                (let ((pkg (hash-table-ref pkgs k)))
                  (format #t "~%~A: ~A => ~A~%" (bggreen "package") (green k) (assoc-val :pkg-path pkg))
                  (for-each (lambda (fld)
                              (format #t "~A: ~A~%" (ugreen "fld") (car fld)))
                            pkg)
                  (if-let ((ms (assoc-val :modules pkg)))
                          (for-each (lambda (m)
                                      (format #t "~A: ~A~%" (ugreen "pkg-module") m))
                                    ms)
                          (format #t "~A: ~A~%" (ugreen "pkg-modules") ms))
                  (format #t "~A:~%" (ugreen "pkg-structures"))
                  (if-let ((ss (assoc-in '(:structures :static) pkg)))
                          (for-each (lambda (s)
                                      (format #t "  ~A: ~A~%" (ugreen "static") s))
                                    (cdr ss))
                          (format #t "  ~A: ~A~%" (ugreen "statics") ss))
                  (if-let ((ss (assoc-in '(:structures :dynamic) pkg)))
                          (for-each (lambda (s)
                                      (format #t "  ~A: ~A~%" (ugreen "dynamic") s))
                                    (cdr ss))
                          (format #t "  ~A: ~A~%" (ugreen "dynamics") ss))
                  ;; (format #t "~A: ~A~%" (ugreen "pkg-structures") (assoc-val :structures pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-signatures") (assoc-val :signatures pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-ocamllex") (assoc-val :ocamllex pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-ocamlyacc") (assoc-val :ocamlyacc pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-cc") (assoc-val :cc pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-ppx") (assoc-val :shared-ppx pkg))
                  (format #t "~A: ~A~%" (ugreen "pkg-opam") (assoc-val :opam pkg))
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

(define (-dump-ppx ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (ppx-tbl (car (assoc-val :shared-ppx @ws))))
    (format #t "~A: ~A~%" (bgcyan "shared-ppx") ppx-tbl)))

(define (-dump-opam ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (opam (car (assoc-val :opam @ws))))
    (format #t "~A: ~A~%" (red "opam keys") (hash-table-keys opam))
    (for-each (lambda (ws)
                (format #t "~A: ~A~%" (red "opam ws") (car ws))
                (for-each (lambda (item)
                            (format #t "  ~A~%" item))
                          (cdr ws)))
              opam)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-load-dune path)
  (format #t "~A: ~A (~A)~%" (blue "-load-dune") path (type-of path))
  (let* ((_wss (if path (load-dune path)
                   (load-dune)))
         )
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
  (set! *shared-deps* '("compiler/tests-compiler" "lib/js_of_ocaml"))

  ;; (set! *wrapped-libs-to-ns-archives* #f)
  ;; (set! *unwrapped-libs-to-archives* #f)

  (let* ((_wss (-load-dune path))

         (mpkgs (-miblize :@))

         (mpkgs (add-filegroups-to-pkgs :@))

         (mpkgs (normalize-manifests! :@)))

    (-resolve-labels :@)
    (resolve-pkg-file-deps :@)
    (-miblarkize :@)

    (handle-shared-ppx :@)

    (if *shared-deps*
        (begin
          (handle-shared-deps :@)
          (handle-shared-opts :@)
          ))

    (ws->starlark :@)

    (ws->opam-bundles :@)

    (format #t "~A~%" (red "PKG DUMP"))
    (-dump-pkgs :@)

    ;; (format #t "~A: ~A~%" (green "selectors")
    ;;         (remove-duplicates *select-protases*))
    ;; (-dump-exports :@)
    (-dump-ppx :@)
    ;; (-dump-filegroups :@)
    ;; (-dump-opam :@)

    )
  '())
