(define (starlark-emit-exports-files outp pkg)
  (format #t "~A: ~A~%" (ublue "starlark-emit-exports-files") pkg)
  (let ((dune (assoc-val :dune pkg)))
    (if-let ((exports (assoc-val :exports-files dune)))
            (begin
              ;; first exports-files
              (let ((exports (filter (lambda (export)
                                       (format #t "~A: ~A~%" (uwhite "export") export)
                                         (not (vector? export)))
                                     exports)))
                (begin
                  (format #t "~A: ~A~%" (uwhite "exports") exports)
                  (format outp "exports_files([~{\"~A\"~^, ~}])\n" exports)
                  (newline outp)))
              ;; then genrules
              (let ((exports (filter (lambda (export)
                                       (vector? export))
                                     exports)))
                (for-each (lambda (export)
                            (let ((src (export 0))
                                  (dst (export 1)))
                              (format #t "~A: ~A~%" (uwhite "genfiles") exports)
                              (format outp "genrule(\n")
                              (format outp "    outs = [\"~A\"],\n" dst)
                              (format outp "    srcs = [\"~A\"],\n" src)
                              (format outp "    cmd = \" \".join([\n")
                              (format outp "        \"cp $(location ~A) $@\"\n" src)
                              (format outp "    ]),\n")
                              (format outp "    name = \"__~A__\"\n" src)
                              (format outp ")\n")
                              (newline outp)))
                            exports))
              ))))

(define (starlark-emit-filegroups outp ws pkg)
  (format #t "~A: ~A~%" (ublue "starlark-emit-filegroups") pkg)
  (format #t "~A~%" (blue "processing pkg-filegroups"))
  (let ((pkg-path (car (assoc-val :pkg-path pkg)))
        (pkg-filegroups (assoc :filegroups pkg)))
    (format #t "~A: ~A~%" (uwhite "pkg-filegroups") pkg-filegroups)
    (if pkg-filegroups
        (begin
          (newline outp)
          (for-each (lambda (fg)
                      (format #t "~A: ~A~%" (white "pkg filegroup") fg)
                      (let* ((pattern (assoc-val :glob (cdr fg)))
                             (pattern (if pattern
                                          (format #f "glob([~S])" pattern)
                                          (format #f "[~S]" pattern))))
                        (format #t "~A: ~A~%" (white "glob") glob)
                        (format outp "filegroup(\n")
                        (format outp "    name = \"~A\",\n" (car fg))
                                ;; (keyword->symbol (car fg)))
                        (format outp "    srcs = ~A,\n" pattern)
                        (let* ((clients (assoc-val :clients (cdr fg)))
                               (clients (if clients (remove pkg-path clients) '()))
                               (clients (if (null? clients) #f clients)))
                          (format #t "~A: ~A~%" (bggreen "clients") clients)
                          (format #t "~A: ~A~%" (bggreen "pkg-path") pkg-path)
                          ;; (error 'STOP "STOP emit fgs")
                          (if clients  ;; FIXME FIXME: make this work again
                              (format outp "    visibility = [~{\"//~A:__pkg__\"~^, ~}]\n" clients)
                              (format outp "    visibility = [\"//visibility:public\"]\n")))
                              ;; (format outp "    visibility = [\"//visibility:private\"]\n")))

                        (format outp ")\n")
                        (newline outp)))
                    (cdr pkg-filegroups))))
    )

  (format #t "~A~%" (blue "processing ws-filegroups"))
  (let* ((-ws (if (keyword? ws) (assoc-val ws -mibl-ws-table) ws))
         ;; (_ (format #t "~A: ~A~%" (uwhite "-ws") -ws))
         (ws-filegroups (car (assoc-val :filegroups -ws)))
         (pkg-path (assoc-val :pkg-path pkg)))
    (format #t "~A: ~A~%" (uwhite "ws-filegroups tbl") ws-filegroups)
    (format #t "~A: ~A~%" (uwhite "pkg-path") pkg-path)
    (if ws-filegroups
        (begin
          (newline outp)
          (for-each (lambda (kv)
                      (format #t "~A: ~A~%" (uyellow "fg") kv)
                      (if (equal? pkg-path (car kv))
                          (let ((key (car kv))
                                (fg (cadr kv)))
                            (format #t "~A: ~A~%" (white "filegroup") fg)
                            (let* ((glob? (eq? :glob (caadr fg)))
                                   (name  (if (eq? ::all (car fg)) "__all__" (car fg)))
                                   (pattern (cdadr fg))
                                   (pattern (if glob?
                                                (format #f "glob([~S])" pattern)
                                                (format #f "[~S]" pattern))))
                              (format #t "~A: ~A~%" (white "glob") glob)
                              (format outp "filegroup(\n")
                              (format outp "    name = \"~A\",\n" name)
                              ;; (keyword->symbol (car fg)))
                              (format outp "    srcs = ~A,\n" pattern)
                              (if-let ((client (assoc :client fg)))
                                      (begin
                                        (format #t "~A: ~A~%" (bggreen "client") (car client))
                                        (format #t "~A: ~A~%" (bggreen "pkg-path") pkg-path)
                                        (if (equal? (car client) pkg-path)
                                            (format outp "    visibility = [\"//visibility:private\"]\n")
                                            (format outp "    visibility = [\"//~A:__pkg__\"]\n" client)))
                                      (format outp "    visibility = [\"//visibility:public\"]\n"))

                              (format outp ")\n")
                              (newline outp)))))
                    ws-filegroups))))
  (format #t "~A~%" (red "finished starlark-emit-filegroups"))
  )
