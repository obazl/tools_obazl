(define (starlark-emit-exports-files outp pkg)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "starlark-emit-exports-files") pkg))
  (let ((dune (assoc-val :dune pkg)))
    (if-let ((exports (assoc-val :exports-files dune)))
            (begin
              ;; first exports-files
              (let ((exports (filter (lambda (export)
                                       (if *debugging*
                                           (format #t "~A: ~A~%" (uwhite "export") export))
                                       (not (vector? export)))
                                     exports)))
                (begin
                  (if *debugging*
                      (format #t "~A: ~A~%" (uwhite "exports") exports))
                  (format outp "exports_files([~{\"~A\"~^, ~}])\n" exports)
                  (newline outp)))
              ;; then genrules
              (let ((exports (filter (lambda (export)
                                       (vector? export))
                                     exports)))
                (for-each (lambda (export)
                            (let ((src (export 0))
                                  (dst (export 1)))
                              (if *debugging*
                                  (format #t "~A: ~A~%" (uwhite "genfiles") exports))
                              (format outp "FG genrule(\n")
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
  (if *debugging*
      (begin
        (format #t "~A: ~A~%" (ublue "starlark-emit-filegroups") pkg)
        (format #t "~A~%" (blue "processing pkg-filegroups"))))
  (let ((pkg-path (car (assoc-val :pkg-path pkg)))
        (pkg-filegroups (assoc :filegroups pkg)))
    (if *debugging*
        (format #t "~A: ~A~%" (uwhite "pkg-filegroups") pkg-filegroups))
    (if pkg-filegroups
        (begin
          (for-each (lambda (fg)
                      (if *debugging*
                          (format #t "~A: ~A~%" (white "pkg filegroup") fg))
                      (let* ((pattern (assoc-val :glob (cdr fg)))
                             (pattern (if pattern
                                          (format #f "glob([~S])" pattern)
                                          (format #f "[~S]" pattern))))
                        (if *debugging*
                            (format #t "~A: ~A~%" (white "glob") glob))
                        (format outp "##########~%")
                        (format outp "filegroup(~%")
                        (format outp "    name = \"~A\",~%" (car fg))
                                ;; (keyword->symbol (car fg)))
                        (format outp "    srcs = ~A,~%" pattern)
                        (let* ((clients (assoc-val :clients (cdr fg)))
                               (clients (if clients (remove pkg-path clients) '()))
                               (clients (if (null? clients) #f clients)))
                          (if *debugging*
                              (begin
                                (format #t "~A: ~A~%" (bggreen "clients") clients)
                                (format #t "~A: ~A~%" (bggreen "pkg-path") pkg-path)))
                          ;; (error 'STOP "STOP emit fgs")
                          (if clients  ;; FIXME FIXME: make this work again
                              (format outp "    visibility = [~{\"//~A:__pkg__\"~^, ~}]~%" clients)
                              (format outp "    visibility = [\"//visibility:public\"]~%")))
                              ;; (format outp "    visibility = [\"//visibility:private\"]~%")))

                        (format outp ")~%")
                        (newline outp)))
                    (cdr pkg-filegroups))))
    )

  (if *debugging* (format #t "~A~%" (blue "processing ws-filegroups")))
  (let* ((-ws (if (keyword? ws) (assoc-val ws *mibl-project*) ws))
         ;; (_ (format #t "~A: ~A~%" (uwhite "-ws") -ws))
         (ws-filegroups (car (assoc-val :filegroups -ws)))
         (pkg-path (assoc-val :pkg-path pkg)))
    (if *debugging*
        (begin
          (format #t "~A: ~A~%" (uwhite "ws-filegroups tbl") ws-filegroups)
          (format #t "~A: ~A~%" (uwhite "pkg-path") pkg-path)))
    (if ws-filegroups
        (begin
          (for-each (lambda (kv)
                      (if *debugging*
                          (format #t "~A: ~A~%" (uyellow "fg") kv))
                      (if (equal? pkg-path (car kv))
                          (let ((key (car kv))
                                (fg (cadr kv)))
                            (if *debugging*
                                (format #t "~A: ~A~%" (white "filegroup") fg))
                            (let* ((glob? (eq? :glob (caadr fg)))
                                   (name  (if (eq? ::all (car fg)) "__all__" (car fg)))
                                   (pattern (cdadr fg))
                                   (pattern (if glob?
                                                (format #f "glob([~S])" pattern)
                                                (format #f "[~S]" pattern))))
                              (if *debugging*
                                  (format #t "~A: ~A~%" (white "glob") glob))
                              (format outp "##########~%")
                              (format outp "filegroup(~%")
                              (format outp "    name = \"~A\",~%" name)
                              ;; (keyword->symbol (car fg)))
                              (format outp "    srcs = ~A,~%" pattern)
                              (if-let ((client (assoc :client fg)))
                                      (begin
                                        (if *debugging*
                                            (begin
                                              (format #t "~A: ~A~%" (bggreen "client") (car client))
                                              (format #t "~A: ~A~%" (bggreen "pkg-path") pkg-path)))
                                        (if (equal? (car client) pkg-path)
                                            (format outp "    visibility = [\"//visibility:private\"]~%")
                                            (format outp "    visibility = [\"//~A:__pkg__\"]~%" client)))
                                      (format outp "    visibility = [\"//visibility:public\"]~%"))

                              (format outp ")~%")
                              (newline outp)))))
                    ws-filegroups))))
  (if *debugging*
      (format #t "~A~%" (red "finished starlark-emit-filegroups")))
  )
