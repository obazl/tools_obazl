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
  (format #t "~A: ~A~%" (blue "starlark-emit-filegroups") pkg)
  (let ((filegroups (assoc :filegroups pkg)))
    (format #t "~A: ~A~%" (white "filegroups") filegroups)
    (if filegroups
        (begin
          (newline outp)
          (for-each (lambda (fg)
                      (format #t "~A: ~A~%" (white "filegroup") fg)
                      (let* ((glob? (eq? :glob (caadr fg)))
                             (pattern (cdadr fg))
                             (pattern (if glob?
                                          (format #f "glob([~S])" pattern)
                                          (format #f "[~S]" pattern))))
                        (format #t "~A: ~A~%" (white "glob") glob)
                        (format outp "filegroup(\n")
                        (format outp "    name = \"~A\",\n"
                                (keyword->symbol (car fg)))
                        (format outp "    srcs = ~A,\n" pattern)
                        (format outp "    visibility = [\"//visibility:public\"]\n")
                        (format outp ")\n")
                        (newline outp)))
                    (cdr filegroups))))
    )
  (let* ((-ws (if (keyword? ws) (assoc-val ws -mibl-ws-table) ws))
         ;; (_ (format #t "~A: ~A~%" (uwhite "-ws") -ws))
         (filegroups (car (assoc-val :filegroups -ws))))
    (format #t "~A: ~A~%" (uwhite "filegroups tbl") filegroups)
    (if filegroups
        (begin
          (newline outp)
          (for-each (lambda (kv)
                      (format #t "~A: ~A~%" (uyellow "fg") kv)
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
                          (format outp "    visibility = [\"//visibility:public\"]\n")
                          (format outp ")\n")
                          (newline outp))))
                    filegroups))))
  )
