(define (starlark-emit-filegroups outp pkg)
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
                    (cdr filegroups))))))
