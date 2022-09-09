(define (emit-cc-target outp cc)
  (format outp "## cc: ~A~%" cc)
  (format #t "~A: ~A~%" (ublue "emit-cc-target") cc)
  (format outp "cc_library(\n")
  (format outp "    name = \"~A.stubs\",~%" (assoc-val :name cc))
  (format outp "    srcs = [~{\"~A.c\"~^, ~}]," (assoc-val :srcs cc))
  ;; (format outp "    ~%")
  ;; (format outp "    ~%")
  ;; (format outp "    ~%")
  ;; (format outp "    ~%")
  ;; (format outp "    ~%")
  ;; (format outp "    ~%")
  (newline outp)
  (format outp ")~%")
  )

(define (starlark-emit-cc-targets outp ws pkg)
  (format #t "~A: ~A\n" (bgblue "starlark-emit-cc-targets") pkg)
  (let ((stanzas (assoc-val :dune pkg)))
    (for-each
     (lambda (stanza)
       (format #t "~A: ~A\n" (uwhite "stanza") stanza)
       (let ((cc (assoc-val :cc-stubs (cdr stanza))))
         (if cc
             (emit-cc-target outp cc))))
     stanzas)))


