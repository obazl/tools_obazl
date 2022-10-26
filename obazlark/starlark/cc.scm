(define (emit-cc-target outp cc)
  (format #t "~A: ~A~%" (ublue "emit-cc-target") emit-cc-target)
  (format outp "## cc: ~A~%" cc)
  (format #t "~A: ~A~%" (ublue "emit-cc-target") cc)

  (format outp "################ ################")
  (newline outp)

  (let ((nm (assoc-val :name cc)))

    (format outp "cc_selection_proxy(\n")
    (format outp "    name = \"__lib~A__\",~%" nm)
    (format outp "    selection = select({~%")
    (format outp "        \"@ocaml//platforms:vm?\": [\"dll~A.stubs.so\"],~%" nm)
    (format outp "        \"@ocaml//platforms:sys?\": [\"~A.stubs\"],~%" nm)
    (format outp "        \"//conditions:default\": [\"~A.stubs\"]" nm)
    (format outp "    })~%")
    (format outp ")~%")

    (newline outp)
    (format outp "cc_binary(\n")
    (format outp "    name = \"dll~A.stubs.so\",~%" (assoc-val :name cc))
    (format outp "    linkshared = True,~%")
    (format outp "    srcs = [~{\"~A.c\"~^, ~}],~%" (assoc-val :srcs cc))
    (format outp "    deps = [\"@ocaml//c\"],~%")
    (format outp "    copts = [\"-I\", \"external/ocaml/c\"]~%")
    (format outp ")~%")

    (newline outp)
    (format outp "cc_library(\n")
    (format outp "    name = \"~A.stubs\",~%" (assoc-val :name cc))
    (format outp "    linkstatic = True,~%")
    (format outp "    srcs = [~{\"~A.c\"~^, ~}],~%" (assoc-val :srcs cc))
    (format outp "    deps = [\"@ocaml//c\"],~%")
    (format outp "    copts = [\"-I\", \"external/ocaml/c\"]~%")
    (format outp ")~%"))
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


