(define (-tool-for-genrule pkg-path tool deps)
  (if (or *debug-genrules* *debugging*)
      (format #t "~A: ~A~%" (ublue "-tool-for-genrule") tool)
      (format #t "~A: ~A~%" (blue "deps") deps))
  (case tool
    ((::cat) 'cat) ;; FIXME: use lookup table from constants.scm
    ((::deps) (car deps))
    ((::diff) 'diff)
    ((::ocamlc) 'ocamlc)
    ((::node) 'node)

    (else
     ;; not a (builtin) shell tool
     (let* ((tool-deps (if-let ((tds (assoc-val ::tools deps)))
                               tds deps))
            (_ (format #t "~A: ~A~%" (blue "tool-deps") tool-deps))
            (tool (if (keyword? tool)
                      (let* ((tool-tlbl (assoc tool tool-deps))
                             (tool-alist (cdr tool-tlbl))
                             (_ (format #t "~A: ~A~%" (red "tool-alist") tool-alist))
                             (tool-label (assoc-val :lbl tool-alist))
                             (_ (format #t "~A: ~A~%" (red "tool-label") tool-label))
                             (tool-pkg (assoc-val :pkg tool-alist))
                             (_ (format #t "~A: ~A~%" (red "tool-pkg") tool-pkg))
                             (_ (format #t "~A: ~A~%" (red "pkg-path") pkg-path))
                             (tool-tag (caadr tool-alist))
                             (_ (format #t "~A: ~A~%" (cyan "tool-tag") tool-tag))
                             (tool-tgt (case tool-tag
                                         ((:tgt)
                                          (assoc-val :tgt tool-alist))
                                         ((:tgts)
                                          (assoc-val :tgts tool-alist))
                                         ((:fg)
                                          (format #f "*~A*"
                                                  (assoc-val :fg tool-alist)))
                                         (else
                                          (error 'fixme (format #f "~A: ~A~%" (red "unrecognized tool tag") tool-tag))))))
                        (format #t "~A: ~A~%" (red "tool-tgt") tool-tgt)
                        (if tool-label
                            tool-label
                            (if (equal? tool-pkg pkg-path)
                                (begin
                                  (format #f "~A" tool-tgt))
                                (begin
                                  (format #t "~A: ~A~%" (red "YOU") tool-tgt)
                                  (format #f "//~A:~A" tool-pkg tool-tgt)))))
                      ;; else tool must be bash or system?
                      tool
                      )))
       tool))))
