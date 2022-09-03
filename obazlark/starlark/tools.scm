(define (-tool-for-genrule pkg-path tool deps)
  (format #t "~A: ~A~%" (ublue "-tool-for-genrule") tool)
  (format #t "~A: ~A~%" (blue "deps") deps)
  (case tool
    ((::cat) 'cat) ;; FIXME: use lookup table from constants.scm
    ((::deps)
     (car deps))
    (else
     ;; not a (builtin) shell tool
     (let* ((tool-deps (if-let ((tds (assoc-val ::tools deps)))
                               tds deps))
            (_ (format #t "~A: ~A~%" (blue "tool-deps") tool-deps))
            (tool (if (keyword? tool)
                      (let* ((tool-tlbl (assoc tool tool-deps))
                             (tool-label (cdr tool-tlbl))
                             (_ (format #t "~A: ~A~%" (red "tool-label") tool-label))
                             (tool-pkg (assoc-val :pkg tool-label))
                             (_ (format #t "~A: ~A~%" (red "tool-pkg") tool-pkg))
                             (_ (format #t "~A: ~A~%" (red "pkg-path") pkg-path))
                             (tool-tag (caadr tool-label))
                             (_ (format #t "~A: ~A~%" (cyan "tool-tag") tool-tag))
                             (tool-tgt (case tool-tag
                                         ((:tgt)
                                          (assoc-val :tgt tool-label))
                                         ((:tgts)
                                          (assoc-val :tgts tool-label))
                                         ((:fg)
                                          (format #f "*~A*"
                                                  (assoc-val :fg tool-label)))
                                         (else
                                          (error 'fixme (format #f "~A: ~A~%" (red "unrecognized tool tag") tool-tag))))))
                        (format #t "~A: ~A~%" (red "tool-tgt") tool-tgt)
                        (if (equal? tool-pkg pkg-path)
                            (begin
                              (format #f "~A" tool-tgt))
                            (begin
                              (format #t "~A: ~A~%" (red "YOU") tool-tgt)
                              (format #f "//~A:~A" tool-pkg tool-tgt))))
                      ;; else tool must be bash or system?
                      tool
                      )))
       tool))))
