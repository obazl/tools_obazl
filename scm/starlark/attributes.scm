(define (outputs->outs-attr pkg-path outputs)
  (if *debugging*
      (format #t "~A: ~A\n" (ublue "outputs->outs-attr") outputs))
  (let* ((outs (map (lambda (out-tlbl)
                      (if *debugging*
                          (format #t "output item: ~A\n" out-tlbl))
                      (let* ((tag (car out-tlbl))
                             (_ (if *debugging*
                                    (format #t "~A: ~A~%" (yellow "tag") tag)))
                             (label (cdr out-tlbl))
                             (_ (if *debugging*
                                    (format #t "~A: ~A~%" (yellow "label") label)))
                             (pkg (assoc-val :pkg label))
                             (_ (if *debugging*
                                    (format #t "~A: ~A~%" (yellow "pkg") pkg)))
                             (tgt (if-let ((t (assoc-val :tgt label)))
                                          t (assoc-val :fg label)))
                             (_ (if *debugging*
                                    (format #t "~A: ~A~%" (yellow "tgt") tgt)))
                             )
                        (if (or (equal? pkg pkg-path)
                                (equal? pkg "./"))
                            tgt
                            ;; should not happen?
                            (format #f "~A:~A" pkg tgt))))
                        outputs))
         (outs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() outs)))
    (if *debugging*
        (format #t "OUTPUTS: ~A\n" outs))
    outs))

(define (deps->srcs-attr pkg-path tool deps)
  (if *debugging*
      (format #t "~A: ~A\n" (ublue "deps->srcs-attr") deps))
  ;; deps is a list of alists; key :maps to list of (:pkg :file) pairs
  ;; key :_ (anonymous) may map to multiple pairs
  ;; other keys are dune 'variables', each mapping to one (:pkg :file) pair

  (if deps
      (let* ((srcs (map (lambda (dep)
                          (if *debugging* (format #t "dep: ~A~%" dep))
                          (cond
                           ((equal? (car dep) tool) (begin)) ;; skip, it goes in tools attr
                           ((eq? (car dep) ::tools) ;; form: (::tools (:foo (:pkg x) (:tgt y)) (:bar (:pkg a)(:tgt b)))
                            (begin))
                           ((eq? (car dep) ::glob) ;; anonymous glob: (::glob (:pkg . x) (:glob . y))
                            ;;(find-in-filegroups dep)
                            (format #f "//~A:~A" (assoc-val :pkg (cdr dep)) (assoc-val :glob (cdr dep))))
                           (else
                            (let* ((tag (car dep))
                                   (_ (if *debugging* (format #t "~A: ~A~%" (yellow "tag") tag)))
                                   (label (cdr dep))
                                   (_ (if *debugging* (format #t "~A: ~A~%" (yellow "label") label))))
                              (cond
                               ((alist? label)
                                (let* ((pkg (cdr (assoc :pkg label)))
                                       (ws (if-let ((ws (assoc-val :ws label)))
                                                   ws "")))
                                  (cond
                                   ((assoc-val :tgt label)
                                    (if (equal? pkg-path pkg)
                                        (format #f ":~A" (assoc-val :tgt label))
                                        (format #f "~A//~A:~A" ws pkg (assoc-val :tgt label))))
                                   ((assoc-val :tgts label)
                                    (if (equal? pkg-path pkg)
                                        (format #f ":~A" (assoc-val :tgts label))
                                        (format #f "~A//~A:~A" ws pkg (assoc-val :tgts label))))
                                   ((assoc-val :glob label)
                                    (if (equal? pkg-path pkg)
                                        (format #f ":~A" (assoc-val :glob label))
                                        (format #f "~A//~A:~A" ws pkg (assoc-val :glob label))))
                                   ((assoc-val :fg label)
                                    (if (equal? pkg-path pkg)
                                        (format #f ":~A" (assoc-val :fg label))
                                        (format #f "~A//~A:*~A*" ws pkg (assoc-val :fg label))))
                                   (else (error 'FIXME
                                                (format #f "Unrecognized tag in: ~A" label))))))

                               ((keyword? label)
                                (if (equal? ::unresolved label)
                                    (begin
                                      (if *debugging*
                                          (format #t "~A: ~A for ~A~%" (bgred "omitting unresolved src lbl") label tag))
                                      (values))
                                    (error 'FIXME (format #f "dunno how to handle this dep: ~A" dep))))

                               ((list? label)
                                (begin
                                  (case (car label)
                                    ((::import)
                                     (if *debugging*
                                         (format #t "~A: ~A~%" (uyellow "::import") tag))
                                     (format #f "~A" tag))
                                    ((::pkg)
                                     (if *debugging*
                                         (format #t "~A: ~A~%" (uyellow "::pkg") tag))
                                     (format #f "~A" tag))
                                    (else
                                     (error 'fixme
                                            (format #f "~A" label))))))
                               (else
                                (error 'FIXME
                                       (format #f "unrecognized form ~A" label))))))))
                        deps))
             (_ (if *debugging* (format #t "~A: ~A~%" (uwhite "prelim srcs") srcs)))

             ;; srcs list may contain mix of strings and sublists
             (srcs (fold (lambda (src accum)
                           (cond
                            ((string? src) (append accum (list src)))
                            ((symbol? src) (append accum (list src)))
                            (else (append accum src))))
                         '() srcs)))
        (if *debugging*
            (format #t "SRCS ATTR: ~A\n" srcs))
        srcs)
      #f))
