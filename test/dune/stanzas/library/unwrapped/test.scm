(define modules-ht (make-hash-table))

(begin
  (load "dune.scm")
  (if (equal? '(A)
              (modules-fld->submodules-fld
               '(modules A)
               '(:modules (:static (A (:mli "a.mli") (:ml "a.ml")))
                          (:dynamic (D (:ml "d.ml"))))
               modules-ht))
      (format #t "passed test A\n")
      (error 'failed-test "failed test A"))
  )

(begin
  (load "dune.scm")
  (if (equal? '(A D)
              (modules-fld->submodules-fld
               '(modules A D)
               '(:modules (:static (A (:mli "a.mli") (:ml "a.ml")))
                          (:dynamic (D (:ml "d.ml"))))
               modules-ht))
      (format #t "passed test B\n")
      (error 'failed-test "failed test B"))
  )


