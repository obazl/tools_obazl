(define modules-ht (make-hash-table))
(load "dune.scm")

(let ((r (modules-fld->submodules-fld
          '(modules A)
          '(:modules (:static (A2 (:mli "a2.mli") (:ml "a2.ml"))
                              (A (:mli "a.mli") (:ml "a.ml")))
                     (:dynamic (D (:ml "d.ml"))))
          modules-ht)))
  bye)

