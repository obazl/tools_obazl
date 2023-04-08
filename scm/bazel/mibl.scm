;; bazel/mibl.scm

;; routines for converting mibl model to bazel model

(define mibl->bazel
  (let ((+documentation+ "convert mibl data model to bazel data model")
        (+signature+ '(mibl-bazel pkgs-list)))
    (if *mibl-debug-s7*
        (format #t "~A: ~A~%" (ublue "mibl->bazel")))
    (lambda (pkgs)
      (let ((slark
             (map (lambda (pkg)
                    (if *mibl-debug-s7*
                        (format #t "~A" (assoc-val :pkg-path pkg))))
                  pkgs)))))))

