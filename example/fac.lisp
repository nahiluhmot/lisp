;;; fac.lisp: example factorial program.

(defun fac (n)
  (let ((go (lambda (m acc)
              (if (== m 0)
                  acc
                (recur (- m 1) (* m acc))))))
    (go n 1)))

(puts (fac (first (read (first *argv*)))))
