;;; fibs.lisp: example fibonacci program

(defun fibs-to (n)
  (let ((go (lambda (a b acc)
              (if (> a n)
                  acc
                (recur b (+ a b) (snoc acc a))))))
    (go 0 1 ())))

(puts (fibs-to (first (read (first *argv*)))))
