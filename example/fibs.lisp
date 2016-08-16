;;; fibs.lisp: example fibonacci program

(defun fibs (n)
  (let ((go (lambda (count a b acc)
              (if (> count n)
                  acc
                (recur (+ count 1) b (+ a b) (snoc acc a))))))
    (go 0 0 1 ())))

(fibs (first (read (first *argv*))))
