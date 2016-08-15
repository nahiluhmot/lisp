;;; primes.lisp: example program which generates prime numbers.

(defun range (start end)
  (let ((go (lambda (curr acc)
              (if (> curr end)
                  acc
                (recur (+ curr 1) (snoc acc curr))))))
    (go start ())))

(defun primes-to (n)
  (let ((go (lambda (curr acc primes)
              (let ((filtered
                     (select (lambda (prime) (!= 0 (% prime curr))) acc))
                    (next-primes (snoc primes curr)))
                (if (>= (* curr curr) n)
                    (append next-primes filtered)
                  (recur (first filtered) (rest filtered) next-primes))))))
    (if (>= n 2)
        (go 2 (range 3 n) ()))))

(puts (primes-to (first (read (first *argv*)))))
