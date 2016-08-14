;;; list.lisp: list utilities.

(defun empty? (xs)
  (== 'nil (type-of xs)))

(defun foldl (f initial list)
  (let ((go (lambda (acc xs)
              (if xs
                  (recur (f acc (first xs)) (rest xs))
                acc))))
    (go initial list)))

(defun foldr (f initial list)
  (let ((go (lambda (acc xs)
              (if xs
                  (recur (f (last xs) acc) (but-last xs))
                acc))))
    (go initial list)))

(defun unfoldr (f init)
  (let ((go (lambda (curr acc)
              (let ((next (f curr)))
                (if next
                    (recur next (cons curr acc))
                  acc)))))
    (go init ())))

(defun unfoldl (f init)
  (let ((go (lambda (curr acc)
              (let ((next (f curr)))
                (if next
                    (recur next (snoc acc curr))
                  acc)))))
    (go init ())))

(defun reverse (xs)
  (foldl (lambda (x y) (cons y x)) () xs))

(defun map (f xs)
  (foldl (lambda (ys x) (snoc ys (f x))) () xs))

(defun select (f xs)
  (foldl (lambda (ys x) (if (f x) (snoc ys x) ys)) () xs))

(defun reject (f xs)
  (select (lambda (x) (not (f x))) xs))

(defun take-while (f xs)
  (let ((go (lambda (ys acc)
              (if ys
                  (if (f (first ys))
                      (recur (rest ys) (snoc acc (first ys)))
                    acc)
                acc))))
    (go xs ())))

(defun drop-while (f xs)
  (let ((go (lambda (ys)
              (if ys
                  (if (f (first ys))
                      (recur (rest ys))
                    ys)))))
    (go xs)))

(defun flatten (xs)
  (foldr (lambda (x acc)
           (let ((type (type-of x)))
             (if (== type 'list)
                 (append (flatten x) acc)
               (cons x acc))))
         ()
         xs))
