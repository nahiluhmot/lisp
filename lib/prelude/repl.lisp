;; repl.lisp: repl source code

(defun repl ()
  (let ((go (lambda ()
              (print "> ")
              (puts (eval (read (gets))))
              (recur))))
    (on-error (lambda (err) (puts err) (repl))
      (go))))
