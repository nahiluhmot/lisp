;;; lisp.lisp: file executed on startup.

(defun lisp-main ()
  (cond
   ((empty? *argv*)
    (puts "Welcome to the Lisp REPL!")
    (repl))
   (t
    (let ((file (first *argv*)))
      (def *argv* (rest *argv*))
      (load file)))))

(lisp-main)
