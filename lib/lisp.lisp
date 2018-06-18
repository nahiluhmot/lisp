;;; lisp.lisp: file executed on startup.

(defun lisp-main ()
  (on-error (lambda (err) (puts err) (exit 1))
    (if (empty? *argv*)
        (do
         (println "Welcome to the Lisp REPL!")
         (repl))
      (let ((file (first *argv*)))
        (def *argv* (rest *argv*))
        (load file)))))

(lisp-main)
