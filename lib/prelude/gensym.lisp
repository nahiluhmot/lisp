;;; gensym.lisp: utilities for generating unique symbols.

(def *gensym-counter* 0)
(def *gensym-prefix* "gensym-")

(defun gensym ()
  (let ((curr *gensym-counter*)
        (next (+ curr 1)))
    (def *gensym-counter* next)
    (intern (append *gensym-prefix* (to-s curr)))))

(defmacro with-gensym (sym . body)
  `(let ((,sym (gensym)))
     ,@body))

(defmacro with-gensyms (syms . body)
  `(let ,(map (lambda (sym) `(,sym (gensym))) syms)
     ,@body))
