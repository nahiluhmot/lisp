;; Define a macro with the given name, arguments, and body.
(def defmacro (macro (name args . body)
                     `(def ,name (macro ,args ,@body))))

;; Define a function with the given name, arguments, and body.
(defmacro defun (name args . body)
  `(def ,name (lambda ,args ,@body)))
