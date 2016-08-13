(def defmacro (macro (name args . body)
                     `(def ,name (macro ,args ,@body))))

(defmacro defun (name args . body)
  `(def ,name (lambda ,args ,@body)))
