;;; This is the top level file loaded by the lisp interpreter.
;;; It contains definitions of very basic macros (such as defmacro and defun)
;;; and loads the rest of the standard library.

;; Define a macro with the given name, arguments, and body.
(def defmacro (macro (name args . body)
                     `(def ,name (macro ,args ,@body))))

;; Define a function with the given name, arguments, and body.
(defmacro defun (name args . body)
  `(def ,name (lambda ,args ,@body)))

(load "prelude/gensym")
(load "prelude/list")
(load "prelude/macros")
(load "prelude/function")
