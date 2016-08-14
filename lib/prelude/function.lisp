;;; function.lisp: utilities for interacting with functions

(defun partial (func . given)
  (lambda extra
    (apply func (append given extra))))

(defun compose funcs
  (if (not funcs)
      (raise 'arity-error "Expected at least one function to compose"))
  (foldr (lambda (func acc)
           (lambda args
             (func (apply acc args))))
         (last funcs)
         (but-last funcs)))
