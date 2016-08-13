;;; macros.lisp: general purpose macros.

(defmacro and forms
  (if forms
      (foldr (lambda (form acc) `(if ,form ,acc))
             (last forms)
             (but-last forms))))

(defmacro or forms
  (if forms
      (foldr (lambda (form acc)
               (with-gensym curr
                 `(let ((,curr ,form))
                    (if ,curr ,curr ,acc))))
             (last forms)
             (but-last forms))))

(defmacro cond forms
  (foldr (lambda (form acc)
           (if (== 'list (type-of form))
               `(if ,(first form) (do ,@(rest form)) ,acc)
             (raise 'cond-error (append "Unexpected form: " (to-s form)))))
         '(raise 'cond-error "No matching case found")
         forms))
