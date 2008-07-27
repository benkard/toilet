(%defmacro* defun (name lambda-list . body)
  (let ((lambda-sym (gensym)))
    `(%defun ,name ,lambda-sym
       (d-b ,lambda-list nil nil ,lambda-sym
         ,@body))))

(%defmacro* defmacro (name lambda-list . body)
  (let ((arg-sym (gensym))
        (lambda-sym (gensym))
        (whole-sym (gensym))
        (env-sym (gensym)))
    `(%defmacro ,name ,arg-sym
       (let ((,whole-sym (first ,arg-sym))
             (,lambda-sym (cdr (first ,arg-sym)))
             (,env-sym (second ,arg-sym)))
         (d-b ,lambda-list ,env-sym ,whole-sym ,lambda-sym
           ,@body)))))


(export '(defmacro defun))
