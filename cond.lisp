(%defun list* args
  (if (null (cdr args))
      (car args)
      (cons (car args)
            (apply 'list* (cdr args)))))

(%defmacro let* args
  (let ((form (car args)))
    (let ((bindings (car (cdr form)))
          (body (cdr (cdr form))))
      (if (null bindings)
          (list* 'let nil body)
          (let ((first-binding (car bindings))
                (rest (cdr bindings)))
            (list 'let
                  (list first-binding)
                  (list* 'let* rest body)))))))

(%defmacro cond args
  (let* ((form (car args))
         (clauses (cdr form))
         (clause (car clauses))
         (rest (cdr clauses)))
    (if (null clauses)
        nil
        (list 'if
              (car clause)
              (cons 'progn (cdr clause))
              (cons 'cond rest)))))
