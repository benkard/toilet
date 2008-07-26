(%defmacro %defun args
  (list '%fset
        (list 'quote (car (cdr (car args))))
        (cons '%lambda (cdr (cdr (car args))))))

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

(%defun not args
  (if (null (car args)) t nil))

(%defun make-%defmacro*-body args
  (let ((lambda-list (car args))
        (lambda-list-name (car (cdr args)))
        (body (car (cdr (cdr args)))))
    (cond ((null lambda-list) body)
          ((not (listp lambda-list))
           (list
            (list* 'let
                   (list (list lambda-list lambda-list-name))
                   body)))
          (t (let ((lambda-symbol (car lambda-list))
                   (rest-lambda-list (cdr lambda-list))
                   (rest-name (gensym)))
               (list
                (list* 'let
                       (list (list lambda-symbol
                                   (list 'car lambda-list-name))
                             (list rest-name
                                   (list 'cdr lambda-list-name)))
                       (make-%defmacro*-body (cdr lambda-list)
                                             rest-name
                                             body))))))))

(%defmacro %defmacro* args
  (let* ((form (car args))
         (real-args (cdr form)))
    (let ((name (car real-args))
          (lambda-list (car (cdr real-args)))
          (body (cdr (cdr real-args)))
          (macro-lambda-list-name (gensym))
          (lambda-list-name (gensym)))
      (list '%defmacro
            name
            macro-lambda-list-name
            (list* 'let
                   (list (list lambda-list-name
                               (list 'cdr
                                     (list 'car macro-lambda-list-name))))
                   (make-%defmacro*-body lambda-list lambda-list-name body))))))

(%defmacro %defun* args
  (let* ((form (car args))
         (real-args (cdr form)))
    (let ((name (car real-args))
          (lambda-list (car (cdr real-args)))
          (body (cdr (cdr real-args)))
          (lambda-list-name (gensym)))
      (list* '%defun
             name
             lambda-list-name
             (make-%defmacro*-body lambda-list lambda-list-name body)))))

(%defmacro* and expressions
  (cond ((null expressions) t)
        ((null (cdr expressions)) (car expressions))
        (t (list 'if
                 (car expressions)
                 (cons 'and (cdr expressions))
                 nil))))

(%defmacro* or expressions
  (cond ((null expressions) nil)
        ((null (cdr expressions)) (car expressions))
        (t (let ((expr-sym (gensym)))
             (list 'let
                   (list (list expr-sym (car expressions)))
                   (list 'if
                         expr-sym
                         expr-sym
                         (cons 'or (cdr expressions))))))))

(%defun* %member (item list)
  (and list
       (or (and (eq item (car list)) list)
           (%member item (cdr list)))))

(%defmacro* case (object . clauses)
  (let ((this-clause (car clauses))
        (rest (cdr clauses))
        (object-sym (gensym)))
    (if (null clauses)
        nil
        (if (and (null rest)
                 (or (eq (car this-clause) t)
                     (eq (car this-clause) 'otherwise)))
            (cons 'progn (cdr this-clause))
            (list 'let
                  (list (list object-sym object))
                  (list 'if
                        (if (listp (car this-clause))
                            (list '%member
                                  object-sym
                                  (list 'quote (car this-clause)))
                            (list 'eq
                                  object-sym
                                  (list 'quote (car this-clause))))
                        (cons 'progn (cdr this-clause))
                        (list* 'case object-sym rest)))))))
