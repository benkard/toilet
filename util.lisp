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

(%defun* %reverse-helper (list stack)
  (if (null list)
      stack
      (%reverse-helper (cdr list) (cons (car list) stack))))

(%defun* reverse (list)
  (%reverse-helper list nil))

(%defun* %append-helper (reversed-list1 list2)
  (if (null reversed-list1)
      list2
      (%append-helper (cdr reversed-list1) (cons (car reversed-list1) list2))))

(%defun* %append-two-lists (list1 list2)
  (%append-helper (reverse list1) list2))

(%defun* %append (lists)
  (if (null (cdr lists))
      (car lists)
      (let ((first-list (car lists))
            (second-list (car (cdr lists)))
            (rest (cdr (cdr lists))))
        (%append (list* (%append-two-lists first-list second-list) rest)))))

(%defun append lists
  (%append lists))

(%defmacro* sys::quasiquote (object)
  (if (not (consp object))
      (list 'quote object)
      (cond ((eq 'sys::unquote (car object)) (car (cdr object)))
            ((eq 'sys::quasiquote (car object)) (list 'quote object))
            ((and (consp (car object))
                  (eq 'sys::unquote-splicing (car (car object))))
             (list 'append
                   (car (cdr (car object)))
                   (list 'sys::quasiquote (cdr object))))
            (t (list 'cons
                     (list 'sys::quasiquote (car object))
                     (list 'sys::quasiquote (cdr object)))))))

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
            `(progn ,@(cdr this-clause))
            `(let ((,object-sym ,object))
               (if ,(if (listp (car this-clause))
                        `(%member ,object-sym
                                  (quote ,(car this-clause)))
                        `(eq ,object-sym
                             (quote ,(car this-clause))))
                   (progn ,(cdr this-clause))
                   (case ,object-sym ,@rest)))))))

(%defun* list-eqp (list1 list2)
  "Not really EQUALP (only works on trees of symbols)."
  (if (and (consp list1) (consp list2))
      (and (list-eqp (car list1) (car list2))
           (list-eqp (cdr list1) (cdr list2)))
      (eq list1 list2)))

(%defun* macroexpand (object . rest)
  (let* ((env (if rest (car rest) nil))
         (expansion-1 (macroexpand-1 object env))
         (expansion-2 (macroexpand-1 expansion-1 env)))
    (if (list-eqp expansion-1 expansion-2)
        expansion-1
        (macroexpand expansion-2))))

(%defun* macroexpand-all (object . rest)
  (let* ((env (if rest (car rest) nil)))
    (if (consp object)
        (let ((expansion (macroexpand object env)))
          (cons (macroexpand-all (car expansion))
                (macroexpand-all (cdr expansion))))
        object)))

(%defmacro* unless (test . body)
  `(if (not ,test) (progn ,@body) nil))
