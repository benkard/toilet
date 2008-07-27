;; D-B may not expand to (but _may_ itself use!) plain list function
;; calls because these are defined in list-functions.lisp by way of
;; DEFUN, which is in turn based on D-B.  Because of this, we define our
;; own functions here.
(%defun* %car (list)
  (sys::car list))

(%defun* %cdr (list)
  (sys::cdr list))

(setq lambda-list-keywords
      '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(%defmacro* d-b (lambda-list environment whole-sym expression . body)
  ;; (ns-log lambda-list)
  `(let* ,(unless whole-sym
            (let ((real-expression expression))
              (setq whole-sym (gensym "WHOLE")
                    expression (gensym "EXPRESSION"))
              `((,expression ,real-expression)
                (,whole-sym ,expression))))
     ,(cond ((consp lambda-list)
             (case (car lambda-list)
               (&environment
                `(let ((,(cadr lambda-list) ,environment))
                   (d-b ,(cddr lambda-list) ,environment ,whole-sym ,expression
                     ,@body)))
               (&aux
                (if (or (endp (cdr lambda-list))
                        (%member (cadr lambda-list) lambda-list-keywords))
                    `(d-b ,(cdr lambda-list) ,environment ,whole-sym ,expression
                       ,@body)
                    `(let (,(cadr lambda-list))
                       (d-b (&aux ,@(cddr lambda-list)) ,environment ,whole-sym ,expression
                         ,@body))))
               (&optional
                (if (or (endp (cdr lambda-list))
                        (%member (cadr lambda-list) lambda-list-keywords))
                    `(d-b ,(cdr lambda-list) ,environment ,whole-sym ,expression
                       ,@body)
                    (let ((sym (gensym))
                          (head (cadr lambda-list)))
                      `(let* ((,sym ,expression)
                              ,@(cond ((atom head)
                                       `((,head (%car ,sym))))
                                      ((null (cdr head))
                                       `((,(car head) (%car ,sym))))
                                      ((null (cddr head))
                                       `((,(car head) (if (null ,sym)
                                                          ,(cadr head)
                                                          (%car ,sym)))))
                                      (t
                                       `((,(car head) (if (null ,sym)
                                                          ,(cadr head)
                                                          (%car ,sym)))
                                         (,(caddr head) (not (null ,sym)))))))
                         (d-b (&optional ,@(cddr lambda-list)) ,environment ,whole-sym (%cdr ,sym)
                           ,@body)))))
               ((&rest &body)
                (if (%member (cadr lambda-list) lambda-list-keywords)
                    `(d-b ,(cdr lambda-list) ,environment ,whole-sym ,expression
                       ,@body)
                    (let ((sym (gensym)))
                      `(let* ((,sym ,expression)
                              (,(cadr lambda-list) ,sym))
                         (d-b ,(cddr lambda-list) ,environment ,whole-sym ,sym
                           ,@body)))))
               (&whole
                `(let ((,(cadr lambda-list) ,whole-sym))
                   (d-b ,(cddr lambda-list) ,environment ,whole-sym ,expression
                     ,@body)))
               (&allow-other-keys
                `(d-b ,(cdr lambda-list) ,environment ,whole-sym ,expression
                   ,@body))
               (&key
                (if (or (endp (cdr lambda-list))
                        (%member (cadr lambda-list) lambda-list-keywords))
                    `(d-b ,(cdr lambda-list) ,environment ,whole-sym ,expression
                       ,@body)
                    (let* ((sym (gensym))
                           (value-sym (gensym))
                           (missing (gensym "MISSING"))
                           (head (cadr lambda-list))
                           (var (if (consp head)
                                    (if (consp (car head))
                                        (cadar head)
                                        (car head))
                                    head))
                           (keyword-name
                            (if (and (consp head) (consp (car head)))
                                (caar head)
                                (intern (symbol-name var) (find-package '#:keyword)))))
                      `(let* ((,sym ,expression)
                              (,value-sym (getf ,sym ,keyword-name ',missing))
                              ,@(cond ((atom head)
                                       `((,var ,value-sym)))
                                      ((null (cdr head))
                                       `((,var ,value-sym)))
                                      ((null (cddr head))
                                       `((,var (if (eq ,value-sym ',missing)
                                                   ,(cadr head)
                                                   ,value-sym))))
                                      (t
                                       `((,var (if (eq ,value-sym ',missing)
                                                   ,(cadr head)
                                                   ,value-sym))
                                         (,(caddr head) (not (eq ,value-sym ',missing)))))))
                         (d-b (&key ,@(cddr lambda-list)) ,environment ,whole-sym ,sym
                           ,@body)))))
               (otherwise
                (let ((sym (gensym)))
                  `(let ((,sym ,expression))
                     (d-b ,(car lambda-list) ,environment ,whole-sym (%car ,sym)
                       (d-b ,(cdr lambda-list) ,environment ,whole-sym (%cdr ,sym)
                         ,@body)))))))
            ((null lambda-list)
             `(progn ,@body))
            (t `(let ((,lambda-list ,expression))
                  ,@body)))))

(%defmacro* destructuring-bind (tree expression . body)
  `(d-b ,tree nil nil ,expression ,@body))


(export '(destructuring-bind lambda-list-keywords
          &allow-other-keys &aux &body &environment &key &optional &rest
          &whole))
