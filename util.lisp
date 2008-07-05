(%defmacro %defun args
  (list '%fset
        (list 'quote (car (cdr (car args))))
        (cons '%lambda (cdr (cdr (car args))))))
