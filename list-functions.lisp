(%defmacro pushq args
  (list* 'setq (car (cdr args)) (car args)))

(%defun first args
  (car (car args)))

(%defun caar args
  (car (cdr (first args))))

(%defun cadr args
  (car (cdr (first args))))

(%defun cdar args
  (car (cdr (first args))))

(%defun cddr args
  (cdr (cdr (first args))))

(%defun caddr args
  (car (cddr (first args))))

(%defun cdddr args
  (cdr (cddr (first args))))

(%defun second args
  (cadr (car args)))

(%defun third args
  (caddr (car args)))

(%defun fourth args
  (car (cdddr (car args))))

(%defun fifth args
  (cadr (cdddr (car args))))

(%defun sixth args
  (caddr (cdddr (car args))))

(%defun seventh args
  (car (cdddr (cdddr (car args)))))

(%defun eigthth args
  (cadr (cdddr (cdddr (car args)))))

(%defun ninth args
  (caddr (cdddr (cdddr (car args)))))

(%defun tenth args
  (car (cdddr (cdddr (cdddr (car args))))))
