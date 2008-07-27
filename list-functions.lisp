(%defmacro pushq args
  (list* 'setq (car (cdr args)) (car args)))

(%defun first args
  (car (car args)))


;;;;-----------------------------------------------------------------
;;;; THE CxR FUNCTIONS
;;;;-----------------------------------------------------------------
(%defun caaaar args
  (car (caaar (first args))))

(%defun caaadr args
  (car (caadr (first args))))

(%defun caaar args
  (car (caar (first args))))

(%defun caadar args
  (car (cadar (first args))))

(%defun caaddr args
  (car (caddr (first args))))

(%defun caadr args
  (car (cadr (first args))))

(%defun caar args
  (car (car (first args))))

(%defun cadaar args
  (car (cdaar (first args))))

(%defun cadadr args
  (car (cdadr (first args))))

(%defun cadar args
  (car (cdar (first args))))

(%defun caddar args
  (car (cddar (first args))))

(%defun cadddr args
  (car (cdddr (first args))))

(%defun caddr args
  (car (cddr (first args))))

(%defun cadr args
  (car (cdr (first args))))

(shadow 'car)
(unexport 'sys::car (find-package :sys))
(%defun car args
  (sys::car (first args)))

(%defun cdaaar args
  (cdr (caaar (first args))))

(%defun cdaadr args
  (cdr (caadr (first args))))

(%defun cdaar args
  (cdr (caar (first args))))

(%defun cdadar args
  (cdr (cadar (first args))))

(%defun cdaddr args
  (cdr (caddr (first args))))

(%defun cdadr args
  (cdr (cadr (first args))))

(%defun cdar args
  (cdr (car (first args))))

(%defun cddaar args
  (cdr (cdaar (first args))))

(%defun cddadr args
  (cdr (cdadr (first args))))

(%defun cddar args
  (cdr (cdar (first args))))

(%defun cdddar args
  (cdr (cddar (first args))))

(%defun cddddr args
  (cdr (cdddr (first args))))

(%defun cdddr args
  (cdr (cddr (first args))))

(%defun cddr args
  (cdr (cdr (first args))))

(shadow 'cdr)
(unexport 'sys::cdr (find-package :sys))
(%defun cdr args
  (sys::cdr (first args)))


;;;;-----------------------------------------------------------------
;;;; SECOND ... TENTH
;;;;-----------------------------------------------------------------
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


;;;;-----------------------------------------------------------------
;;;; CONS
;;;;-----------------------------------------------------------------
(shadow 'cons)
(unexport 'sys::cons (find-package :sys))
(%defun cons args
  (sys::cons (first args) (second args)))


;;;;-----------------------------------------------------------------
;;;; TYPE PREDICATES
;;;;-----------------------------------------------------------------
(shadow '(consp listp null atom))
(unexport '(sys::consp sys::listp sys::null sys::atom) (find-package :sys))

(%defun consp args
  (sys::consp (first args)))

(%defun listp args
  (sys::listp (first args)))

(%defun null args
  (sys::null (first args)))

(%defun atom args
  (sys::atom (first args)))

;(%deftype cons args '(satisfies consp))
;(%deftype list args '(satisfies listp))
;(%deftype null args '(satisfies null))
;(%deftype atom args '(satisfies atom))


;;;;-----------------------------------------------------------------
;;;; OTHER PREDICATES
;;;;-----------------------------------------------------------------
(%defun endp args
  (let ((thing (first args)))
    ;;FIXME (check-type thing (first args) list)
    (null thing)))


;;;;-----------------------------------------------------------------
;;;; ACCESSORS
;;;;-----------------------------------------------------------------
(shadow '(rplaca rplacd))
(unexport '(sys::rplaca sys::rplacd) (find-package :sys))

(%defun rplaca args
  (sys::rplaca (first args) (second args)))

(%defun rplacd args
  (sys::rplacd (first args) (second args)))


;;;;-----------------------------------------------------------------
(export '(cons car cdr list* first second third fourth fifth sixth
          seventh eigthth ninth tenth atom consp listp null rplaca
          rplacd))
