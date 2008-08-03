;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: core.lisp,v 1.30 2004/05/26 07:57:52 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; objects
;; function
(defun fdefinition (function-name)
  (etypecase function-name
    (symbol (symbol-function function-name))
    (setf-function-name )))

;; (defsetf fdefinition (function-name) (new-function)
;;   ;;FIXME
;;   )


;; data and control flow
(defconstant call-arguments-limit
  50)

(defconstant lambda-parameters-limit
  50)

(defconstant lambda-list-keywords
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(defmacro defparameter (name initial-value 
			&optional (documentation nil documentation-p))
  `(progn (declaim (special ,name))
    (setf (symbol-value ',name) ,initial-value)
    ,(when documentation-p
	   `(setf (documentation ',name 'variable) ',documentation))
    ',name))

(defmacro defvar (name &optional
		  (initial-value nil initial-value-p)
		  (documentation nil documentation-p))
  `(progn (declaim (special ,name))
    ,(when initial-value-p
	   `(unless (boundp ',name)
	     (setf (symbol-value ',name) ,initial-value)))
    ,(when documentation-p
	   `(setf (documentation ',name 'variable) ',documentation))
    ',name))

(defun eql (x y)
  (or (eq x y)
      (and (numberp x) (numberp y) (= x y) (eq (type-of x) (type-of y)))
      (and (characterp x) (characterp y) (char= x y))))

(defun equalp (x y)
  (cond
   ((eq x y) t)
   ((characterp x) (and (characterp y) (char-equal x y)))
   ((numberp x) (and (numberp y) (= x y)))
   ((consp x) (and (consp y) (equalp (car x) (car y)) (equalp (cdr x) (cdr y))))
   ((arrayp x) (and (arrayp y)
		    (equal (array-dimensions x) (array-dimensions y))
		    (dotimes (i (array-total-size x) t)
		      (unless (equalp (row-major-aref x i) (row-major-aref y i))
			(return nil)))))
   ((hash-table-p x) (and (hash-table-p y)
			  (= (hash-table-count x) (hash-table-count y))
			  (eq (hash-table-test x) (hash-table-test y))
			  (with-hash-table-iterator (get x)
			    (loop
			     (multiple-value-bind (entry-returned key x-value)
				 (get)
			       (unless entry-returned
				 (return t))
			       (multiple-value-bind (y-value present-p)
				   (gethash key y)
				 (unless (and present-p (equalp x-value y-value))
				   (return nil))))))))
   ((typep x 'structure-object) (and (typep x 'structure-object)
				     (eq (class-of x) (class-of y))
				     ))
   (t nil)))
