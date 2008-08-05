;;; -*- mode: lisp; coding: utf-8 -*-
;;; Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
;;; Copyright (C) 2008  Matthias Andreas Benkard.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:common-lisp)

(export '(cons list* first second third fourth fifth sixth seventh
          eigthth ninth tenth atom consp listp null rplaca rplacd caaaar
          caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar
          caddar cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr
          cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr
          rest mapc dolist))


(%defmacro* pushq args
  (list* 'setq (car (cdr args)) (car args)))


;;;;-----------------------------------------------------------------
;;;; THE CxR FUNCTIONS
;;;;-----------------------------------------------------------------
(defun caaaar (list)
  (car (caaar list)))

(defun caaadr (list)
  (car (caadr list)))

(defun caaar (list)
  (car (caar list)))

(defun caadar (list)
  (car (cadar list)))

(defun caaddr (list)
  (car (caddr list)))

(defun caadr (list)
  (car (cadr list)))

(defun caar (list)
  (car (car list)))

(defun cadaar (list)
  (car (cdaar list)))

(defun cadadr (list)
  (car (cdadr list)))

(defun cadar (list)
  (car (cdar list)))

(defun caddar (list)
  (car (cddar list)))

(defun cadddr (list)
  (car (cdddr list)))

(defun caddr (list)
  (car (cddr list)))

(defun cadr (list)
  (car (cdr list)))

(%shadowing-export car)
(defun car (list)
  (sys::car list))

(defun cdaaar (list)
  (cdr (caaar list)))

(defun cdaadr (list)
  (cdr (caadr list)))

(defun cdaar (list)
  (cdr (caar list)))

(defun cdadar (list)
  (cdr (cadar list)))

(defun cdaddr (list)
  (cdr (caddr list)))

(defun cdadr (list)
  (cdr (cadr list)))

(defun cdar (list)
  (cdr (car list)))

(defun cddaar (list)
  (cdr (cdaar list)))

(defun cddadr (list)
  (cdr (cdadr list)))

(defun cddar (list)
  (cdr (cdar list)))

(defun cdddar (list)
  (cdr (cddar list)))

(defun cddddr (list)
  (cdr (cdddr list)))

(defun cdddr (list)
  (cdr (cddr list)))

(defun cddr (list)
  (cdr (cdr list)))

(%shadowing-export cdr)
(defun cdr (list)
  (sys::cdr list))


;;;;-----------------------------------------------------------------
;;;; FIRST ... TENTH, REST
;;;;-----------------------------------------------------------------
(defun first (list)
  (car list))

(defun second (list)
  (cadr list))

(defun third (list)
  (caddr list))

(defun fourth (list)
  (car (cdddr list)))

(defun fifth (list)
  (cadr (cdddr list)))

(defun sixth (list)
  (caddr (cdddr list)))

(defun seventh (list)
  (car (cdddr (cdddr list))))

(defun eigthth (list)
  (cadr (cdddr (cdddr list))))

(defun ninth (list)
  (caddr (cdddr (cdddr list))))

(defun tenth (list)
  (car (cdddr (cdddr (cdddr list)))))

(defun rest (list)
  (cdr list))


;;;;-----------------------------------------------------------------
;;;; CONS
;;;;-----------------------------------------------------------------
(%shadowing-export cons)
(defun cons (x y)
  (sys::cons x y))


;;;;-----------------------------------------------------------------
;;;; TYPE PREDICATES
;;;;-----------------------------------------------------------------
(%shadowing-export consp)
(%shadowing-export listp)
(%shadowing-export null)
(%shadowing-export atom)

(defun consp (x)
  (sys::consp x))

(defun listp (x)
  (sys::listp x))

(defun null (x)
  (sys::null x))

(defun atom (x)
  (sys::atom x))

;(%deftype cons args '(satisfies consp))
;(%deftype list args '(satisfies listp))
;(%deftype null args '(satisfies null))
;(%deftype atom args '(satisfies atom))


;;;;-----------------------------------------------------------------
;;;; OTHER PREDICATES
;;;;-----------------------------------------------------------------
(defun endp (list)
  (let ((thing list))
    ;;FIXME (check-type thing list)
    (null thing)))


;;;;-----------------------------------------------------------------
;;;; ACCESSORS
;;;;-----------------------------------------------------------------
(%shadowing-export rplaca)
(%shadowing-export rplacd)

(defun rplaca (cons new-value)
  (sys::rplaca cons new-value))

(defun rplacd (cons new-value)
  (sys::rplacd cons new-value))


;;;;-----------------------------------------------------------------
;;;; ITERATION
;;;;-----------------------------------------------------------------
(defun mapc (function list)
  ;;FIXME: Rewrite using TAGBODY.
  (when list
    (funcall function (first list))
    (mapc function (rest list))))

(defmacro dolist ((var list &optional result) &body body)
  ;;FIXME: Rewrite using TAGBODY.
  `(progn
     (mapc (lambda (,var) ,@body) ,list)
     ,result))


;;;;-----------------------------------------------------------------
;;;; UTILITIES
;;;;-----------------------------------------------------------------
(defmacro pushq (thing variable)
  (let ((tmp (gensym)))
    `(let ((,tmp ,thing))
       (setq ,variable (cons ,tmp ,variable))
       ,tmp)))


;;;;-----------------------------------------------------------------
