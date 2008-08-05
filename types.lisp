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

(export '(most-positive-fixnum most-negative-fixnum type-of typep
          subtypep and or not satisfies symbol fixnum bignum real
          complex float integer ratio rational number short-float
          long-float single-float double-float null symbol list cons
          standard-char base-char extended-char string vector bit-vector
          array simple-array simple-vector simple-string
          simple-bit-vector sequence two-way-stream stream echo-stream
          broadcast-stream file-stream synonym-stream string-stream
          concatenated-stream deftype typecase etypecase))


(setq *subtype-supertypes-dict*
      (let ((relationship-alist
             '((base-char . (character))
               (bignum . (integer))
               (bit-vector . (vector))
               (broadcast-stream . (stream))
               (complex . (number))
               (concatenated-stream . (stream))
               (cons . (list))
               (double-float . (float))
               (echo-stream . (stream))
               (extended-char . (character))
               (file-stream . (stream))
               (fixnum . (integer))
               (float . (real))
               (integer . (rational))
               (list . (sequence))
               (long-float . (float))
               (null . (list symbol))
               (ratio . (rational))
               (rational . (real))
               (real . (number))
               (short-float . (float))
               (simple-array . (array))
               (simple-bit-vector . (simple-array))
               (simple-string . (simple-array))
               (simple-vector . (simple-array))
               (single-float . (float))
               (standard-char . (base-char))
               (string . (vector))
               (string-stream . (stream))
               (synonym-stream . (stream))
               (two-way-stream . (stream))
               (vector . (array sequence))))
            (dict (send-by-name (find-objc-class "NSMutableDictionary")
                                "dictionary")))
        (dolist (pair relationship-alist dict)
          (send-by-name dict "setObject:forKey:" (cdr pair) (nullify (car pair))))))


(setq *type-expanders* (send-by-name (find-objc-class "NSMutableDictionary")
                                     "dictionary"))


(%shadowing-export fixnump)
(defun fixnump (thing)
  (sys::fixnump thing))

;; (setq most-positive-fixnum 32767)
;; (setq most-negative-fixnum -32768)


(defun type-of (thing)
  (let ((primitive-type (primitive-type-of thing)))
    (case primitive-type
      ((null symbol cons single-float double-float function package)
       primitive-type)
      (fixnum 'fixnum)
      (integer 'bignum)
      (base-char 'base-char)  ;FIXME
      (sys::lexical-context 'sys::lexical-context)
      (sys::binding 'sys::binding)
      (stream 'stream)  ;FIXME
      (sys::exception 'sys::exception)
      (array 'array)  ;FIXME
      (otherwise t))))  ;FIXME: classes and struct types, DEFTYPE


(defun some1 (function list)
  (and (not (null list))
       (or (funcall function (first list))
           (some1 function (rest list)))))

(defun every1 (function list)
  (or (null list)
      (and (funcall function (first list))
           (every1 function (rest list)))))


(defmacro deftype (type-name lambda-list &body body)
  `(send-by-name *type-expanders*
                 "setObject:forKey:"
                 (destructuring-lambda ,lambda-list ,@body)
                 ',type-name))

(defun expand-type (type &optional env)
  (let* ((expansion-1 (expand-type-1 type env))
         (expansion-2 (expand-type-1 expansion-1 env)))
    (if (list-eqp expansion-1 expansion-2)
        expansion-1
        (expand-type expansion-2))))

(defun expand-type-1 (type &optional environment)
  (let ((expander (send-by-name *type-expanders*
                                "objectForKey:"
                                (if (listp type) (first type) type))))
    (if expander
        (apply expander (if (listp type) (rest type) nil))
        type)))

(defun typep (thing typespec &optional environment)
  ;;FIXME: DEFTYPE
  (let ((type (type-of thing))
        (typespec (expand-type typespec environment)))
    (cond ((eq typespec t) t)
          ((consp typespec)
           (case (first typespec)
             (and (every1 (lambda (x) (typep thing x environment)) (rest typespec)))
             (or (some1 (lambda (x) (typep thing x environment)) (rest typespec)))
             (not (not (typep thing (second typespec) environment)))
             (satisfies (funcall (second typespec) thing))
             (otherwise
              (subtypep type typespec environment))))
          (t (subtypep type typespec environment)))))


(defun subtypep (type1 type2 &optional environment)
  (let ((type1 (expand-type type1 environment))
        (type2 (expand-type type2 environment)))
    (cond ((eq type2 t) t)
          ((eq type1 nil) t)
          ((consp type1)
           (case (first type1)
             (and (some1 (lambda (x) (subtypep x type2 environment)) (rest type1)))
             (or (every1 (lambda (x) (subtypep x type2 environment)) (rest type1)))
             (not (not (subtypep (second type1) type2 environment)))
             (satisfies nil) ;FIXME?
             (otherwise
              ;;FIXME!!
              (subtypep (first type1) type2 environment))))
          ((consp type2)
           (case (first type2)
             (and (every1 (lambda (x) (subtypep type1 x environment)) (rest type2)))
             (or (some1 (lambda (x) (subtypep type1 x environment)) (rest type2)))
             (not nil)     ;FIXME
             (satisfies nil) ;FIXME?
             (otherwise
              ;;FIXME?
              (subtypep type1 (first type2) environment))))
          (t (or (eq type1 type2)
                 (let ((supertypes (send-by-name *subtype-supertypes-dict*
                                                 "objectForKey:"
                                                 type1)))  ;strictly, this should be (nullify type1),
                                                           ;but type1 can't be NIL here
                   (some1 (lambda (x) (subtypep x type2 environment)) supertypes)))))))

(defun numberp (x)
  (typep x 'number))

(defun characterp (x)
  (typep x 'character))

(defmacro typecase (expression &body cases)
  (when cases
    (let ((tmp (gensym))
          (this-case (first cases))
          (rest (rest cases)))
      (if (and (null rest)
               (or (eq (car this-case) t)
                   (eq (car this-case) 'otherwise)))
          `(progn ,@(cdr this-case))
          `(let ((,tmp ,expression))
             (if (typep ,tmp ',(car this-case))
                 (progn ,@(cdr this-case))
                 (typecase ,tmp ,@rest)))))))

(defmacro etypecase (expression &body cases)
  ;; FIXME: Incorrect.
  `(typecase ,expression
     ,@cases
     (otherwise (error "~A fell through ETYPECASE expression" expression))))
