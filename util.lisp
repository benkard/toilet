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

(export '(and or not let* list* case cond append reverse macroexpand
          otherwise unless when eq boundp))


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

(%defun* %zerop (integer)
  (fixnum-eq integer 0))

(%defun* %= (int1 int2)
  (send-by-name int1 "isEqual:" int2))

(%defun* %1- (integer)
  (add integer -1))

(%defun* %1+ (integer)
  (add integer 1))

(%defun* qq-expand (object level)
  (if (not (consp object))
      (list 'quote object)
      (cond ((eq 'sys::unquote (car object))
             (if (%= level 1)
                 (car (cdr object))
                 (list 'sys::unquote (qq-expand (car (cdr object)) (%1- level)))))
            ((eq 'sys::quasiquote (car object))
             (if (%zerop level)
                 (qq-expand (car (cdr object)) (%1+ level))
                 (list 'sys::quasiquote (qq-expand (car (cdr object)) (%1+ level)))))
            ((and (consp (car object))
                  (eq 'sys::unquote-splicing (car (car object))))
             (if (%= level 1)
                 (list 'append
                       (car (cdr (car object)))
                       (qq-expand (cdr object) level))
                 (list 'sys::unquote-splicing (qq-expand (car (cdr object))
                                                         (%1- level)))))
            (t (list 'cons
                     (qq-expand (car object) level)
                     (qq-expand (cdr object) level))))))

(%defmacro sys::quasiquote form-and-env
  (qq-expand (car form-and-env) 0))

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
                   (progn ,@(cdr this-clause))
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

(%defmacro* unless (test . body)
  `(if (not ,test) (progn ,@body) nil))

(%defmacro* when (test . body)
  `(if ,test (progn ,@body) nil))

(%defmacro* %shadowing-export (symbol)
  `(progn
     (shadow ',symbol)
     (unexport ',symbol (find-package :sys))
     (unexport ',symbol (find-package :cl))
     (export (intern (symbol-name ',symbol) (find-package :cl)))))

(%shadowing-export eq)
(%defun* eq (x y)
  (sys::eq x y))

(%defun* boundp (symbol)
  (send-by-name (send-by-name (find-objc-class "MLKDynamicContext")
                              "currentContext")
                "boundp:"
                symbol))

(unless (boundp '+nil+)
  (setq +nil+ (gensym)))

(%defun* denullify (x)
  (if (eq x +nil+)
      nil
      x))

(%defun* nullify (x)
  (or x +nil+))
