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

(export '(defmacro defun))


(%defun* make-defun-body (lambda-list body destructuring-p)
  (let ((lambda-sym (gensym)))
    `(,lambda-sym
       (d-b ,lambda-list nil nil ,lambda-sym
         ,@body))))

(%defmacro* defun (name lambda-list . body)
  `(%defun ,name
     ,@(make-defun-body lambda-list body nil)))

(%defun* make-defmacro-body (lambda-list body)
  (let ((arg-sym (gensym))
        (lambda-sym (gensym))
        (whole-sym (gensym))
        (env-sym (gensym)))
    `(,arg-sym
       (let ((,whole-sym (first ,arg-sym))
             (,lambda-sym (cdr (first ,arg-sym)))
             (,env-sym (second ,arg-sym)))
         (d-b ,lambda-list ,env-sym ,whole-sym ,lambda-sym
           ,@body)))))

(%defmacro* defmacro (name lambda-list . body)
  `(%defmacro ,name
     ,@(make-defmacro-body lambda-list body)))

(%defmacro* lambda (lambda-list . body)
  `(%lambda
     ,@(make-defun-body lambda-list body nil)))

(%defmacro* destructuring-lambda (lambda-list . body)
  `(%lambda
     ,@(make-defun-body lambda-list body t)))

(defun funcall (function &rest arguments)
  (apply function arguments))
