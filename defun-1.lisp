;;; -*- mode: lisp; coding: utf-8 -*-
;;; Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
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


(%defmacro* defun (name lambda-list . body)
  (let ((lambda-sym (gensym)))
    `(%defun ,name ,lambda-sym
       (d-b ,lambda-list nil nil ,lambda-sym
         ,@body))))

(%defmacro* defmacro (name lambda-list . body)
  (let ((arg-sym (gensym))
        (lambda-sym (gensym))
        (whole-sym (gensym))
        (env-sym (gensym)))
    `(%defmacro ,name ,arg-sym
       (let ((,whole-sym (first ,arg-sym))
             (,lambda-sym (cdr (first ,arg-sym)))
             (,env-sym (second ,arg-sym)))
         (d-b ,lambda-list ,env-sym ,whole-sym ,lambda-sym
           ,@body)))))

(%defmacro* lambda (lambda-list . body)
  (let ((lambda-sym (gensym)))
    `(%lambda ,lambda-sym
       (d-b ,lambda-list nil nil ,lambda-sym
         ,@body))))

(defun funcall (function &rest arguments)
  (apply function arguments))
