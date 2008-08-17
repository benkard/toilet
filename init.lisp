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

(%fset 'test (compile '(sys::%lambda args)))
(ns-log (test 'a 'b 'c))
(%fset 'test2 (compile '(sys::%lambda args args)))
(ns-log (test2 1 2 3))
(%fset 'test3 (compile '(sys::%lambda args (test args))))
(ns-log (test3 1 2 3))
(%fset 'test4 (compile '(sys::%lambda args (progn args args args))))
(ns-log (test4 1 2 3))
(%fset 'test5 (compile '(sys::%lambda args (let ((x args)
                                                 (y args))
                                             args x y))))
(ns-log (test5 1 2 3))
(%fset 'test6 (compile '(sys::%lambda args (let ((x 'value-x)
                                                 (y 'value-y))
                                             args x y))))
(ns-log (test6 1 2 3))

;; (load "util.lisp")
;; (load "defun-0.lisp")
;; (load "list-functions.lisp")
;; (load "destructuring-bind.lisp")
;; (load "defun-1.lisp")
;; (load "list-functions.lisp")
;; (load "reader.lisp")
;; (load "sharpsign.lisp")
;; (load "control-flow.lisp")
;; (load "types.lisp")
;; (load "numbers.lisp")
;; (load "list-functions-2.lisp")
;; (load "ffi.lisp")

;; (load "Sacla/share.lisp")
;; (load "Sacla/do.lisp")

;; (load "evaluation.lisp")

;; (load "Sacla/share-2.lisp")

;; (load "Sacla/data-and-control.lisp")

;; (load "array.lisp")
;; (load "Sacla/array.lisp")

;; (load "string.lisp")
;; (load "package.lisp")

(setq *system-initialised-p* t)

(in-package #:common-lisp-user)
