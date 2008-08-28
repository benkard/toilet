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

(require "util.lisp")
(require "defun-0.lisp")
(require "list-functions.lisp")
(require "destructuring-bind.lisp")
(require "defun-1.lisp")
(require "list-functions.lisp")
(require "reader.lisp")
(require "sharpsign.lisp")
(require "control-flow.lisp")
(require "types.lisp")
(require "numbers.lisp")
(require "list-functions-2.lisp")
(require "ffi.lisp")

(require "Sacla/share.lisp")
(require "Sacla/do.lisp")

(require "evaluation.lisp")

(require "Sacla/share-2.lisp")

(require "Sacla/data-and-control.lisp")

(require "array.lisp")
(require "Sacla/array.lisp")

(require "string.lisp")
(require "package.lisp")

(setq *system-initialised-p* t)

(in-package #:common-lisp-user)
