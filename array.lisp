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

(export '(aref row-major-aref))


(defun row-major-aref (array row-major-index)
  (send-by-name array "idAtIndex:" row-major-index))

(defun (setf row-major-aref) (new-value array row-major-index)
  (send-by-name array "replaceIdAtIndex:withId:" row-major-index new-value))

(defun array-dimensions (array)
  (send-by-name (find-objc-class "MLKCons")
                "listWithArray:"
                (send-by-name array "dimensions")))
