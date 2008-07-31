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

(defun |#\-READER| (stream char &optional arg)
  ;; FIXME: Handle #\Newline etc..
  (send-by-name stream "readChar"))

(set-dispatch-macro-character (send-by-name (find-objc-class "MLKCharacter")
                                            "characterWithUnichar:"
                                            35)
                              (send-by-name (find-objc-class "MLKCharacter")
                                            "characterWithUnichar:"
                                            92)
                              (function |#\-READER|))


(defun |#'-READER| (stream char &optional arg)
  `(function ,(read stream t nil t)))

(set-dispatch-macro-character #\# #\' (function |#'-READER|))


(defun |#.-READER| (stream char &optional arg)
  (eval (read stream t nil t)))

(set-dispatch-macro-character #\# #\. (function |#.-READER|))
