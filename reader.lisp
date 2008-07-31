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

(export '(read read-preserving-whitespace read-from-string))

(defun read (&optional stream eof-error-p eof-value recursive-p)
  (send-by-name (find-objc-class "MLKReader")
                "readFromStream:eofError:eofValue:recursive:preserveWhitespace:"
                stream
                eof-error-p             ;FIXME: this isn't a BOOL
                eof-value
                recursive-p
                nil))                   ;FIXME: this neither

(defun read-preserving-whitespace (&optional stream eof-error-p eof-value recursive-p)
  (send-by-name (find-objc-class "MLKReader")
                "readFromStream:eofError:eofValue:recursive:preserveWhitespace:"
                stream
                eof-error-p  ;FIXME: this isn't a BOOL
                eof-value
                recursive-p
                t))          ;FIXME: this neither

(defun read-from-string (string &optional eof-error-p eof-value
                         &key start end preserve-whitespace)
  (let ((stream (send-by-name (find-objc-class "MLKStringInputStream")
                              "streamWithString:"
                              string)))
    (send-by-name (find-objc-class "MLKReader")
                  "readFromStream:eofError:eofValue:recursive:preserveWhitespace:"
                  stream
                  eof-error-p
                  eof-value
                  nil
                  preserve-whitespace)))

(defun set-dispatch-macro-character (char subchar function &optional readtable)
  (unless readtable
    (setq readtable *readtable*))
  (let ((dispatcher (send-by-name readtable "macroFunctionForCharacter:" char)))
    (send-by-name dispatcher "setMacroFunction:forCharacter:" function subchar)))
