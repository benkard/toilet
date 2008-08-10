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

(export '(make-package defpackage use-package find-package package packagep))


(defun ns-set-with-list (list)
  (if list
      (send-by-name (find-objc-class "NSSet")
                    "setWithArray:"
                    (send-by-name list "array"))
      (send-by-name (find-objc-class "NSSet")
                    "set")))

(defun make-package (package-name &key nicknames use)
  (let ((package nil)) ;FIXME: (find-package package-name)
    (when package
      (error "Package ˜A is already there" package-name))
    (setq package
          (send-by-name (find-objc-class "MLKPackage") "packageWithName:nicknames:"
                        (etypecase package-name
                          (symbol (symbol-name package-name))
                          (string package-name))
                        (ns-set-with-list nicknames)))
    (use-package use package)
    package))

(defun packagep (thing)
  (send-by-name thing "isKindOfClass:" (find-package "MLKPackage")))

(deftype package ()
  `(satisfies packagep))

(defun find-package (designator)
  (etypecase designator
    (package designator)
    (symbol (find-package (symbol-name symbol)))
    (string (send (find-objc-class "MLKPackage")
                  "findPackage:"
                  string))))

(defun use-package (use-list &optional package)
  (unless package
    (setq package *package*))
  (typecase use-list
    (list (dolist (p use-list)
            (send package "usePackage:" (find-package p))))
    (t (use-package (list use-list) package))))

(defmacro defpackage (package-name &body options)
  (let ((documentation (cdr (assoc :documentation options)))
        (use (cdr (assoc :use options)))
        (nicknames (cdr (assoc :nicknames options)))
        (shadow (cdr (assoc :shadow options)))
        (shadowing-import-from (cdr (assoc :shadowing-import-from options)))
        (import-from (cdr (assoc :import-from options)))
        (export (cdr (assoc :export options)))
        (intern (cdr (assoc :intern options)))
        (size (cdr (assoc :size options)))
        (name (etypecase package-name
                (symbol (symbol-name package-name))
                (string package-name))))
    ;; FIXME
    `(progn (make-package ',package-name :use ',use))))