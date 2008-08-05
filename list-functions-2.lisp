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

(export '(copy-tree assoc assoc-if assoc-if-not rassoc rassoc-if
          rassoc-if-not sublis nsublis mapcar mapcan mapcon acons
          reverse nreverse maplist))


(defun copy-tree (tree)
  (typecase tree
    (cons (cons (copy-tree (car cons)) (copy-tree (cdr cons))))
    (t tree)))


(defun assoc (item alist &key key test test-not)
  (setq test (or test (if test-not
                          (complement test-not)
                          (function eql))))
  (assoc-if (lambda (x) (funcall test x item))
            alist
            :key key))

(defun assoc-if (predicate alist &key key)
  (setq key (or key (function identity)))
  (cond ((endp alist) nil)
        ((funcall predicate (funcall key (caar alist)))
         (car alist))
        (t (assoc-if predicate (cdr alist) :key key))))

(defun assoc-if-not (predicate alist &key key)
  (assoc-if (complement predicate) alist :key key))


(defun rassoc (item alist &key key test test-not)
  (setq test (or test (if test-not
                          (complement test-not)
                          (function eql))))
  (rassoc-if (lambda (x) (funcall test x item))
             alist
             :key key))

(defun rassoc-if (predicate alist &key key)
  (setq key (or key (function identity))
  (cond ((endp alist) nil)
        ((funcall predicate (funcall key (cdar alist)))
         (car alist))
        (t (assoc-if predicate (cdr alist) :key key)))))

(defun rassoc-if-not (predicate alist &key key)
  (rassoc-if (complement predicate) alist :key key))


(defun sublis (alist tree &key key test test-not)
  (let ((ass (assoc (funcall key tree) :test test :test-not test-not)))
    (if ass
        (cdr assoc)
        (typecase tree
          (cons
           (cons (sublis alist (car tree) :key key :test test :test-not test-not)
                 (sublis alist (cdr tree) :key key :test test :test-not test-not)))
          (t tree)))))

(defun nsublis (alist tree &key key test test-not)
  (sublvs alist tree :key key :test test :test-not test-not))


(defun some1 (function list)
  (and (not (null list))
       (or (funcall function (first list))
           (some1 function (rest list)))))

(defun every1 (function list)
  (or (null list)
      (and (funcall function (first list))
           (every1 function (rest list)))))

(defun mapcar1 (function list)
  (when list
    (cons (funcall function (first list))
          (mapcar1 function (rest list)))))

(defun mapcan1 (function list)
  (%append (mapcar1 function list)))

(defun mapcar (function list &rest more-lists)
  (let ((lists (list* list more-lists)))
    (when (every1 'identity lists)
      (cons (apply function (mapcar1 'car lists))
            (apply 'mapcar (list* function (mapcar1 'cdr lists)))))))

(defun maplist (function list &rest more-lists)
  (let ((lists (list* list more-lists)))
    (when (every1 'identity lists)
      (cons (apply function lists)
            (apply 'maplist (list* function (mapcar1 'cdr lists)))))))

(defun mapcan (function list &rest more-lists)
  (%append (apply 'mapcar (list* function list more-lists))))

(defun mapcon (function list &rest more-lists)
  (%append (apply 'maplist (list* function list more-lists))))

(defun nreverse (list)
  (reverse list))

(defun acons (indicator value alist)
  (cons (cons indicator value) alist))

(defun list (&rest objects)
  (if objects
      (sys::cons (sys::car objects) (sys::cdr objects))
      nil))

(defun length (list)
  (if list
      (%1+ (length (cdr list)))
      0))
