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

(export '(1+ 1- = mod evenp oddp zerop + - *))


(defun 1+ (n)
  (etypecase n
    (fixnum (add-fixnums n 1))
    (integer (%1+ n))))

(defun 1- (n)
  (etypecase n
    (fixnum (subtract-fixnums n 1))
    (integer (%1- n))))

(defun = (x y)
  (etypecase x
    (fixnum (etypecase y
              (fixnum (fixnum-eq x y))
              (integer nil)))
    (integer (etypecase y
               (fixnum nil)
               (integer (%= x y))))))

(defun + (x y)
  (etypecase x
    (fixnum (etypecase y
              (fixnum (add-fixnums x y))
              (integer (send-by-name x "add:" y))))
    (integer (etypecase y
               (integer (send-by-name x "add:" y))))))

(defun - (x y)
  (etypecase x
    (fixnum (etypecase y
              (fixnum (subtract-fixnums x y))
              (integer (send-by-name x "subtract:" y))))
    (integer (etypecase y
               (integer (send-by-name x "subtract:" y))))))

(defun * (x y)
  (etypecase x
    (fixnum (etypecase y
              (fixnum (multiply-fixnums x y))
              (integer (send-by-name x "multiplyWith:" y))))
    (integer (etypecase y
               (integer (send-by-name x "multiplyWith:" y))))))

(defun idiv (x y)
  (etypecase x
    (fixnum (etypecase y
              (fixnum (idivide-fixnums x y))
              (integer (send-by-name x "divideBy:" y))))
    (integer (etypecase y
               (integer (send-by-name x "divideBy:" y))))))

(defun mod (n m)
;;   (if (and (typep n 'fixnum)
;;            (typep m 'fixnum))
;;       (fixnum-mod n m)
;;       (send-by-name n "mod:" m))
  (send-by-name n "mod:" m))


(defun evenp (n)
  (etypecase n
    (fixnum (zerop (mod n 2)))
    (integer (send-by-name n "evenp"))))

(defun oddp (n)
  (not (evenp n)))

(defun zerop (n)
  (etypecase n
    (integer (%zerop n))))
