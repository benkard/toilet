(export '(most-positive-fixnum most-negative-fixnum type-of typep
          subtypep and or not satisfies symbol fixnum bignum real
          complex float integer ratio short-float long-float
          single-float double-float null symbol list cons standard-char
          base-char extended-char string vector bit-vector array
          simple-array simple-vector simple-string simple-bit-vector
          sequence two-way-stream stream echo-stream broadcast-stream
          file-stream synonym-stream string-stream concatenated-stream))


;(defvar *type-table*)

(setq *subtype-relationships*
      '((real . number)
        (complex . number)
        (rational . real)
        (float . real)
        (integer . rational)
        (ratio . rational)
        (fixnum . integer)
        (bignum . integer)
        (short-float . float)
        (long-float . float)
        (single-float . float)
        (double-float . float)
        (null . symbol)
        (null . list)
        (cons . list)
        (standard-char . base-char)
        (base-char . character)
        (extended-char . character)
        (string . vector)
        (bit-vector . vector)
        (vector . array)
        (simple-array . array)
        (simple-vector . simple-array)
        (simple-string . simple-array)
        (simple-bit-vector . simple-array)
        (vector . sequence)
        (list . sequence)
        (two-way-stream . stream)
        (echo-stream . stream)
        (broadcast-stream . stream)
        (file-stream . stream)
        (synonym-stream . stream)
        (string-stream . stream)
        (concatenated-stream . stream)))


(setq most-positive-fixnum 32767)
(setq most-negative-fixnum -32768)


(defun type-of (thing)
  (let ((primitive-type (primitive-type-of thing)))
    (case primitive-type
      ((null symbol cons single-float double-float function package)
       primitive-type)
      (integer
       (if (and (send-by-name -1 "isEqual:" (send-by-name thing
                                                          "compare:"
                                                          most-positive-fixnum))
                (send-by-name -1 "isEqual:" (send-by-name most-negative-fixnum
                                                          "compare:"
                                                          thing)))
           'fixnum
           'bignum))
      (base-char 'base-char)  ;FIXME
      (sys::lexical-context 'sys::lexical-context)
      (sys::binding 'sys::binding)
      (stream 'stream)  ;FIXME
      (sys::exception 'sys::exception)
      (array 'array)  ;FIXME
      (otherwise t))))  ;FIXME: classes and struct types, DEFTYPE


(defun mapcar1 (function list)
  (when list
    (cons (funcall function (first list))
          (mapcar1 function (rest list)))))


(defun mapcan1 (function list)
  (%append (mapcar1 function list)))


(defun some1 (function list)
  (and (not (null list))
       (or (funcall function (first list))
           (some1 function (rest list)))))

(defun every1 (function list)
  (or (null list)
      (and (funcall function (first list))
           (every1 function (rest list)))))


(defun expand-type (type)
  ;;FIXME: DEFTYPE
  type)


(defun typep (thing typespec &optional environment)
  ;;FIXME: DEFTYPE
  (let ((type (type-of thing))
        (typespec (expand-type typespec)))
    (if (listp typespec)
        (case (first typespec)
          (and (every1 (lambda (x) (typep thing x environment)) (rest typespec)))
          (or (some1 (lambda (x) (typep thing x environment)) (rest typespec)))
          (not (not (typep thing (second typespec) environment)))
          (satisfies (funcall (second typespec) thing))
          (otherwise
           (subtypep type typespec environment)))
        (subtypep type typespec environment))))


(defun subtypep (type1 type2 &optional environment)
  (let ((type1 (expand-type type1))
        (type2 (expand-type type2)))
    (cond ((listp type1)
           (case (first type1)
             (and (some1 (lambda (x) (subtypep x type2 environment)) (rest type1)))
             (or (every1 (lambda (x) (subtypep x type2 environment)) (rest type1)))
             (not (not (subtypep (second type1) type2 environment)))
             (satisfies nil) ;FIXME?
             (otherwise
              ;;FIXME!!
              (subtypep (first type1) type2 environment))))
          ((listp type2)
           (case (first type2)
             (and (every1 (lambda (x) (subtypep type1 x environment)) (rest type2)))
             (or (some1 (lambda (x) (subtypep type1 x environment)) (rest type2)))
             (not nil)     ;FIXME
             (satisfies nil) ;FIXME?
             (otherwise
              ;;FIXME?
              (subtypep type1 (first type2) environment))))
          (t (or (eq type1 type2)
                 (let ((supertypes (mapcan1 (lambda (x)
                                              (when (eq (car x) type1)
                                                (list (cdr x))))
                                            *subtype-relationships*)))
                   (some1 (lambda (x) (subtypep x type2 environment)) supertypes)))))))
