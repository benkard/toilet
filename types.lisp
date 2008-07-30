(export '(most-positive-fixnum most-negative-fixnum type-of typep
          subtypep and or not satisfies symbol fixnum bignum real
          complex float integer ratio rational number short-float
          long-float single-float double-float null symbol list cons
          standard-char base-char extended-char string vector bit-vector
          array simple-array simple-vector simple-string
          simple-bit-vector sequence two-way-stream stream echo-stream
          broadcast-stream file-stream synonym-stream string-stream
          concatenated-stream))


(setq *subtype-supertypes-dict*
      (let ((relationship-alist
             '((base-char . (character))
               (bignum . (integer))
               (bit-vector . (vector))
               (broadcast-stream . (stream))
               (complex . (number))
               (concatenated-stream . (stream))
               (cons . (list))
               (double-float . (float))
               (echo-stream . (stream))
               (extended-char . (character))
               (file-stream . (stream))
               (fixnum . (integer))
               (float . (real))
               (integer . (rational))
               (list . (sequence))
               (long-float . (float))
               (null . (list symbol))
               (ratio . (rational))
               (rational . (real))
               (real . (number))
               (short-float . (float))
               (simple-array . (array))
               (simple-bit-vector . (simple-array))
               (simple-string . (simple-array))
               (simple-vector . (simple-array))
               (single-float . (float))
               (standard-char . (base-char))
               (string . (vector))
               (string-stream . (stream))
               (synonym-stream . (stream))
               (two-way-stream . (stream))
               (vector . (array sequence))))
            (dict (send-by-name (find-objc-class "NSMutableDictionary")
                                "dictionary")))
        (dolist (pair relationship-alist dict)
          (send-by-name dict "setObject:forKey:" (cdr pair) (car pair)))))


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


(defun expand-type (type &optional environment)
  ;;FIXME: DEFTYPE
  type)


(defun typep (thing typespec &optional environment)
  ;;FIXME: DEFTYPE
  (let ((type (type-of thing))
        (typespec (expand-type typespec environment)))
    (cond ((eq typespec t) t)
          ((listp typespec)
           (case (first typespec)
             (and (every1 (lambda (x) (typep thing x environment)) (rest typespec)))
             (or (some1 (lambda (x) (typep thing x environment)) (rest typespec)))
             (not (not (typep thing (second typespec) environment)))
             (satisfies (funcall (second typespec) thing))
             (otherwise
              (subtypep type typespec environment))))
          (t (subtypep type typespec environment)))))


(defun subtypep (type1 type2 &optional environment)
  (let ((type1 (expand-type type1 environment))
        (type2 (expand-type type2 environment)))
    (cond ((eq type2 t) t)
          ((eq type1 nil) t)
          ((listp type1)
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
                 (let ((supertypes (send-by-name *subtype-supertypes-dict*
                                                 "objectForKey:"
                                                 type1)))
                   (some1 (lambda (x) (subtypep x type2 environment)) supertypes)))))))
