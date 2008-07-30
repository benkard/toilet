(export '(identity constantly complement tagbody go block return-from
          defconstant))


(defun identity (x)
  x)

(defun constantly (c)
  (lambda (x)
    (declare (ignore x))
    c))

(defun complement (function)
  (lambda (x) (not (funcall function x))))


(defmacro defconstant (name value &optional documentation)
  `(setq ,name ,value))


;; FIXME: Should be (EVAL-WHEN (:compile-toplevel) ...).
(unless (boundp '+block-mapping-sym+)
  (defconstant +block-mapping-sym+ (gensym "BLOCK-NAME")))

(defmacro #.+block-mapping-sym+ () nil)

(defmacro block (block-name &body body)
  (let ((catch-tag (gensym)))
    `(macrolet ((,+block-mapping-sym+ ()
                  `(quote ,(acons ',block-name
                                  ',catch-tag
                                  (,+block-mapping-sym+)))))
       (catch ',catch-tag
         ,@body))))

(defmacro return-from (block-name &optional value &environment env)
  `(throw ',(cdr (assoc block-name (cadr (macroexpand `(,+block-mapping-sym+)
                                                      env))))
     ,value))


;; FIXME: Should be (EVAL-WHEN (:compile-toplevel) ...).
(unless (boundp '+go-tag-function-mapping-sym+)
  (defconstant +go-tag-function-mapping-sym+ (gensym "GO-FUNCTION"))
  (defconstant +go-tag-catch-tag-mapping-sym+ (gensym "GO-CATCH-TAG")))

(defmacro #.+go-tag-function-mapping-sym+ () nil)
(defmacro #.+go-tag-catch-tag-mapping-sym+ () nil)

(defmacro go (tag &environment env)
  `(throw ',(cdr (assoc tag (cadr (macroexpand `(,+go-tag-catch-tag-mapping-sym+)
                                               env))))
     (function ,(cdr (assoc tag (cadr (macroexpand `(,+go-tag-function-mapping-sym+)
                                                   env)))))))

(defmacro tagbody (&body body)
  (let* (labels-and-catch-tags
         labels-and-functions
         (catch-tag (gensym))
         (block-name (gensym))
         (return-value-sym (gensym))
         (end-marker (gensym))
         (sections
          (mapcon (let (current-label
                        accumulated-clauses
                        current-function)
                    (lambda (clause-and-rest)
                      (let ((clause (car clause-and-rest))
                            (rest (cdr clause-and-rest)))
                        (cond
                          ((atom clause)
                           (when current-function
                             (push (cons current-label current-function)
                                   labels-and-functions)
                             (push (cons current-label catch-tag)
                                   labels-and-catch-tags))
                           (let ((old-function current-function))
                             (setq current-label clause
                                   current-function (gensym))
                             (prog1
                               `((,old-function ()
                                    ,@(nreverse accumulated-clauses)
                                    ,(if rest `#',current-function `nil))
                                 ,@(when (endp rest)
                                     `(,current-function ()
                                        ',end-marker)))
                               (setq accumulated-clauses nil))))
                          (t (push clause accumulated-clauses)
                             (if (endp rest)
                                 (progn
                                   (when current-function
                                     (push (cons current-label current-function)
                                           labels-and-functions)
                                     (push (cons current-label catch-tag)
                                           labels-and-catch-tags))
                                   `((,current-function ()
                                        ,@(nreverse accumulated-clauses)
                                        ',end-marker)))
                                 nil))))))
                   body)))
    `(macrolet ((,+go-tag-catch-tag-mapping-sym+ ()
                  (list 'quote
                        (list* ,@(mapcar (lambda (x) (list 'quote x))
                                         labels-and-catch-tags)
                               (,+go-tag-catch-tag-mapping-sym+))))
                (,+go-tag-function-mapping-sym+ ()
                  (list 'quote
                        (list* ,@(mapcar (lambda (x) (list 'quote x))
                                         labels-and-functions)
                               (,+go-tag-function-mapping-sym+)))))
       (labels (,@(rest sections))
         (block ,block-name
           (let (,return-value-sym)
             (%loop
               (setq ,return-value-sym
                     (catch ',catch-tag
                       (if ,return-value-sym
                           (funcall ,return-value-sym)
                           (progn ,@(cddr (first sections))))))
               (when (eq ,return-value-sym ',end-marker)
                 (return-from ,block-name nil)))))))))
