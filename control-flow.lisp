(export '(identity constantly complement))


(defun identity (x)
  x)

(defun constantly (c)
  (lambda (x)
    (declare (ignore x))
    c))

(defun complement (function)
  (lambda (x) (not (funcall function x))))


;; FIXME: Should be (EVAL-WHEN (:compile-toplevel) ...).
(unless (boundp '+block-mapping-sym+)
  ;; FIXME: Should be DEFCONSTANT.
  (setq +block-mapping-sym+ (gensym)))

(set +block-mapping-sym+ nil)  ;FIXME: should be DEFLEX

(defmacro block (block-name &body body)
  (let ((catch-tag (gensym)))
    `(compiler-let ((,+block-mapping-sym+ (acons ,block-name
                                                 ,catch-tag
                                                 ,+block-mapping-sym+)))
       (catch ,catch-tag
         ,@body))))

(defmacro return-from (block-name &optional value)
  ;; #, is like COMPILE-TIME-VALUE... in a way.
  `(throw (compile-time-value (cdr (assoc ,+block-mapping-sym+)) t)
     ,@(when value (list value))))


;; FIXME: Should be (EVAL-WHEN (:compile-toplevel) ...).
(unless (boundp '+go-tag-function-mapping-sym+)
  ;; FIXME: Should be DEFCONSTANT.
  (setq +go-tag-function-mapping-sym+ (gensym))
  (setq +go-tag-catch-tag-mapping-sym+ (gensym)))

(set +go-tag-function-mapping-sym+ nil)  ;FIXME: should be DEFLEX
(set +go-tag-catch-tag-mapping-sym+ nil)  ;FIXME: should be DEFLEX

;; FIXME: Implement TAGBODY and GO.
(defmacro go (tag)
  `(throw (compile-time-value (cdr (assoc ,+go-tag-catch-tag-mapping-sym+)) t)
     #'#,(cdr (assoc ,+go-tag-function-mapping-sym+)) t))

(defmacro tagbody (&body body)
  (let* (labels-and-catch-tags
         labels-and-functions
         (catch-tag (gensym))
         (block-name (gensym))
         (return-value-sym (gensym))
         (sections
          (nreverse
           (mapcon (let (current-label
                         accumulated-clauses
                         current-function)
                     (lambda (clause-and-rest)
                       (let ((clause (car clause-and-rest))
                             (rest (cdr clause-and-rest)))
                         (if (atom clause)
                             (progn
                               (when current-function
                                 (push (cons current-label current-function)
                                       labels-and-functions)
                                 (push (cons current-label catch-tag)
                                       labels-and-catch-tags))
                               (let ((old-function current-function))
                                 (setq current-label clause
                                       current-function (gensym))
                                 `((,old-function ()
                                      ,@(nreverse accumulated-clauses)
                                      ,(if rest `#',current-function `nil)))))
                             (push clause accumulated-clauses)))))
                   body))))
    `(compiler-let ((,+go-tag-catch-tag-mapping-sym+
                     (list* ,@(mapcar (lambda (x) (list 'quote x))
                                      labels-and-catch-tags)
                            ,+go-tag-catch-tag-mapping-sym+))
                    (,+go-tag-function-mapping-sym+
                     (list* ,@(mapcar (lambda (x) (list 'quote x))
                                      labels-and-functions)
                            ,+go-tag-function-mapping-sym+)))
       (labels (,@(rest sections))
         (block ,block-name
           (let (,return-value-sym)
             (loop-forever
               (setq ,return-value-sym
                     (catch ,catch-tag
                       (if ,return-value-sym
                           (funcall ,return-value-sym)
                           (progn ,@(cddr (first sections))))
                       (return-from ,block-name nil))))))))))
