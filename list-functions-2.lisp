(export '(copy-tree assoc assoc-if assoc-if-not rassoc rassoc-if
          rassoc-if-not sublis nsublis))


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
