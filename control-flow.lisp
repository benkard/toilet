(defun identity (x)
  x)

(defun constantly (c)
  (lambda (x)
    (declare (ignore x))
    c))

(defun complement (function)
  (lambda (x) (not (funcall function x))))
