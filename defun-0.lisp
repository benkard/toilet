(%defmacro* defun args
  `(%defun* ,@args))

(%defmacro* defmacro args
  `(%defmacro* ,@args))
