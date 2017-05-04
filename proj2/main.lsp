(defmacro def-class (name &rest slots)
  `(progn
     (defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
       (vector ,@slots))
     (defun ,(intern (concatenate 'string "GET-" (string name) "-SLOTS")) ()
       ',slots)

     ,(setq counter 0)
     
     ,@(mapcar #'(lambda (x) `(defun ,(intern (concatenate 'string (string name) "-" (string-upcase x))) (,name)
                               (aref ,name ,counter))
                              (setq counter (1+ counter))) slots)))

