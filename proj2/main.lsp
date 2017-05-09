;macro definition.
(defmacro def-class (name &rest slots)
  `(progn
    ;Constructor
    (defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
      (vector ,(symbol-name name) ,@slots))
    ;Get slots. Returns the list with the slots of the class
    (defun ,(intern (concatenate 'string "GET-" (string name) "-SLOTS")) ()
      ',slots)
    ;Getters
    ,@(mapcar #'(lambda (x) `(defun ,(intern (concatenate 'string (string name) "-" (string-upcase x))) (,name)
                               (aref ,name ,(position x
                                                      slots
                                                      :test #'equal)))) slots)
    ;Recognizer (class? obj). It doesn't work yet
    (defun ,(intern (concatenate 'string (string name) "?")) (obj)
      't)
    ))
