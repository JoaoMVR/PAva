(defvar *classes-db* (make-hash-table))

(defun add-to-db (class slots)
  (setf (gethash class *classes-db*) slots))

(defun get-from-db (class)
  (gethash class *classes-db*))

(defmacro def-inheritance-class ((&rest classes) &rest slots)
  (let* ((name (car classes))
        (inherited (cdr classes))
        (slots (append slots (mapc #'get-from-db inherited))))
   `(progn
     (defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
       (vector ,(symbol-name name) ,@slots)))))


;macro definition.
(defmacro def-class (name &rest slots)
 (add-to-db name slots)
  `(progn
    ;Constructor
    (defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
      (vector ,(symbol-name name) ,@slots))
    ;Get slots. Returns the list with the slots of the class
    (defun ,(intern (concatenate 'string "GET-" (string name) "-SLOTS")) ()
      ',slots)
    ;Getters
    ,@(mapcar #'(lambda (x) `(defun ,(intern (concatenate 'string (string name) "-" (string-upcase x))) (,name)
                               (aref ,name ,(1+(position x
                                                      slots
                                                      :test #'equal))))) slots)
    ;Recognizer (class? obj). It doesn't work yet
    (defun ,(intern (concatenate 'string (string name) "?")) (,name)
      (if (string= ,(symbol-name name) (aref ,name ,0))'t 'nil))
    ))
