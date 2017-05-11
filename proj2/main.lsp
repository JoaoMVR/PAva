(defvar *classes-db* (make-hash-table))

(defun add-to-db (class slots)
  (setf (gethash class *classes-db*) slots))

(defun get-from-db (class)
  (gethash class *classes-db*))

(defun make-constructor (name slots)
  `(defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
    (vector ,(symbol-name name) ,@slots)))

; Not working correctly
(defun make-getters (name slots)
  `(,@(mapcar #'(lambda (x) `(defun ,(intern (concatenate 'string (string name) "-" (string-upcase x))) (,name)
                             (aref ,name ,(1+(position x
                                                    slots
                                                    :test #'equal))))) slots)))

;change Recognizer to recognize superclass
(defun make-recognizer (name)
  `(defun ,(intern (concatenate 'string (string name) "?")) (,name)
    (if (string= ,(symbol-name name) (aref ,name ,0))'t 'nil)))

(defmacro def-inheritance-class (classes &rest classlots)
  (let* ((name (if (listp classes) (car classes) classes))
        (inherited (if (listp classes) (cdr classes) nil))
        (slots (if (listp classes) (append classlots (mapcan #'get-from-db inherited)) classlots)))
   `(progn
     ,(make-constructor name slots)
     ;TODO: change this - we should call make-getters
     ,@(mapcar #'(lambda (x) `(defun ,(intern (concatenate 'string (string name) "-" (string-upcase x))) (,name)
                                (aref ,name ,(1+(position x
                                                       slots
                                                       :test #'equal))))) slots)
     ,(make-recognizer name)
      )))


;macro definition.
(defmacro def-class (name &rest slots)
 (add-to-db name slots)
  `(progn
    ;Constructor
    (defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
      (vector ,(symbol-name name) ,@slots))
    ;Getters
    ,@(mapcar #'(lambda (x) `(defun ,(intern (concatenate 'string (string name) "-" (string-upcase x))) (,name)
                               (aref ,name ,(1+(position x
                                                      slots
                                                      :test #'equal))))) slots)
    ;Recognizer (class? obj). It doesn't work yet
    (defun ,(intern (concatenate 'string (string name) "?")) (,name)
      (if (string= ,(symbol-name name) (aref ,name ,0))'t 'nil))
    ))
