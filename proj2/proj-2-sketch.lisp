(defparameter *metaclasses* (make-hash-table))

;;;-----------------------------------------------------------------------------

(defun make-name (&rest strings)
  (intern (string-upcase (apply #'concatenate 'string strings))))

(defun make-constructor-name (constructor-name)
  (make-name "make-" (string constructor-name)))

(defun make-class-name (class-name)
  (make-name "class-" (string class-name)))

(defun make-getter-name (class-name slot-name)
  (make-name (string class-name) "-" (string slot-name)))

(defun make-slot-name (slot-name)
  (make-name "slot-" (string slot-name)))

;;;-----------------------------------------------------------------------------

(defun delegate (classes msg arg)
  (dolist (class classes)
    (let ((res (funcall class msg arg)))
      (when res (return res)))))

;;;-----------------------------------------------------------------------------

(defun make-constructor (class-name super-classes keys)
  `(defun ,(make-constructor-name class-name) (&key ,@keys)
     (lambda (&key msg &optional arg)
       (let ((slots ,(mapcar #'make-slot-name keys))
             (class-name ',(make-class-name class-name)))
         (if (and (eql msg 'GET-SLOT-VALUE)
                  (nth-value 1 (gethash arg slots)))
             (gethash arg slots)
             (or (and (eql msg 'RECOGNIZE)
                      (eql arg ))
                 (delegate ,super-classes msg arg)))))))

(defun make-getter (class-name)
  #'(lambda (slot-name)
      `(defun ,(make-getter-name class-name slot-name) (obj)
         (funcall obj
                  :msg 'GET-SLOT-VALUE
                  ',(make-slot-name slot-name)))))

(defun make-recognizer (class-name)
  `(defun ,(make-name (string class-name) "?") (obj)
     (typecase obj
       (function (funcall obj
                          :msg 'RECOGNIZE
                          ',(make-class-name class-name)))
       (t nil))))

;;;-----------------------------------------------------------------------------

(defmacro def-class (classes &rest slots)
  (let* ((class-name    (if (listp classes) (car classes) classes))
         (super-classes (when (listp classes) (cdr classes))))
    `(progn
       (def-metaclass ',(make-class-name class-name)
           ',(mapcar #'make-slot-name slots))
       ,(make-constructor class-name super-classes slots) ; Constructor
       ,@(mapcar (make-getter class-name) slots)          ; Getters
       ,(make-recognizer class-name))))                       ; Recognizer

(defun def-metaclass (class-name slots)
  (setf (gethash class-name *metaclasses*)
        (lambda (&key (msg 'CLASS-NAME))
          (cond
            ((eql msg
                  'CLASS-NAME) class-name)
            ((eql msg
                  'CLASS-SLOTS) slots)))))

