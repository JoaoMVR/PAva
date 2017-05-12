(defparameter *metaclasses* (make-hash-table))

;;;-----------------------------------------------------------------------------

(defun make-name (&rest strings)
  (intern (string-upcase (apply #'concatenate 'string strings))))

(defun make-constructor-name (constructor-name)
  (make-name "make-" (string constructor-name)))

(defun make-class-name (class-name)
  (make-name "class-" (string class-name)))

(defun make-slot-name (slot-name)
  (make-name "slot-" (string slot-name)))

(defun make-getter-name (class-name slot-name)
  (make-name (string class-name) "-" (string slot-name)))

(defun make-recognizer-name (class-name)
  (make-name (string class-name) "?"))

;;;-----------------------------------------------------------------------------

(defun delegate (classes msg arg)
  (dolist (class classes)
    (let ((res (funcall class msg arg)))
      (when res (return res)))))

(defun make-slots-alist (unbound-slots)
  `(mapcar #'(lambda (us)
               )))

(defun make-slots-alist (unbound-slots)
  (cons 'list
        (mapcar #'(lambda (unbound-slot)
                    `(list ',(make-slot-name unbound-slot)
                           ,unbound-slot))
                unbound-slots)))

(defun make-slots (unbound-slots)
  `(let ((slots-alist ,(make-slots-alist unbound-slots))
         (slots-ht (make-hash-table)))
     (progn
       (dolist (key-value slots-alist)
         (setf (gethash (car key-value) slots-ht)
               (cadr key-value)))
       slots-ht)))

(defun recognize (hierarchy class-name-to-recognize)
  (or (mapcar #'(lambda (class-name)
                  (eql class-name-to-recognize
                       (funcall (gethash class-name *metaclasses*)
                                :msg 'CLASS-NAME)))
              hierarchy)))

;;;-----------------------------------------------------------------------------

(defun make-constructor (unbound-class
                         unbound-hierarchy
                         unbound-slots)
  `(defun ,(make-constructor-name unbound-class) (&key ,@unbound-slots)
     (lambda (msg param)
       (let ((slots ,(make-slots unbound-slots))
             ; Slots are wrong, they're missing the ones from the superclasses
             (hierarchy ',(mapcar #'make-class-name unbound-hierarchy)))
         (cond
           ((eql msg 'RECOGNIZE) (recognize hierarchy param))
           ((and (eql msg 'GET-SLOT-VALUE)
                 (nth-value 1 (gethash param slots))) (gethash param slots)))))))

(defun make-getter (class-name)
  #'(lambda (slot-name)
      `(defun ,(make-getter-name class-name slot-name) (obj)
         (funcall obj
                  'GET-SLOT-VALUE
                  ',(make-slot-name slot-name)))))

(defun make-recognizer (class-name)
  `(defun ,(make-recognizer-name (string class-name)) (obj)
     (when (functionp obj)
       (funcall obj
                'RECOGNIZE
                ',(make-class-name class-name)))))

;;;-----------------------------------------------------------------------------

(defmacro def-class (classes &rest slots)
  (let* ((classes (if (listp classes) (car classes) (list classes)))
         (class (car classes)))
    `(progn
       (def-metaclass
           ',(make-class-name class)
           ',(mapcar #'make-slot-name slots))

       ,(make-constructor class classes slots)

       ,@(mapcar (make-getter class) slots)

       ,(make-recognizer class))))

(defun def-metaclass (class-name slots)
  (setf (gethash class-name *metaclasses*)
        (lambda (&key (msg 'CLASS-NAME))
          (cond
            ((eql msg
                  'CLASS-NAME) class-name)
            ((eql msg
                  'CLASS-SLOTS) slots)))))

;;;-----------------------------------------------------------------------------

(def-class person name age) 

(let ((a (make-person :name "Paulo" :age 33))
      (b "I am not a person"))
  (list
   (person-name a)
   (person-age a)
   (person? a)
   (person? b)))

