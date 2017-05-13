(defvar *metaclasses* (make-hash-table))

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

(defun get-slots-from-class (class)
  (when (gethash class *metaclasses*)
    (funcall (gethash class *metaclasses*)
             :msg 'CLASS-SLOTS)))

(defun get-slots-from-class-hierarchy (hierarchy)
  (mapcan #'get-slots-from-class hierarchy))

(defun slot-name-to-slot (slot-name)
  (intern (subseq (string slot-name) 5)))

(defun make-slot-pair (slot-name)
  `(list ',slot-name ,(slot-name-to-slot slot-name)))

(defun make-slots-alist (slots-names)
  (cons 'list (mapcar #'make-slot-pair slots-names)))

(defun make-slots (hierarchy)
  `(let ((slots-alist ,(make-slots-alist (get-slots-from-class-hierarchy hierarchy)))
        (slots-ht (make-hash-table)))
    (progn
      (dolist (key-value slots-alist)
        (setf (gethash (car key-value) slots-ht)
              (cadr key-value)))
      slots-ht)))

(defun make-slot-keys (hierarchy)
  (mapcar #'slot-name-to-slot (get-slots-from-class-hierarchy hierarchy)))

(defun recognize (hierarchy class-name-to-recognize)
  (or (mapcar #'(lambda (class-name)
                  (eql class-name-to-recognize
                       (funcall (gethash class-name *metaclasses*)
                                :msg 'CLASS-NAME)))
              hierarchy)))

;;;-----------------------------------------------------------------------------

(defun make-constructor (unbound-class unbound-hierarchy)
  `(defun ,(make-constructor-name unbound-class)
       (&key ,@(make-slot-keys (mapcar #'make-class-name unbound-hierarchy)))
     (lambda (msg param)
       (let ((slots ,(make-slots (mapcar #'make-class-name unbound-hierarchy))))
         (cond
           ((eql msg 'RECOGNIZE) (recognize ',(mapcar #'make-class-name unbound-hierarchy) param))
           ;; This still doesn't differentiate whether the slots are undefined or nil
           ((and (eql msg 'GET-SLOT-VALUE)
                 (nth-value 1 (gethash param slots))) (gethash param slots)))))))

;; Maybe the getter should only work if the object is recognized.
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

(defun def-metaclass (class-name slots)
  (setf (gethash class-name *metaclasses*)
        (lambda (&key (msg 'CLASS-NAME))
          (cond
            ((eql msg
                  'CLASS-NAME) class-name)
            ((eql msg
                  'CLASS-SLOTS) slots)))))

(defmacro def-class (classes &rest slots)
  (let* ((all-classes (if (listp classes) classes (list classes)))
         (class (car all-classes)))
    `(progn
       (def-metaclass
           ',(make-class-name class)
           ',(mapcar #'make-slot-name slots))

       ,(make-constructor class all-classes)

       ,@(mapcar (make-getter class) slots)

       ,(make-recognizer class))))

;;;-----------------------------------------------------------------------------

(def-class person
  name
  age) 

(def-class (student person)
  course) 

;; (funcall (gethash 'CLASS-PERSON *metaclasses*) :msg 'CLASS-SLOTS) 


(setf p (make-person :name "Rodrigo" :age 22))

(person-name p) 
(person-age p) 

(let ((a (make-person :name "Paulo" :age 33))
      (b "I am not a person"))
  (list
   (person-name a)
   (person-age a)
   (person? a)
   (person? b)))

(defun test (&rest keys &key name age)
  (print name)
  (print age))

