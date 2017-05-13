(defvar *classes* (make-hash-table))

(defun make-name (&rest strings)
  (intern (string-upcase (apply #'concatenate
                                'string
                                (mapcar #'string
                                        strings)))))

(defun make-constructor-name (constructor-name)
  (make-name "make-" (string constructor-name)))

(defun make-getter-name (class-name slot-name)
  (make-name (string class-name) "-" (string slot-name)))

(defun make-recognizer-name (class-name)
  (make-name (string class-name) "?"))

;;;-----------------------------------------------------------------------------
;;; Reflection

(defun set-slots! (class slots)
  (setf (gethash 'SLOTS (gethash class *classes*)) slots))

(defun set-class! (class)
  (progn
    (setf (gethash class *classes*) (make-hash-table))
    (setf (gethash 'CLASS
                   (gethash class *classes*)) class)))

(defun set-super-classes! (class super-classes)
  (setf (gethash 'SUPER-CLASSES
                 (gethash class *classes*)) super-classes))

(defun get-slots (class)
  (gethash 'SLOTS (gethash class *classes*)))

(defun get-super-classes-slots (class)
  (mapcan #'get-slots
          (gethash 'SUPER-CLASSES
                   (gethash class *classes*))))

(defun get-all-slots (class)
  (append (get-slots class)
          (get-super-classes-slots class)))

;;;-----------------------------------------------------------------------------

(defun make-slot-pair (unbound-slot)
  `(list ',unbound-slot ,unbound-slot))

(defun make-slots-alist (unbound-slots)
  (cons 'list (mapcar #'make-slot-pair unbound-slots)))

(defun make-slots (unbound-slots)
  `(let ((slots-ht (make-hash-table)))
     (loop for (key . value) in ,(make-slots-alist unbound-slots) do
       (setf (gethash key slots-ht) (car value)))
     slots-ht))

(defun make-constructor (unbound-class)
  (let ((hierarchy-unbound-slots (get-all-slots unbound-class)))
    `(defun ,(make-constructor-name unbound-class) (&key ,@hierarchy-unbound-slots)
       (let ((self (make-hash-table)))
         (progn
           (setf (gethash 'CLASS self) ',unbound-class)
           (setf (gethash 'SLOTS self)
                 ,(make-slots hierarchy-unbound-slots))
           self)))))

(defun make-getter (unbound-class)
  #'(lambda (unbound-slot)
      `(defun ,(make-getter-name unbound-class unbound-slot) (self)
         (gethash ',unbound-slot (gethash 'slots self)))))

(defun make-recognizer (unbound-class)
  `(defun ,(make-recognizer-name unbound-class) (obj)
     (when (hash-table-p obj)
       (eql (gethash 'CLASS obj) ',unbound-class))))

(defun make-metaclass! (unbound-class unbound-super-classes unbound-slots)
  `(let ((class         ',(make-name unbound-class))
         (super-classes ',(mapcar #'make-name unbound-super-classes))
         (slots         ',(mapcar #'make-name unbound-slots)))
     (progn
       (set-class! class)
       (set-super-classes! class super-classes)
       (set-slots! class slots))))

(defmacro def-class (unbound-classes &rest unbound-slots)
  (let* ((unbound-class         (if (listp unbound-classes)
                                    (car unbound-classes)
                                    unbound-classes))
         (unbound-super-classes (if (listp unbound-classes)
                                      (cdr unbound-classes)
                                      nil)))
    `(progn
       ,(make-metaclass! unbound-class unbound-super-classes unbound-slots)
       ,(make-constructor unbound-class)
       ,@(mapcar (make-getter unbound-class) unbound-slots)
       ,(make-recognizer unbound-class))))

(def-class person
  name
  age) 

(loop for key being the hash-keys of *classes* collect key) 

(let ((a (make-person :name "Paulo" :age 33))
      (b "I am not a person"))
  (and
    (equal (person-name a) "Paulo")
    (equal (person-age a) 33)
    (equal (person? a) t)
    (equal (person? b) nil)))
