;;; FIXME:
;;; 1. We should make clearer in the code when we're manipulating class names
;;;    (of type symbol) and classes (of type hash-table).
;;; 2. What do we do if we apply a method that is not defined for a certain
;;;    class?

;;;-----------------------------------------------------------------------------

(defvar *classes* (make-hash-table))

;;;-----------------------------------------------------------------------------

(defun print-hash-entry (key value)
  (format t "(~S . ~S)~%" key value))

(defun print-hash-table (hash-table)
  (maphash #'print-hash-entry hash-table))

(defun print-classes ()
  (print-hash-table *classes*))

(defun print-everything ()
  (print-classes)

  (maphash (lambda (key value)
             (format t "~%")
             (print-hash-table value))
           *classes*))

;;;-----------------------------------------------------------------------------

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

(defun set-super-classes! (class direct-super-classes)
  (setf (gethash 'SUPER-CLASSES
                 (gethash class *classes*))
        direct-super-classes))

(defun get-class (class-name)
  (gethash class-name *classes*))

(defun get-class-name (class)
  (gethash 'CLASS class))

(defun get-direct-super-classes (class-name)
  (gethash 'SUPER-CLASSES (get-class class-name)))

(defun get-class-hierarchy-with-duplicates (class-name)
  (let ((direct-super-classes (get-direct-super-classes class-name)))
    (append direct-super-classes
            (if (null direct-super-classes)
                nil
                (apply #'append (mapcar #'get-class-hierarchy-with-duplicates
                                        direct-super-classes))))))

(defun get-class-hierarchy (class-name)
  (remove-duplicates (get-class-hierarchy-with-duplicates class-name)))

(defun get-slots (class)
  (gethash 'SLOTS (gethash class *classes*))
  )

(defun get-super-classes-slots (class)
  (apply #'append (mapcar #'get-slots (get-class-hierarchy class))))

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

(defun make-constructor (unbound-class unbound-slots)
    `(defun ,(make-constructor-name unbound-class) (&key ,@unbound-slots)
       (let ((self (make-hash-table)))
         (progn
           (setf (gethash 'CLASS self) ',unbound-class)
           (setf (gethash 'SLOTS self)
                 ,(make-slots unbound-slots))
           self))))

(defun make-getter (unbound-class)
  #'(lambda (unbound-slot)
      `(defun ,(make-getter-name unbound-class unbound-slot) (self)
         (gethash ',unbound-slot (gethash 'slots self)))))

(defun make-recognizer (unbound-class)
  `(defun ,(make-recognizer-name unbound-class) (obj)
     (when (hash-table-p obj)
       (let ((class-name (get-class-name obj)))
         (if (eql class-name ',unbound-class)
             t
             (loop for super-class-name in (get-class-hierarchy class-name)
                   thereis (eql super-class-name ',unbound-class)))))))

(defun make-metaclass! (unbound-class unbound-super-classes unbound-slots)
  (let* ((class         (make-name unbound-class))
         (super-classes (mapcar #'make-name unbound-super-classes))
         (slots         (mapcar #'make-name unbound-slots)))
    (progn
      (set-class! class)
      (set-super-classes! class super-classes)
      (set-slots! class slots))))

(defmacro def-class (unbound-classes &rest unbound-slots)
  (let* ((unbound-class         (if (listp unbound-classes)
                                    (car unbound-classes)
                                    unbound-classes))
         (unbound-slots         (if (listp unbound-classes)
                                    (append unbound-slots
                                            (apply #'append
                                                   (mapcar #'get-slots
                                                           (cdr unbound-classes))))
                                    unbound-slots))
         (unbound-super-classes (if (listp unbound-classes)
                                      (cdr unbound-classes)
                                      nil)))
    (make-metaclass! unbound-class unbound-super-classes unbound-slots)
    `(progn
       ,(make-constructor unbound-class unbound-slots)
       ,@(mapcar (make-getter unbound-class) unbound-slots)
       ,(make-recognizer unbound-class))))
