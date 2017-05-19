;;;-----------------------------------------------------------------------------
;;; The class system implementation is structured as follows: classes are
;;; organized in a hash-table, *classes* indexed by class name. A class name is
;;; represented by a symbol. For example, (def-class A) creates a class with
;;; name 'A.
;;;
;;; Each class is itself represented as an hash-table. Each class stores its own
;;; name (with key 'CLASS), its direct super-classes (with key 'SUPER-CLASSES)
;;; and its slots (with key 'SLOTS).
(defvar *classes* (make-hash-table))

;;;-----------------------------------------------------------------------------
;;; Utility tools for the creation of names for constructors and methods.

(defun make-name (&rest strings)
  (intern (string-upcase (apply #'concatenate
                                'string
                                (mapcar #'string strings)))))

(defun make-constructor-name (constructor-name)
  (make-name "make-" (string constructor-name)))

(defun make-getter-name (class-name slot-name)
  (make-name (string class-name) "-" (string slot-name)))

(defun make-recognizer-name (class-name)
  (make-name (string class-name) "?"))

;;;-----------------------------------------------------------------------------
;;; Reflection
;;;
;;; Here we have methods that manipulate or query the *classes* hash-table.

(defun set-slots! (class slots)
  "Sets the slots of CLASS, an hash-table (a class, not an instance!) to SLOTS,
a list of symbols."
  (setf (gethash 'SLOTS (gethash class *classes*)) slots))

(defun set-class! (class)
  "Inserts a new class with name CLASS (a symbol) in *classes*."
  (progn
    (setf (gethash class *classes*) (make-hash-table))
    (setf (gethash 'CLASS
                   (gethash class *classes*)) class)))

(defun set-super-classes! (class super-classes)
  "Sets the super-classes of the class with name CLASS (a symbol) to
SUPER-CLASSES (a list of symbols)."
  (setf (gethash 'SUPER-CLASSES
                 (gethash class *classes*))
        super-classes))

(defun get-class (class)
  "Returns the class (an hash-table) of the class with name CLASS-NAME (a
symbol)."
  (gethash class *classes*))

(defun get-class-name (class)
  "Returns the name (a symbol) of the class CLASS (an hash-table)."
  (gethash 'CLASS class))

(defun get-direct-super-classes (class)
  "Returns the names of the first-level super-classes (a list of symbols) of
the class with name CLASS (a symbol)."
  (gethash 'SUPER-CLASSES (get-class class)))

(defun get-class-hierarchy-with-duplicates (class-name)
  "Same as get-class-hierarchy but possibly with duplicates."
  (let ((direct-super-classes (get-direct-super-classes class-name)))
    (append direct-super-classes
            (if (null direct-super-classes)
                nil
                (apply #'append (mapcar #'get-class-hierarchy-with-duplicates
                                        direct-super-classes))))))

(defun get-class-hierarchy (class)
  "Returns the names of all the super-classes (a list of symbols) of the class
with name CLASS (a symbol)."
  (remove-duplicates (get-class-hierarchy-with-duplicates class)))

(defun get-slots (class)
  "Returns the names of the slots (a list of symbols) of the class with name
CLASS (a symbol)."
  (gethash 'SLOTS (gethash class *classes*)))

(defun get-super-classes-slots (class)
  "Returns the names of the slots (a list of symbols) of all the super-classes
of the class with name CLASS (a symbol)."
  (apply #'append (mapcar #'get-slots (get-class-hierarchy class))))

(defun get-all-slots (class)
  "Returns the names of the slots (a list of symbols) of all the classes in the
hierarchy of the class with name CLASS (a symbol), i.e., the slots of CLASS plus
the slots of its super-classes."
  (append (get-slots class)
          (get-super-classes-slots class)))

;;;-----------------------------------------------------------------------------
;;; Helper functions for the class definition macro

(defun make-slot-pair (unbound-slot)
  `(list ',unbound-slot ,unbound-slot))

(defun make-slots-alist (unbound-slots)
  "As make-slots, but for an alist instead of an hash-table."
  (cons 'list (mapcar #'make-slot-pair unbound-slots)))

(defun make-slots (unbound-slots)
  "Generates code for the creation of the slots data structure (an hash-table)
of an instance of a class. See make-constructor."
  `(let ((slots-ht (make-hash-table)))
     (loop for (key . value) in ,(make-slots-alist unbound-slots) do
       (setf (gethash key slots-ht) (car value)))
     slots-ht))

(defun make-constructor (unbound-class unbound-slots)
  "Helper function to create the class constructor. A class instance is
represented by an hash-table that stores its class name and slots."
  `(defun ,(make-constructor-name unbound-class) (&key ,@unbound-slots)
     (let ((self (make-hash-table)))
       (progn
         (setf (gethash 'CLASS self) ',unbound-class)
         (setf (gethash 'SLOTS self)
               ,(make-slots unbound-slots))
         self))))

(defun make-getter (unbound-class)
  "Helper function to create the getters. The generated method simply queries
the instance's inner slots."
  #'(lambda (unbound-slot)
      `(defun ,(make-getter-name unbound-class unbound-slot) (self)
         (gethash ',unbound-slot (gethash 'slots self)))))

(defun make-recognizer (unbound-class)
  "Helper function to create the recognizers. The generated methods start by
checking if the argument is of the correct type. Returns true if yes, and
returns if any of its super-classes has the correct type, if not."
  `(defun ,(make-recognizer-name unbound-class) (obj)
     (when (hash-table-p obj)
       (let ((class-name (get-class-name obj)))
         (if (eql class-name ',unbound-class)
             t
             (loop for super-class-name in (get-class-hierarchy class-name)
                   thereis (eql super-class-name ',unbound-class)))))))

(defun make-metaclass! (unbound-class unbound-super-classes unbound-slots)
  "Defines all the necessary information about a class and stores it in
*classes*. That information can be later retrieved by the \"reflection\" functions
described up in the file. It can be seen as a trivial implementation of
metaclasses."
  (let* ((class         (make-name unbound-class))
         (super-classes (mapcar #'make-name unbound-super-classes))
         (slots         (mapcar #'make-name unbound-slots)))
    (progn
      (set-class! class)
      (set-super-classes! class super-classes)
      (set-slots! class slots))))

;;;-----------------------------------------------------------------------------
;;; Class definition

(defmacro def-class (unbound-classes &rest unbound-slots)
  "Class definition. If UNBOUND-CLASSES is a list then it is a definition with
inheritance; otherwise it's just a simple class. UNBOUND-SLOTS are the slots to
be of the class."
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
