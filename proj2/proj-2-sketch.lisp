(defun make-name (&rest strings)
  (intern (string-upcase (apply #'concatenate 'string strings))))

(defun delegate (classes query arg)
  (dolist (class classes)
    (let ((res (funcall class query arg)))
      (when res (return res)))))

(defun make-constructor (class-name super-classes slots)
  `(defun ,(make-name "MAKE-" (string class-name)) (&key ,@slots)
     (lambda (query &optional arg)
       (cond
         ((eql query 'IS-INSTANCE-OF)
          (eql arg ',(make-name "CLASS-" (string class-name))))

         ((eql query 'GET-SLOT-VALUE)
          (let ((key-value (assoc arg ,(make-slots-alist slots))))
            (when (car key-value) (cadr key-value))))

         (t (delegate ,super-classes query arg))))))

(defun make-getter (class-name)
  #'(lambda (x)
      `(defun ,(make-name (string class-name) "-" (string x)) (obj)
         (funcall obj 'GET-SLOT-VALUE ',(make-name "SLOT-" (string x))))))

(defun make-recognizer (class-name)
  `(defun ,(make-name (string class-name) "?") (obj)
     (typecase obj
       (function (funcall obj 'IS-INSTANCE-OF ',(make-name "CLASS-" (string class-name))))
       (t nil))))

(defun make-slots-alist (slots)
  (cons 'list
        (mapcar
         #'(lambda (slot)
             `(list ',(make-name "SLOT-" (string slot)) ,slot))
         slots)))

(defmacro def-class (classes &rest slots)
  (let* ((class-name    (if (listp classes) (car classes) classes))
         (super-classes (when (listp classes) (cdr classes)))
         (all-slots (append slots ()))) ;(get-all-slots super-classes))))
    `(progn
       ,(make-constructor class-name super-classes all-slots) ; Constructor
       ,@(mapcar (make-getter class-name) all-slots)          ; Getters
       ,(make-recognizer class-name))))                       ; Recognizer

;;------------------------------------------------------------------------------

(def-class person name age) 

(let ((a (make-person :name "Paulo" :age 33))
      (b "I am not a person"))
  (list
   (person-name a)
   (person-age a)
   (person? a)
   (person? b)))

;;------------------------------------------------------------------------------

;; We could use this instead to implement the recognizer.
(gensym)

;;------------------------------------------------------------------------------

(defmacro make-assertions (&body assertions)
  `(progn ,@(mapcar #'(lambda (x) `(assert ,x))
                    assertions)))

(def-class person
  name
  age)

(def-class researcher
  group)

(def-class (student person)
  course)

(def-class sportsman
  activity
  schedule)

(def-class (ist-student student sportsman))

(def-class (phd-student ist-student researcher)
  thesis)

(let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
  (make-assertions
    (equal (person-name s) "Paul")
    (equal (student-course s) "Informatics")))

(let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
  (make-assertions
    (ist-student? m)
    (student? m)
    (sportsman? m)
    (equal (ist-student-name m) "Maria")
    (equal (person-name m) "Maria")
    (equal (sportsman-activity m) "Tennis")
    (equal (ist-student-activity m) "Tennis")))

(let ((b (make-phd-student :name "Brian" :age 28 :course "Informatics" :activity "Soccer" :group "ESW" :thesis "Code Migration")))
  (make-assertions
    (researcher? b)
    (person? b)
    (student? b)
    (sportsman? b)
    (phd-student? b)
    (equal (phd-student-thesis b) "Code Migration")
    (equal (student-name b) "Brian")
    (equal (phd-student-group b) "ESW")
    (equal (phd-student-name b) "Brian")))
