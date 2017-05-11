(defun make-name (&rest strings)
  (intern (apply #'concatenate 'string strings))) 

(defun make-recognizer (class-name)
  `(defun ,(make-name (string class-name) "?") (obj)
     (typecase obj
       (function (funcall obj 'RECOGNIZE ',(make-name "CLASS-" (string class-name))))
       (t nil)))) 

(defun make-getter (class-name)
  #'(lambda (x)
      `(defun ,(make-name (string class-name) "-" (string x)) (obj)
         (funcall obj 'GET-SLOT ',(make-name "SLOT-" (string x)))))) 

(defun make-slots-alist (slots)
  (cons 'list
         (mapcar
          #'(lambda (slot)
              `(list ',(make-name "SLOT-" (string slot)) ,slot))
          slots))) 

(defun delegate (classes query arg)
  (dolist (class classes)
    (let ((res (funcall class query arg)))
      (when res (return res))))) 

(defun make-constructor (class-name super-classes slots)
  `(defun ,(make-name "MAKE-" (string class-name)) (&key ,@slots)
     (lambda (query arg)
       (let ((slots ,(make-slots-alist slots)))
         (cond
           ((eql query 'RECOGNIZE) (eql arg ',(intern (concatenate 'string "CLASS-"
                                                                   (string class-name)))))
           ((eql query 'GET-SLOT) (let ((key-value (assoc arg slots)))
                                    (when (car key-value) (cadr key-value))))
            (t (delegate ,super-classes query arg))))))) 

(defmacro def-class (classes &rest slots)
  (let ((class-name    (if (listp classes) (car classes) classes))
        (super-classes (if (listp classes) (cdr classes) nil)))
    `(progn
       ,(make-constructor class-name super-classes slots) ; Constructor
       ,@(mapcar (make-getter class-name) slots)          ; Getters
       ,(make-recognizer class-name))))                   ; Recognizer

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
