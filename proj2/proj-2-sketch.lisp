(defun make-recognizer (class-name)
  `(defun ,(intern (concatenate 'string
                                (string class-name)
                                "?")) (obj)
     (typecase obj
       (function (funcall obj ',(intern (concatenate 'string
                                                     "CLASS-"
                                                     (string class-name)))))
       (t nil)))) 

(defun make-getter (class-name)
  #'(lambda (x)
      `(defun ,(intern (concatenate 'string
                                    (string class-name)
                                    "-"
                                    (string x))) (obj)
         (funcall obj ',(intern (concatenate 'string
                                             "SLOT-"
                                             (string x))))))) 
(defmacro def-class (classes &rest slots)
  (let ((class-name (if (listp classes)
                        (car classes)
                        classes))
        (super-classes (if (listp classes)
                           (cdr classes)
                           nil)))
  `(progn

     ;; Constructor
     (defun ,(intern (concatenate 'string
                                  "MAKE-"
                                  (string class-name))) (&key ,@slots)
       (lambda (arg)
         (let ((superclass ,super-classes)
               (slots
                 ,(cons 'list
                       (mapcar
                        #'(lambda (slot)
                            `(list ',(intern (concatenate 'string
                                                          "SLOT-"
                                                          (string slot)))
                                   ,slot))
                        slots))))
           (if (eql arg ',(intern (concatenate 'string
                                               "CLASS-"
                                               (string class-name))))
               t
              (let ((key-value (assoc arg slots)))
                  (if (car key-value)
                      (cadr key-value)
                      (dolist (class ,super-classes)
                        (let ((res (funcall class arg)))
                          (when res (return res))))))))))

     ;; Getters
     ,@(mapcar (make-getter class-name) slots)

     ;; Recognizer
     ,(make-recognizer class-name)))) 

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
