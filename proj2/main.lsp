(defmacro def-class (name &rest slots)
    `(progn
        (defun ,(intern (concatenate 'string "MAKE-" (string name))) (&key ,@slots)
            (vector ,@slots))
		
		(setq counter 0)	
		(loop for x in ',slots
			do (defun ,(intern (concatenate 'string (string name) "-" (string-upcase 'x))) (,name)
									(aref ,name ,counter))
								(setq counter (1+ counter)))))