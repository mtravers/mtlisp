(in-package :mt)

;;; These have to come early so they can be used at readtime.

;;; Documentation

;;; From BioBike (with mods)
(defmacro one-string (&rest string-designators)
  "Creates a single string (if its arguments are all constant strings)
   or a form that creates a single string (if some arguments are variables)"
  (flet ((string-designator-p (x)
           (or (stringp x) (characterp x)))
         (to-string (x)
           (cond
            ((stringp x) x)
            ((characterp x) (string x)))))
    (cond
     ((every 'stringp string-designators)
      (apply 'concatenate 'string string-designators))
     ((every #'string-designator-p string-designators)
      (apply 'concatenate 
        'string (mapcar #'to-string string-designators)))
     (t
      (let ((reversed-form (reverse '(concatenate 'string)))
            (merged-constant-strings ""))
        (dolist (x string-designators)
          (if (not (string-designator-p x))
              (progn
                (when (> (length merged-constant-strings) 0)
                  (push merged-constant-strings reversed-form)
                  (setq merged-constant-strings ""))
                (push x reversed-form))
            (setq merged-constant-strings
                  (concatenate 'string 
                    merged-constant-strings (to-string x)))
            ))
        (when (> (length merged-constant-strings) 0)
          (push merged-constant-strings reversed-form))
        (reverse reversed-form)
        )))))

(defmacro doc (&rest string-designators)
  "Inserts newlines after every argument except the last, and calls ONE-STRING"
  (let ((nl (string #\Newline)))
    `(one-string
      ,@(loop for strings on string-designators 
              as s = (first strings)
              nconc 
             (if (cdr strings) (list s nl) (list s))))))
