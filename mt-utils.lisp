(in-package :mt)

#| ######################################################################

 Random Lisp utilities

Copyright © 1994-2010 Michael Travers
Permission is given to use and modify this code as long
as the copyright notice is preserved.

Send questions, comments, and fixes to mt@alum.mit.edu.

-------------------------------------------------------------------------

This file contains a large miscellany of useful hacks that I have come
to depend on.  

Originally written for MCL; this library now has been used with SBCL,
ACL, and ABCL as well.

Many items here are borrowed from others. In most cases the source
is acknowledged.

NOTE: This is the canonical version!  Accept no substitutes.

###################################################################### |#


;;; Generalized variables, lists, binding, etc.

(defmacro deletef (thing place &rest delete-args)
  `(setf ,place (delete ,thing ,place ,@delete-args)))

(defmacro removef (thing place &rest remove-args)
  `(setf ,place (remove ,thing ,place ,@remove-args)))

;;; Like push, but will remove an existing element if its already there. A typical use 
;;; is managing lists of tagged structures. KEY is applied to THING as well as to the elements
;;; of PLACE, and in fact this operation only makes sense with a KEY arg.
;;; should have once-only +++
(defmacro replacef (thing place &rest delete-args &key (key #'identity) &allow-other-keys)
  `(progn
     (deletef (funcall ,key ,thing) ,place ,@delete-args)
     (push ,thing ,place)))

(defmacro push-end (thing place)
  `(setf ,place
         (nconc ,place
                (list ,thing))))

(defmacro pop-end (place)
  `(let* ((last (last ,place 2))
	  (v (cadr last)))
     (if (null (cdr last))
	 (pop ,place)
	 (progn
	   (rplacd last nil)
	   v))))

(defmacro pushnew-end (item place &key (test #'eql))
  `(if (member ,item ,place :test ,test)
     ,place
     (push-end ,item ,place)))

(defun list-insert (list item n)
  (let ((cdr (nthcdr (1- n) list)))
    (rplacd cdr (cons item (cdr cdr)))
    list))

; destructive
(defun insert-before (list new before)
  (if (null before)
      (nconc list (list new))
      (do ((rest list (cdr rest))
	   (last nil rest))
	  ((null rest)
	   (error "insert-before: ~A not in ~A" before list))
	(if (eq before (car rest))
	    (if last
		(progn (rplacd last (cons new rest))
		       (return list))
		(return (cons new list)))))))

(defun firstn (list n)
  (if (>= n (length list))
    list
    (subseq list 0 n)))

;;; Could be even safer I suppose
(defun subseq-safe (list from &optional to)
  (subseq list from (and to (min to (length list)))))

(defun break-list (l n)
  "Break LIST into sublists of length N"
  (if l
      (cons (firstn l n)
	    (break-list (nthcdr n l) n))))

;;; If the lists are not sets (have duplicated items) result is undefined
(defun set-equal (l1 l2 &optional (test #'eq))
  (and (= (length l1) (length l2))
       (dolist (elt l1 t)
         (unless (find elt l2 :test test)
           (return nil)))))

(defun stable-nset-difference (list1 list2 &key (test #'eql))
  #.(doc
     "Like NSET-DIFFERENCE, but preserves the order of the LIST1 argument")
  (do ((rest list1)
       (head nil)
       (tail nil))
      ((endp rest) head)
    (unless (member (car rest) list2 :test test)
      (if (null head)
	  (setf head rest
		tail rest)
	  (progn
	    (rplacd tail rest)
	    (setf tail rest))))
    (pop rest)))

;;; for descending down alist type structures, esp. those in JSON like form
(defun findprop (prop structure)
  (cadr (member prop structure :test #'equal)))

;;; recursive version of above
(defun findprops (structure &rest props)
  (if (null props) 
      structure
      (apply #'findprops (findprop structure (car props)) (cdr props))))

;;; From BioLisp
(defmacro assocadr (key alist &rest assoc-keywords)
  "Shorthand for (CADR (ASSOC ...))"
  `(cadr (assoc ,key ,alist ,@assoc-keywords)))

(defmacro assocdr (key alist &rest assoc-keywords)
  "Shorthand for (CDR (ASSOC ...))"
  `(cdr (assoc ,key ,alist ,@assoc-keywords)))

;;; mv-let*: lets the car of a let binding form be a list
;;; elements of which get bound to multiple values.

(defmacro mv-let* (forms &body body)
  (cond ((null forms)
	 `(progn ,@body))
	((or (symbolp (car forms))
	     (symbolp (caar forms)))

	 `(let (,(car forms))
	    (mv-let* ,(cdr forms)
	      ,@body)))
	(t
	 `(multiple-value-bind ,(caar forms) ,(cadar forms)
	    (mv-let* ,(cdr forms)
	      ,@body)))))

;;; Lifted from PCL.  Ensure that a macro variable is only expanded once.
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
                     (numberp ,var)
                     (and (listp ,var)
			  (member (car ,var) '(quote function))))
                 ,var
                 (let ((,gensym-var (gensym)))
                   (push ,gensym-var ,run-time-vars)
                   (push ,var ,run-time-vals)
                   ,gensym-var))
            expand-time-val-forms))    
    `(let* (,run-time-vars
            ,run-time-vals
            (wrapped-body
              ((lambda ,vars . ,body) . ,(reverse expand-time-val-forms))))
       `((lambda ,(nreverse ,run-time-vars)  ,wrapped-body)
         . ,(nreverse ,run-time-vals)))))

#-CCL
(defmacro let-globally (clauses &body body)
  (let ((temp-vars (mapcar #'(lambda (s) (gensym (symbol-name (car s)))) clauses)))
    `(let ,(mapcar #'(lambda (temp clause) `(,temp ,(car clause))) temp-vars clauses)
       (unwind-protect 
         (progn
           ,@(mapcar #'(lambda (clause)
                         (cons 'setf clause))
                     clauses)
           ,@body)
         ,@(mapcar #'(lambda (temp clause)
                       `(setf ,(car clause) ,temp))
                   temp-vars clauses)))))

;;; destructuring-let

;;; Random small aids to expression

(defmacro non-nil (var)
  `(and (boundp ',var)
	,var))

#-:SBCL
(declaim (ignore ignore))               ; So sue me

(defmacro return-if (val)
  (once-only (val)
    `(if ,val (return ,val))))

(defmacro return-from-if (block val)
  (once-only (val)
    `(if ,val (return-from ,block ,val))))

#-CCL ; CCL has this built in
(defmacro neq (a b)
  `(not (eq ,a ,b)))

(defun circular-list (&rest elements)
  (rplacd (last elements) elements))

(defun fringe (list)
  (cond ((null list) nil)
	((listp list)
	 (append (fringe (car list)) (fringe (cdr list))))
	(t (list list))))
;;; Iteration and Mapping

;;; Like dolist, but works with any sequence
(defmacro dosequence ((var sequence &optional result) &body body)
  (let ((index (gensym))
	(len (gensym)))
    `(do ((,index 0 (1+ ,index))
	  (,len (length ,sequence))
	  ,var)
      ((= ,index ,len)
       ,result)
      (setq ,var (elt ,sequence ,index))
      ,@body)))

(defun mapsequence (proc sequence)
  (collecting
    (dosequence (v sequence)
      (collect (funcall proc v)))))

(defmacro do-for-array-elements (array vars &body body)
  `(let ((array-dimensions (array-dimensions ,array)))
     (do-for-array-elements-1 ,array ,vars 0 ,@body)))

(defmacro do-for-array-elements-1 (array vars dim &body body)
  (if vars
      `(dotimes (,(car vars) (nth ,dim array-dimensions))
	 (do-for-array-elements-1 ,array ,(cdr vars) ,(1+ dim)
	   ,@body))
      `(progn ,@body)))

;;; do-collect
(defmacro collecting (&body body)
  `(let ((%results nil))
     (flet ((collect (thing) (push thing %results))
	    (collect-if (thing) (when thing (push thing %results)))
	    (collect-new (thing &optional (test #'eql)) (pushnew thing %results :test test)))
       (declare (ignorable (function collect) (function collect-if) (function collect-new)))
       ,@body)
     (nreverse %results)))

(defmacro summing (&body body)
  `(let ((%result 0))
     (flet ((sum (thing) (incf %result thing)))
       ,@body)
     %result))

(defmacro accumulating (init func &body body)
  `(let ((%result ,init))
     (flet ((accumulate (thing)
	      (setf %result (funcall ,func %result thing))))
       ,@body)
     %result))

;;; generalized good iterator.

;;; Maximum/minimums

;;; +++ flush return-max, use multiple values instead
(defun extreme (list test &key (key #'identity) (return-max nil))
  (and list
       (let* ((best (car list))
              (max (funcall key best)))
         (dolist (other (cdr list) (if return-max max best))
           (let ((score (funcall key other)))
	     (when (funcall test score max)
               (setq best other max score)))))))

; +++ key arguments are slow
(defun extremes (list test &key (key #'identity))
  (if list
    (let* ((best (list (car list)))
           (max (funcall key (car best))))
      (dolist (other (cdr list) (values best max))
        (let ((score (funcall key other)))
          (if (funcall test score max)
            (setq best (list other) max score)
            (if (funcall test max score)
              nil
              (push other best))))))
    (values nil most-negative-fixnum)))

(defun maximize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'> :key key :return-max return-max))

(defun minimize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'< :key key :return-max return-max))

(defun maximums (list &key (key #'identity))
  (declare (inline extremes))            ; not that this does anything
  (extremes list #'> :key key))

(defun minimums (list &key (key #'identity))
  (declare (inline extremes))            ; not that this does anything
  (extremes list #'< :key key))

(defun closest (value list key)
  (minimize list :key #'(lambda (elt) (abs (- value (funcall key elt))))))

;;; Various mapping functions
;;; Most of these borrowed from Ken Haase

#-ABCL  ;;; has a collect fn built in, need to change name
(defun collect (fcn list)
  "Applies FCN to each element of LIST returning all the non-nil values as a list."
  (let* ((head (list 'HEAD)) 
         (tail head))
    (dolist (elt list (cdr head))
      (let ((value (funcall fcn elt)))
	(when value
	  (push value (cdr tail))
	  (setf tail (cdr tail)))))))

(defun mapappend (fcn list)
  "Applies FCN to every element of LIST, appending the results together.
Order is maintained as one might expect."
  (let* ((head (list '())) (tail head))
    (dolist (elt list (cdr head))
      (dolist (result-elt (funcall fcn elt))
	(setf (cdr tail) (list result-elt))
	(setf tail (cdr tail))))))

(defun mapunion (fcn list &key (test #'eql) (key #'identity))
  "Applies FCN to every element of LIST, unioning the results together.
Except for removal of EQL occurences, order is maintained as one might expect."
  (let* ((head (list '())) (tail head))
    (dolist (elt list (cdr head))
      (dolist (result-elt (funcall fcn elt))
        (unless (member result-elt head :test test :key key)
          (setf (cdr tail) (list result-elt))
          (setf tail (cdr tail)))))))

(defun union* (lists &key (test #'eql) (key #'identity))
  "UNION together an arbitrary number of lists (passed in a containing list)"
  (case (length lists)
    (0 nil)
    (1 (car lists))
    (t (union* (cons (union (car lists) (cadr lists) :test test :key key)
                     (cddr lists))
	       :test test :key key))))

(defun intersection* (lists &key (test #'eql) (key #'identity))
  "INTERSECTION together an arbitrary number of lists (passed in a containing list)"
  (case (length lists)
    (0 nil)
    (1 (car lists))
    (t (intersection* (cons (intersection (car lists) (cadr lists) :test test :key key)
                     (cddr lists))
	       :test test :key key))))

(defun maptree (fcn tree)
  (if (listp tree)
      (mapcar #'(lambda (elt) (maptree fcn elt)) tree)
    (funcall fcn tree)))

;;; works on structure with dotted lists
(defun maptree-dots (fcn tree)
  (cond ((null tree) nil)
	((listp tree)
	 (cons (maptree-dots fcn (car Tree))
	       (maptree-dots fcn (cdr Tree))))
	(t (funcall fcn tree))))

(defun mapsum (fcn list)
  (let ((result 0))
    (dolist (elt list result)
      (incf result (funcall fcn elt)))))

(defun mapcross (fcn list1 list2)
  "Applies FCN to every combination of elements from LIST1 and LIST2,
returning the list of results.  Order is maintained as one might expect."
  (let* ((head (list '())) (tail head))
    (dolist (e1 list1 (cdr head))
      (dolist (e2 list2)
	(push (funcall fcn e1 e2) (cdr tail))
	(setf tail (cdr tail))))))

(defun mapsubsets (proc set)
  "Map PROC over ever subset of SET"
  (if (null set)
      (funcall proc nil)
      (mapsubsets (cdr set) 
		  #'(lambda (ss)
		      (funcall proc ss)
		      (funcall proc (cons (car set) ss))))))

(defun split-list (predicate list)
  "Returns two lists extracted from list based on PREDICATE."
  (let ((wheat '()) (chaff '()))
    (dolist (elt list (values wheat chaff))
      (if (funcall predicate elt)
	  (push elt wheat) (push elt chaff)))))



; +++ key args are slow
(defun filter (predicate list &key key &aux wheat)
  "Return only the elements of list meeting PREDICATE"
  (dolist (elt list (nreverse wheat))
    (when (funcall predicate (if key (funcall key elt) elt))
      (push elt wheat))))

(defun filter-out (predicate list &key key &aux wheat)
  "Return only the elements of list not meeting PREDICATE"
  (dolist (elt list (nreverse wheat))
    (unless (funcall predicate (if key (funcall key elt) elt))
      (push elt wheat))))

;;; Not very efficient, see biolisp for better one
(defun flatten (tree)
  (cond ((null tree) nil)
	((listp tree) 
	 (append (flatten (car tree))
		 (flatten (cdr tree))))
	(t (list tree))))


;;; String Utilities

;;; this is the same as the CL function SUBSTITUTE, so let's flush it...
#|
(defun string-replace-char (string char0 char1 &key (start 0) (end nil))
  (do ((from start)
       (new-string (concatenate 'string string)))
      ((null from) new-string)
    (setq from (position char0 string :start (1+ from) :end end))
    (when from
      (setf (char new-string from) char1))))
|#

(defun string+ (&rest args)
  "Concatenate the elements of ARGS."
  (apply #'concatenate 'string args))

;;; stolen from BioLisp
(defun string-join (string-list &optional (sep #\Space))
  "Concatenates strings together and puts SEP between each joined substring"
  (setq sep (string sep))
  (when (null string-list) (return-from string-join ""))
  (let* ((total-length 0)
         (sep-length (length sep))
         (no-sep (zerop sep-length)))
    (dolist (s string-list) (incf total-length (+ (length s) sep-length)))
    (decf total-length sep-length)
    (let ((result-string (make-string total-length))
          (current-pos 0))
      (dolist (s string-list)
        (replace result-string s :start1 current-pos)
        (incf current-pos (length s))
        (unless (or no-sep (>= current-pos total-length))
          (replace result-string sep :start1 current-pos)
          (incf current-pos sep-length)
          ))
      result-string
      )))

(defun parse-substrings (string separator)
  "Return substrings separated by separator character.  "
  (do ((result nil (cons (subseq string finger0 finger1) result))
       (finger0 0 (and finger1 (1+ finger1)))
       (finger1 (position separator string) (and finger1
                                                 (position separator string :start (1+ finger1)))))
      ((null finger0)
       (nreverse result))))

;;; This is probably not efficient
(defun big-concatenate-strings (list)
  (with-output-to-string (out)
    (dolist (s list)
      (write-string s out))))

;;; a much-needed function. this version conses rather more than it should.
(defun string-replace (string find replace &key (start 0) (end nil) (sequence-type 'string) (test #'char-equal))
  #.(doc
     "Replace occurences of FIND in STRING."
     "REPLACE can be a string or a function which takes the matched substring and returns a replacement (can be used to preserve case, ie).")
  (do ((from start)
       (substrings nil)
       (subst-start t))
      ((null subst-start)
       ;; Apply is limited to some relatively small number of args
       (cond ((and (eq sequence-type 'string)
		   (> (length substrings) 200))
	      (big-concatenate-strings (nreverse substrings)))
;	     ((null (cdr substrings))
;	      (car substrings))
	     (t (apply #'concatenate
		       sequence-type
		       (nreverse substrings)))))
    (setq subst-start (search find string :start2 from :end2 end :test test))
    (push (subseq string from subst-start) substrings)
    (when subst-start
      (setf from (+ subst-start (length find)))
      (push
       (if (stringp replace)
           replace
           (funcall replace (subseq string subst-start from)))
       substrings))
    ))

(defun string-upper-case-p (s)
  (every #'upper-case-p s))

(defun string-prefix-equals (string prefix)
  "T if STRING begins with PREFIX."
  (and (>= (length string) (length prefix))
       (string-equal string prefix :end1 (length prefix))))

(defmacro push-string (place add)
  `(setf ,place (concatenate 'string ,place ,add)))

(defun first-line (string)
  (car (parse-substrings string #\Newline)))

(defun fast-string (obj)
  (typecase obj
    (null "")
    (string obj)
    (symbol (symbol-name obj))          ;what about package?
    (t (fast-princ-to-string obj))))

#+CCL
(defvar *fast-princ-to-string-stream* 
  (ccl::%make-string-output-stream (make-array 100
					       :element-type 'character
					       :adjustable t
					       :fill-pointer 0)))

;;; about twice as fast
#+CCL
(defun fast-princ-to-string (obj)
  (princ obj *fast-princ-to-string-stream*)
  (ccl::get-output-stream-string *fast-princ-to-string-stream*))

;;; for other implementations, do the normal thing
#-CCL
(defun fast-princ-to-string (obj)
  (princ-to-string obj))

(defun string-truncate (string length)
  (if (or (null length)
	  (<= (length string) length))
      string
      (format nil "~A..." (subseq string 0 length))))

(defun string-truncate-to-word-boundary (string limit)
  (if (or (null limit)
	  (<= (length string) limit))
      string
      (let ((boundary-pos (position #\Space string :from-end t :end limit)))
	(string+ (subseq string 0 boundary-pos) "..."))))

;;; From BioBike
(defun translate-string (string from to)
  #.(doc
     "Changes the characters in a string from one set to another. "
     "See the documentation for NTRANSLATE-STRING.")
  (ntranslate-string (copy-seq string) from to))

(defun ntranslate-string (string from to)
  #.(doc
     "Destructively changes the characters in a string from one set to "
     "another.  For example: (ntranslate-string \"This\" "
     "\"abcdefghijklmnopqrstuvwxyz\" \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") " 
     "will change the string to THIS and return it. "
     "NOTE THAT THIS ACTUALLY MODIFIES THE ORIGNAL STRING; "
     "If you want to preserve the string, use TRANSLATE-STRING."
     )
  (if (and (simple-string-p string) 
           (simple-string-p from) 
           (simple-string-p to))
      (ntranslate-string-fast string from to)
    (loop for i below (length string)
          as pos = (position (aref string i) from)
          when pos
          do (setf (aref string i) (aref to pos))))
  string)

(defun ntranslate-string-fast (string from to)
  (declare (simple-string string from to))
  (let ((ls (length string)) (lf (length from)))
    (declare (fixnum ls lf))
    ;; Completely arbitrary test for using hash algorithm.
    (if (and (> lf 10) (> ls 100))
        (let ((ht (make-hash-table :test 'eql)))
          (loop for i fixnum below lf do
                (setf (gethash (schar from i) ht) (schar to i)))
          (loop for i fixnum below ls 
                as translation = (gethash (schar string i) ht)
                when translation
                do (setf (schar string i) translation)
                ))
      (loop for i fixnum below ls
            as pos = (position (schar string i) from)
            when pos
            do (setf (schar string i) (schar to pos)))))
  string)

(defun vector->string (v &optional (len (length v)))
  (let ((string (make-string len)))
    (dotimes (i len)
      (setf (char string i) (code-char (aref v i)))) 
    string))

;;; inefficient, if the purpose is to make long lists manageable.  Easy to fix.
(defun list-truncate (list length)
  (if (> (length list) length)
      (subseq list 0 length)
      list))

;;; Function definition

#-ABCL ; already built in, hopefully compatible
(defmacro defsubst (name args &body body)
  "Define an inline function."
  `(progn
     (declaim (inline ,name))       
     (defun ,name ,args
       ,@body)))

#+ABCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'extensions::defsubst))

#|  This has vanished from Clozure
#+CCL (pushnew "subst" 
               (cdr (assoc 'function ccl::*define-type-alist*)))
|#

; Define a memoized function.  The function should be a function in the mathematical sense
; (a mapping with no state dependencies).  
; Comparision of new args to old is by EQUAL.  Redefining the function resets
; the cache; also defines "reset-<fun>" to clear the cache.
; +++ handle declarations in body
(defmacro def-cached-function (name arglist &body body)
  `(progn
     (defun ,name (&rest args)
       (declare (dynamic-extent args))
       (let ((ht (or (get ',name :cache)
		     (setf (get ',name :cache) (make-hash-table :test #'equal)))))
	 (multiple-value-bind (val found)
	     (gethash args ht)
	   (if found 
	       val
	       (setf (gethash (copy-list args) ht)
		     (block ,name
		       (destructuring-bind ,arglist args
			 ,@body)))))))
     (defun ,(symbol-conc 'reset- name) ()
	 (clrhash (get ',name :cache)))))


;;; alt version for single argument, uses eql as test, more efficient
(defmacro def-cached-function-1 (name arglist &body body)
  (assert (= 1 (length arglist)))
  (let ((ht (make-hash-table :test #'eql)))
    (setf (get name :cache) ht)		;allows access
    `(defun ,name ,arglist
       (multiple-value-bind (val found)
	   (gethash ,(car arglist) ,ht)
	 (if found 
           val
           (setf (gethash ,(car arglist) ,ht)
                 (block ,name
                     ,@body)))))))

(defun symbolize (thing)
  (etypecase thing
    (symbol thing)
    (string (keywordize thing))))

;;; Stolen (with mods) from Alexandria
(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  (let ((fname (symbolize name)))
    `(labels ((,fname ,lambda-list ,@body))
     #',fname)))

;;; Randomness

(defun random-element (list)
  (and list
       (nth (random (length list)) list)))

(defun arand (center range)
  "Return a random number from the range [center-range, center+range]"
  (+ center (random (* 2.0 range)) (- range)))

;;; Numbers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant single-pi (coerce pi 'single-float)))	;Avoid introducing double-floats
(defconstant pi/2 (/ single-pi 2.0))
(defconstant pi/4 (/ single-pi 4.0))
(defconstant 2pi (* single-pi 2))
(defconstant degrees-to-radians (/ 2pi 360))
(defconstant radians-to-degrees (/ degrees-to-radians))
(defmacro d2r (deg)
  `(* degrees-to-radians ,deg))
(defmacro d2ri (deg)
  (* degrees-to-radians deg))
(defmacro r2d (rad)
  `(* radians-to-degrees ,rad))

;;; Fast fixnum arithmetic  

;;; Define functions such as +& that work only for fixnum arguments and results, and 
;;; omit typechecking if the compiler is smart enough (in MCL, it often is -- use DISASM
;;; to check what the compiler is putting out).

(defmacro int (x) `(the fixnum ,x))

(defmacro def-fixnum-op (int-name reg-name &optional (result-type 'fixnum))
  `(defmacro ,int-name (&rest args)
     `(the ,',result-type (,',reg-name ,@(mapcar #'(lambda (arg) `(the fixnum ,arg)) args)))))


(def-fixnum-op +& +)                     ; Only addition-type ops actually get any boost in MCL2.0
(def-fixnum-op -& -)
(def-fixnum-op incf& incf)
(def-fixnum-op decf& decf)
(def-fixnum-op 1+& 1+)
(def-fixnum-op 1-& 1-)
(def-fixnum-op =& = atom)
(def-fixnum-op <& < atom)
(def-fixnum-op <=& <= atom)
(def-fixnum-op >& > atom)
(def-fixnum-op >=& >= atom)
(def-fixnum-op zerop& zerop atom)
(def-fixnum-op plusp& plusp atom)
(def-fixnum-op minusp& minusp atom)

(def-fixnum-op logand& logand)
(def-fixnum-op logior& logior)
(def-fixnum-op lognot& lognot)
(def-fixnum-op logandc1& logandc1)
(def-fixnum-op logxor& logxor)

(def-fixnum-op *& *)                     ; these only work in a few cases (like (* 2 <var>))
(def-fixnum-op /& /)

(def-fixnum-op mod& mod)

(defsubst max& (a b)
  (if (>=& a b)
    a b))

(defsubst min& (a b)
  (if (<=& a b)
    a b))

(defmacro ^ (x y)
 `(expt ,x ,y))

(defsubst sign (num)
  (cond ((plusp num) 1)
	((minusp num) -1)
	(t 0)))

;;; ???
(defun absmin (n min)
  (if (plusp n)
    (min n min)
    (max n (- min))))

(defsubst abs-max (max num)
  (if (<= (abs num) max)
      num
      (* max (sign num))))

(defun integers (from to)
  (if (equal from to) (list from)
      (cons from (integers (+ from (sign (- to from))) to))))

(defun log2 (x)
  (/ (log x) #.(log 2)))

(defun number-of-bits (n)   ;;; smallest k st 2**k>=n
  (ceiling (log2 n)))

(defun bits (n &aux result)
  (dotimes (bit (number-of-bits n) (nreverse result))
    (unless (zerop (logand (ash 1 bit) n) )
      (push bit result))))

(defun sum-list (list)
  (summing (dolist (elt list) (sum elt))))

(defun average (list)
  (if (null list) 0
      (/ (sum-list list) (length list))))

(defun std-dev (list &aux (average (average list)))
  (sqrt (/ (summing (dolist (elt list)
		      (sum (expt (- elt average) 2))))
	   (length list))))

(defun geo-mean (list)
  (nth-root (apply #'* list) (length list)))

(defun nth-root (x n)
  (if (> x 0) (exp (/ (log x) n))
      (error "In NTH-ROOT, X=~S is not positive." x)))

(defun coerce-integer (thing &key no-error (default thing))
  (typecase thing
    (number thing)
    (string 
     (multiple-value-bind (num end) (parse-integer thing :junk-allowed t)
       (if (eq end (length thing))
	   num
	   (if no-error
	       default
	       (error  "can't coerce ~A to a number" thing)))))
    (t (if no-error default (error "can't coerce ~A to a number" thing)))))


(defun coerce-number (thing &key no-error (default thing))
  (typecase thing
    (number thing)
    (string 
     (let ((thing (ignore-errors (read-from-string thing))))
       (if (numberp thing)
	   thing
	   (if no-error
	       default
	       (error  "can't coerce ~A to a number" thing)))))
    (t (if no-error default (error "can't coerce ~A to a number" thing)))))

;;; Fast versions of non-arithmetic functions

(defsubst car& (cons)
  (car (the cons cons)))

(defsubst cdr& (cons)
  (cdr (the cons cons)))

;;; Assumes args are the right type and does no bounds checking
(defsubst svref& (vector index)
  (declare (optimize (speed 3) (safety 0)))
  (svref vector (the fixnum index)))

(defsetf svref& (vector index) (new-val)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (svref ,vector (the fixnum ,index)) ,new-val)))

(defsubst schar& (string index)
  (declare (optimize (speed 3) (safety 0)))
  (schar (the simple-string string) (the fixnum index)))

(defsetf schar& (string index) (new-val)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (setf (schar (the simple-string ,string) (the fixnum ,index)) ,new-val)))

;;; Note:  this has no speedup effect in MCL 2.0
;;; +++ not setfable
(defmacro aref& (array &rest indicies)
  `(locally
     (declare (optimize (speed 3) (safety 0)))
     (aref ,array ,@(mapcar #'(lambda (index) `(the fixnum ,index)) indicies))))

;;; Symbols and packages

;;; package defaults to *package*, which is not always right
(defun symbol-conc (&rest parts)
  (intern (apply #'concatenate 'string (mapcar 'fast-string parts))))

;;; +++ causing package problems
'(defun keyword (symbol)
  (intern (string symbol) (find-package :keyword)))

(defun keywordize (symbol)
  (when symbol (intern (string symbol) (find-package :keyword))))

(defun up-keywordize (symbol)
  (when symbol (intern (string-upcase (string symbol)) (find-package :keyword))))

;;; CL provides no externalp function, and neither does MCL (although it keeps this info with the symbol).
(defun externalp (symbol)
  (multiple-value-bind (ignore type) 
      (find-symbol (symbol-name symbol) (symbol-package symbol))
    #-CCL (declare (ignore ignore))
    (eq type :external)))

(defun add-nickname (package nickname)
  (rename-package package
                  (package-name package)
                  (adjoin nickname (package-nicknames package) :test #'string-equal)))

;;; Not working and not used.
(defun use-package-safely (used user)
  (do-symbols (s used)
    (when (and (externalp s)
	       (find-symbol (symbol-name s) user))
      (shadowing-import s user)
      (export s user)))
  (use-package used user))


;;; Time

;;; these are slightly mistitled, since they don't necessarily return strings anymore

(defun date-time-string (universal-time &key (include-time t) (include-date t) (include-day t) (stream nil))
  #.(doc "Turn a universal time into a string.  Arguments are fairly obvious."
	 ":include-day and :include-date can take the value :unless-today, in which case date or day"
	 "is only included if the time is not today.")
  (multiple-value-bind (second minute hour date month year day-of-week) 
                       (decode-universal-time universal-time)
    (declare (ignore second))
    (let* ((months '(January February March April May June 
                    July August September October November December))
          (days '(monday tuesday wednesday thursday friday saturday sunday))
	  (today? (multiple-value-bind (nsecond nminute nhour ndate nmonth nyear)
		      (decode-universal-time (get-universal-time))
		    (declare (ignore nsecond nminute nhour))
		    (and (= ndate date)
			 (= nmonth month)
			 (= nyear year))))
	  (include-day (if (eq include-day :unless-today)
			   (not today?)
			   include-day))
	  (include-date (if (eq include-date :unless-today)
			   (not today?)
			   include-date))
	  )
      (format stream "~:[~5*~;~:[~*~;~A ~]~A ~A, ~A~]~:[~; ~A:~2,'0D~A~]" 
	      include-date
	      include-day
              (string-capitalize (string (nth day-of-week days)))
              (string-capitalize (string (nth (- month 1) months)))
              date year 
              include-time
              (mod hour 12) minute 
              (if (>= hour 12) "pm" "am")))))

(defun short-date-time-string (universal-time &optional (include-time t) (stream nil))
  (multiple-value-bind (second minute hour date month year day-of-week) 
                       (decode-universal-time universal-time)
    (declare (ignore second day-of-week))
    (format stream "~A/~A/~A~:[~; ~A:~2,'0D~]" 
            month date
            (- year (* 100 (floor year 100)))
            include-time
            hour minute)))

; : means use short format
; @ means omit the time
(defun format-time (stream ut colon-flag at-flag)
  (funcall (if colon-flag
             #'short-date-time-string
             #'date-time-string)
           ut 
           (not at-flag)
           stream))

;;; Streams and strings

(defun new-string (&optional (initial-length 10))
  (make-array initial-length :element-type 'character 
              :adjustable t :fill-pointer 0))


;;; Limitation: these only work with character streams; could be generalized
(defun stream-copy (in out)
  (do (char) (())
    (setq char (read-char in nil :eof))
    (if (eq char :eof)
      (return)
      (write-char char out))))

(defmacro dolines ((var stream) &body body)
  `(do ((,var (read-line ,stream nil :eof)
	      (read-line ,stream nil :eof)))
       ((eq ,var :eof))
     ,@body))

(defun stream-copy-by-lines (in out)
  (dolines (line s)
    (write-line line out)))

(defun file-copy (in out)
  (with-open-file (ins in)
    (with-open-file (outs out :direction :output)
      (stream-copy ins outs))))

(defun file-to-string (file &key (max 1000000))
  #.(doc
     "Returns a string containing all the characters in FILE with line"
     "terminators converted to Newlines.  If the string would exceed MAX"
     "characters (default a million) a warning is issued and NIL is returned.")
  (with-open-file (p file :direction :input)
    (let ((buffer-size (file-length p))
	  buffer)
      (when (> buffer-size max)
        (warn 
         "File ~A is too big! Allowed: ~A, actual: ~A"
         file max buffer-size)
        (return-from file-to-string nil))    
      (setf buffer (make-string buffer-size))
      (read-sequence buffer p)
      buffer)))

(defun string-to-file (string file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (write-string string out)))

(defun this-pathname ()
  "Returns the pathname of the file currently being loaded." 
  #+:allegro excl:*source-pathname* 
  #+:ccl (when ccl:*loading-file-source-file* (parse-namestring ccl:*loading-file-source-file*))
  #-(or :allegro :ccl) 
  (or *load-pathname* *compile-file-pathname*))

(defun relative-pathname (name &optional directories)
  (make-pathname :defaults (pathname name)
		 :directory (append (pathname-directory *load-pathname*)
				    directories)))

(defun pprint-to-string (struct &optional (right-margin *print-right-margin*))
  (with-output-to-string (stream)
    (let ((*print-pretty* t)
          (*print-right-margin* right-margin))
      (write struct :stream stream))))

(defun read-until (stream end-char-or-pred &optional (string (new-string)) untyi-end?)
  (do ((char (read-char stream nil :eof) (read-char stream nil :eof) ))
      ((cond ((characterp end-char-or-pred)
	      (char= char end-char-or-pred))
	     ((eq char :eof))
	     (t (funcall end-char-or-pred char)))
       (when (and untyi-end? (characterp char))
         (unread-char char stream))
       (values string char))
    (when string 
      (vector-push-extend char string))))

(defun read-until-string (stream end-string &optional (string (new-string)))
  (let* ((end-string-length (length end-string))
         (last-char (aref end-string (1- end-string-length))))
    (do ()
        (())
      (read-until stream last-char string)
      (when (string= end-string
                     string
                     :end1 (1- end-string-length)
                     :start2 (- (length string) end-string-length -1))
        (return string)))))

(defvar *whitespace* '(#\Space #\Tab #\Return #\Newline #\Page #\Null #\Linefeed #+MCL #\312))

;;; +++ punt this and use regexps
(defun string-split (str &optional (char #\space) count)
  #.(doc
     "Given a string STR, return a list of the strings between occurances of CHAR.")
  (let ((loc (position char str))
	(start 0)
	(res))
    (if (null loc)
	;; doesn't appear anywhere, just the original string
	(list str)
	;; must do some work
	(loop
	   (push (subseq str start loc) res)
	   (setq start (1+ loc))
	   (if count (decf count))
	   (setq loc (position char str :start start))
	   (when (or (null loc)
		     (eql 0 count))
	     (if (< start (length str))
		 (push (subseq str start) res)
		 (push "" res))
	     (return (nreverse res)))))))

(defun string-split-words (str &optional (whitespace *whitespace*))
  ;; split the given string into words (items separated by white space)
  ;;
  (let ((state 0)
	(i 0)
	(len (length str))
	(start nil)
	(res)
	(ch)
	(spacep))
    (loop
      (if (>= i len)
	  (setq ch #\space)
	  (setq ch (char str i)))
      (setq spacep (fast-whitespacep ch whitespace))
      (case state
	(0  ; looking for non-space
	 (if (not spacep)
	     (setq start i
		   state 1)))
	(1  ; have left anchor, looking for space
	 (when spacep
	     (push (subseq str start i) res)
	     (setq state 0))))
      (when (>= i len) (return))
      (incf i))
    (nreverse res)))

(defun fast-whitespacep (char &optional (whitespace *whitespace*))
  #-:SBCL (declare (optimize (speed 3) (safety 0)))
  (member (the character char) whitespace :test #'eql))

(defun string-fast-whitespacep (string &optional (whitespace *whitespace*))
  #-:SBCL (declare (optimize (speed 3) (safety 0)))
  (dotimes (i (length string) t)
    (unless (fast-whitespacep (char string i) whitespace)
      (return-from string-fast-whitespacep nil))))

(defun string-trim-whitespace (string)
  (string-trim *whitespace* string))

(defun string-remove-chars (string char-bag)
  (remove char-bag string :test #'(lambda (a b) (find b a :test #'equal))))

(defun string-remove-whitespace (string)
  (string-remove-chars string *whitespace*))

;;; Hash tables

(defun dump-ht (ht)
  (maphash #'(lambda (key value)
               (format t "~%~A: ~A" key value))
           ht))

;;; Limit only returns a few elements (note: still slow for large tables in CCL, maphash must be doing something stupid)
(defun ht-contents (ht &key limit)
  (let ((result nil)
	(count 0))
    (maphash #'(lambda (key value)
                 (push (list key value) result)
		 (when (and limit 
			    (> (incf count) limit))
		   (return-from ht-contents result)))
           ht)
    result))

(defun hash-keys (ht)
  (loop for k being the hash-keys of ht collect k))

;;; CLOS utilities

(defmethod subclasses ((c class))
   (remove-duplicates
    (transitive-closure c 
			#+:CCL #'ccl:class-direct-subclasses
			#+:ACL #'mop:class-direct-subclasses
			#+:SBCL #'sb-mop:class-direct-subclasses)))

(defmethod superclasses ((c class))
  (remove-duplicates
   (transitive-closure c 
		       #+:CCL #'ccl:class-direct-superclasses
		       #+:ACL #'mop:class-direct-superclasses
		       #+:SBCL #'sb-mop:class-direct-superclasses)))

(defclass plist-mixin () ((plist :initform nil)))

(defmethod oget ((o plist-mixin) property &optional (default nil))
  (getf (slot-value o 'plist) property default))

(defmethod oput ((o plist-mixin) property value)
  (setf (getf (slot-value o 'plist) property)
        value))

#+:CCL
(defun call-if (gf &rest args)
  #.(doc
     "call a generic function iff it is implemented for the args."
     "Note: this won't work when a gf gets encapsulated (ie, by trace or metering)")
  (when (apply #'ccl:method-exists-p gf args)
    (apply gf args)))

;;; Anaphoric macros (from _On Lisp_ by Paul Graham)

(defmacro aif (if then &optional else)
  `(let ((it ,if))
     (if it
       ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
        (progn ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

;;; +++ a single-form clause doesn't do the right thing (ie what cond does)
(defmacro acond (&rest clauses)
  (when clauses
    (let ((clause (car clauses))
          (sym (gensym)))               ; used so that it is bound only around rhs of clause
      `(let ((,sym ,(car clause)))
         (if ,sym
           (let ((it ,sym))
	     (declare (ignorable it))
	     ,@(cdr clause))
           (acond ,@(cdr clauses)))))))

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))

;;; Higher order procedures

;;; Curried functions.

(defun curry (function &rest curried-args)
  #'(lambda (&rest rest-args)
      (apply function (nconc curried-args rest-args))))

(defun rcurry (function &rest curried-args)
  #'(lambda (&rest rest-args)
      (apply function (nconc rest-args curried-args))))

#|
example: (defun numsort (l) (funcall (rcurry #'sort #'<) l)) 

Why is this easier than writing a lambda expression? Well, if it was scheme we could write simply:
  (define numsort (rcurry sort <))
which is nice and compact. But, this isn't Scheme. Still, these are occasionally useful.
 |#

(defun transitive-closure (thing proc)
  #.(doc 
     "PROC is a procedure of one arg that returns a list."
     "Thing is a list of starting points, or a single non-list")
  (do ((done nil)
       (fringe (if (listp thing) thing (list thing))))
      ((null fringe) done)
    (let ((new (pop fringe)))
      (push new done)
      (dolist (obj (funcall proc new))
	(unless (member obj done)
	  (push obj fringe))))))

(defun transitive-closure-procedure (proc)
  (rcurry #'transitive-closure proc))

#|
example:
(defun all-values (f) (collecting (for-all-frames (f) (for-frame-slots (f s v) (dolist (vv v) (collect-new vv))))))

(funcall (mt::transitive-closure-procedure #'all-values) a-frame)
|#

;;; Saving and restoring

;;; Dump an arbitrary structure to a file. If CLOS objects are part of the structure,
;;; make-load-form will be called.  See clos-dumper.lisp for a slightly more sophisticated
;;; version of this.

(defun dump-vars-to-file (vars file)
  (with-open-file (s (merge-pathnames ".lisp" file) :direction :output :if-exists :supersede)
    (format s "(in-package :~A)~%" (package-name *package*))
    (dolist (var vars)
      (format s "~%(setf ~S '#.~S)"
              var var)))
  (compile-file file)
  (delete-file file))                   ; get rid of lisp file

;;; older function, here for compatibility
(defun dump-var-to-file (var file)
  (dump-vars-to-file (list var) file))

;;; Bit manipulation

;;; Define bit-fields in any setf'able place
;; Example: (defbit node-expanded? 'node-bits 1)
;; uses fixnum declarations!  Better initialize field to zero, not nil!
(defmacro defbit (name field pos)
  (let ((setter (symbol-conc 'set- name)))
    `(progn
       (defmacro ,name (obj)
         `(not (zerop& (logand& (,,field ,obj) ,,(expt 2 pos)))))
       (defmacro ,setter (obj new-val)
         (once-only (obj new-val)
           `(progn
              (setf (,,field ,obj)
                  (if ,new-val
                    (logior& ,,(expt 2 pos) (,,field ,obj))
                    (logandc1& ,,(expt 2 pos) (,,field ,obj))))          ; +++ not inlined in MCL3?
              ,new-val)))
       (defsetf ,name ,setter))))

(defun getbit (n pos)
  (not (zerop& (logand& n (expt 2 pos)))))

(defun setbit (n pos v)
  (if v 
      (logior& n (expt 2 pos))
      n))

(defun xor (a b)
  (and (or a b)
       (not (and a b))))

;;; Debugging

;;; I don't use this much anymore: see CTRACE.LISP for a more flexible way to do this.

(defvar *debug-level* 0)
(defvar *debug-indent-level* 0)
(defvar *debug-stream* *trace-output*)

(defun debug-line-out (string &rest args)
  (terpri *debug-stream*)
  (dotimes (n (* *debug-indent-level* 2)) (write-char #\space *debug-stream*))
  (apply #'format *debug-stream* string args))

(defmacro debug-trace (level string &rest args)
  `(when (<= ,level *debug-level*)
     (debug-line-out ,string ,@args)))

(defmacro with-debug-trace ((level string &rest args) &body body)
  `(let ((*debug-indent-level* *debug-indent-level*))
     (when (<= ,level *debug-level*)
       (debug-line-out ,string ,@args)
       (incf *debug-indent-level*))
     ,@body))

;;; wrap this around a function call form to trace it (good for when you can't use TRACE, ie, low-level CL functions)
(defmacro itrace (form)
  `(let ((args (list ,@(cdr form)))
	 value)
     (print `(calling ,',(car form) ,@args))
     (setf value (apply ',(car form) args))
     (print `(result ,value))
     value))

;;; Like ignore-errors, but will output the error message 
(defmacro report-and-ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) 
            (format *debug-stream* "~%Error: ~A~%" condition)
	    (values nil condition))))

(defmacro let*-debug (forms &body body)
  `(let* ,(mapcar #'(lambda (form)
		      `(,(car form)
			 (progn (format t "~%~A: " ',(car form))
				(print ,(cadr form)))))
		  forms)
     ,@body))

(defun now ()
  (get-universal-time))

(defmacro without-interrupts (&body body)
  `(#+:CCL 
    ccl:without-interrupts
    #-:CCL 
    ,(error "without-interrupts not supported in this lisp")
    ,@body))


;;; Run a section of code in a background process, errors are reported.  Name must be a string.
;;; A closure is made around the body.

(defmacro in-background (name &body body)
  `(#+:ALLEGRO mp:process-run-function #+:ACL-COMPAT acl-compat.mp:process-run-function 
    ,name
    #'(lambda () 
        (handler-case (progn ,@body)
          (error (condition)
	    (format *debug-stream* "~%Error in process ~A: ~A" ,name condition))))))


;;; These are useful in conjunction with functions that take keywords and use &allow-other-keys
#|
Example:
(defun link-to-remote (text url &rest remote-function-options &key html-options &allow-other-keys)
  (link-to-function text (apply #'remote-function url (delete-keyword-args '(:html-options) remote-function-options))
		    :html-options html-options))

The uncommented version can cons; here's a version that modifies arglist, but it doesn't work in all lisps and has
the usual risks associated with mutating lists.

(defun delete-keyword-arg (key arglist)
  (awhen (position key arglist)
    (if (zerop it)
	(setf arglist (cddr arglist))
	(setf (nthcdr it arglist) (nthcdr (+ it 2) arglist))))
  arglist)

|#

(defun delete-keyword-arg (key arglist)
  (aif (position key arglist)
    (if (zerop it)
	(cddr arglist)
	(append (subseq arglist 0 it) (nthcdr (+ it 2) arglist)))
    arglist))

(defun delete-keyword-args (keys arglist)
  (if (null keys) arglist
      (delete-keyword-arg (car keys) (delete-keyword-args (cdr keys) arglist))))

;;; Debugging
;;; Substitute these for let or let* to get a trace of the binding forms. Damn useful!
(defmacro plet* (bind-forms &body body)
  `(let* ,(mapcar #'(lambda (form)
		      (if (listp form)
			  `(,(car form) (let ((v ,(cadr form)))
					  (print (list ',(car form) v))
					  v))
			  form))
		  bind-forms)
     ,@body))

(defmacro plet (bind-forms &body body)
  `(let ,(mapcar #'(lambda (form)
		     (if (listp form)
		      `(,(car form) (let ((v ,(cadr form)))
				      (print (list ',(car form) v))
				      v))
		      form))
		  bind-forms)
     ,@body))

(provide :mt-utils)

