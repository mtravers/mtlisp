(defpackage :closstar
  (:nicknames "CLOS*")
  (:use #+genera clos #+MCL ccl common-lisp)
  (:export "DEFCLASS*" "DEFMETHOD*"))

(in-package :clos*)

#| ######################################################################

 Alternate syntax for CLOS

Copyright © 1994-97 Michael Travers 

Permission is given to use and modify this code
as long as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

This package provides two defining forms, defclass* and defmethod*, that 
work like the ordinary CLOS forms but provide some of the convenience 
features of the Flavors system.  Initially developed to run Flavors code
under CLOS without conversion, CLOS* is also useful for rapid prototyping
since it allows class and method definitions to expressed more succinctly
than the default CLOS syntax.

defclass* uses a more convienient defflavor-like syntax for expressing
slot options.  

Example:
(defclass* pixmap-dialog-item (dialog-item)
  (pixmap
   (dispose nil))                       ; slot with an initial value of NIL
  :initable-instance-variables)         ; define init keywords for all slots

defmethod* makes the slots of the first argument available as symbols (that
is, it does an implicit (with-slots <all-slots> <1st-arg> ...)). Purists
will disapprove. WITH-SLOTS generates calls to SLOT-VALUE, which is
generally slower than using accessors. 

If an argument has the same name as a slot, that slot won't be used (the 
argument takes priority).

WARNING: defmethod* requires that the class be fully defined at compile
time. Compiling a defclass* form to a file will also define the class in
Lisp, but compiling a normal defclass won't. So, using defclass and
defmethod* together may break depending on your compilation and loading
scheme.


History:
Based on my old Artificial Flavors package
PCL version 31 March 90
CLOS version 13 August 90

6/13/97 7:52pm   DEFCLASS* can deal with DEFCLASS style iv-defs

###################################################################### |#


;;; see header for doc.
(defmacro defclass* (name components ivs &rest options)
  (let ((reader-slots nil)
        (writer-slots nil)
        (init-slots nil)
        (bare-ivs nil)
        (class-options nil)
        (keyword-package (find-package "KEYWORD")))
    (dolist (option options)
      (let ((option-name (if (listp option) (car option) option))
            (option-value (if (listp option) (cdr option) nil)))
        (flet ((spec-or-all-ivs (spec)
                 (or spec
                     (mapcar #'(lambda (iv) (if (listp iv) (car iv) iv)) ivs))))
          (case option-name
            (:writable-instance-variables
             (setq writer-slots (spec-or-all-ivs option-value))
             (setq reader-slots (nunion reader-slots (spec-or-all-ivs option-value))))
            (:readable-instance-variables
             (setq reader-slots (nunion reader-slots (spec-or-all-ivs option-value))))
            (:initable-instance-variables
             (setq init-slots (spec-or-all-ivs option-value)))
	    (:abstract
	     (when option-value
	       ;; no-op at the moment, idea is to set up something on initialize-instance that errors out. +++
	       ))
            (t (push option class-options))))))
    (setq reader-slots (set-difference reader-slots writer-slots))
    (flet ((process-iv (iv-form)
             (let ((iv (if (listp iv-form) (car iv-form) iv-form)))
               (push iv bare-ivs)
               `(,iv
                 ,@(if (listp iv-form)
                     (if (= 2 (length iv-form))
                       `(:initform ,(cadr iv-form))
                       (cdr iv-form)))
                 ,@(if (member iv init-slots)
                     `(:initarg ,(intern (symbol-name iv) keyword-package)))
                 ,@(if (member iv reader-slots)
                     `(:reader ,(symbol-conc name "-" iv)))
                 ,@(if (member iv writer-slots)
                     `(:accessor ,(symbol-conc name "-" iv)))))))
      (let ((defclass-form `(defclass ,name ,components ,(mapcar #'process-iv ivs) ,@class-options)))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)   
             ,defclass-form)
	   ))
        )))

(defun symbol-conc (&rest components)
  (intern (apply #'concatenate 'string (mapcar #'string components))))

;;; see header for doc.
(defmacro defmethod* (&rest args)
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (decls main-body)
        (split-off-declarations body)
      (let* ((class (cadar lambda-list)) 
	     (arguments (mapcar #'(lambda (arg-elt) (if (listp arg-elt) (car arg-elt) arg-elt)) lambda-list))
             (slots (slots-for-class class))
	     (use-slots (set-difference slots arguments)))
        (unless slots
          (warn "In DEFMETHOD* ~A, no slots for ~A, may be some finalization problems" name class))
        `(defmethod ,name ,@qualifiers ,lambda-list
                    ,@decls
           (with-slots ,use-slots ,(caar lambda-list)
             ;; Eliminate compiler warnings
             #+:CCL (declare (ccl::ignore-if-unused ,@use-slots)) 
             ,@main-body))))))

;; not sure where this worked, but it doesn't in ACL6.2 or ABCL or SBCL or CCL...
#-(or :allegro :abcl :sbcl :ccl)
(setf (arglist 'defmethod*)
      '(name |{method-qualifier}*|
        specialized-lambda-list
        &body body))

(defun parse-defmethod (form)
  (let ((name (first form))
        (qualifiers nil))
    (do ((rest (cdr form) (cdr rest)))
	 ((not (symbolp (car rest)))
         (values name (nreverse qualifiers) (car rest) (cdr rest)))
      (push (car rest) qualifiers))))

#| obso in clozure
#+:CCL
(defun slots-for-class (class-name)
  (let ((class (find-class class-name)))
    (nconc (mapcar #'slot-definition-name
                   (class-instance-slots class))
           (mapcar #'slot-definition-name
                   (class-class-slots class)))))
|#

#+:CCL
(defun slots-for-class (class-name)
  (let ((class (find-class class-name)))
    (mapcar #'ccl:slot-definition-name (ccl:compute-slots class))))

#+:ALLEGRO
(defun slots-for-class (class-name)
  (let ((class (find-class class-name)))
    (mapcar #'clos:slot-definition-name (clos:compute-slots class))))

#+:ABCL
(defun slots-for-class (class-name)
  (mapcar #'(lambda (sd)
	      (slot-value sd 'system::name))
	  (system:%class-slots (find-class class-name))))

#+:SBCL
(defun slots-for-class (class-name)
  (let ((class (find-class class-name)))
    (sb-mop:finalize-inheritance class)
    (mapcar #'sb-mop:slot-definition-name
	    (sb-mop:class-slots class))))

(defun split-off-declarations (body)
  (do ((rest body (cdr rest))
       (declarations nil))
      ((null rest)
       (values declarations nil))
    (if (or (stringp (car rest))
            (and (listp (car rest))
                 (eq 'declare (car (car rest)))))
        (push (car rest) declarations)
        (return-from split-off-declarations
          (values (nreverse declarations) rest)))))

#| No longer in Clozure, need to find replacements
#+CCL (add-definition-type 'class "CLASS*")
#+CCL (add-definition-type 'method "METHOD*")
|#

;;; Not really clos*, but
#+CCL
(defgeneric oequal (o1 o2))

#+CCL
(defmethod oequal ((o1 t) (o2 t))
  (if (and (ccl::standard-instance-p o1)
	   (ccl::standard-instance-p o2))
      (and (eq (type-of o1) (type-of o2))
	   (let ((c (type-of o1)))
	     (dolist (s (slots-for-class c) t)
	       (unless (oequal (slot-value o1 s)
			       (slot-value o2 s))
		 (return nil)))))
      (equal o1 o2)))

(export '(defmethod* defclass*))

(provide :closstar)
