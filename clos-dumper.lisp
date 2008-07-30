(in-package :cl-user)


#| ######################################################################

 Structure dumper.

Copyright © 1994-97 Michael Travers 

Permission is given to use and modify this code
as long as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

What this does:  Saves a structure of CLOS objects (and other lisp objects) as an executable
form that recreates the structure when loaded.

How it works: Recursively calls DUMP-FORM on an object. The default DUMP-FORM for CLOS
objects calls SLOT-DUMP-FORMS, which should return an init list that will be passed
to make-instance when the object is re-created.  The macro MAKE-SLOT-DUMPER is the
easiest way to specify which slots of a class are to be dumped.

Use the function DUMP-TO-FILE to create a dump file which can be read back with LOAD.

Objects are only dumped once, so "tangled trees" are handled properly. But
circularities are NOT handled. If you have back-pointers in your structure they should
not be dumped, instead arrange to have them set up automatically when the object is
initialized. For example, if you were saving a structure of nested views, you would
dump either the VIEW-CONTAINER slot or the VIEW-SUBVIEWS slot, but not both.

History:
1/10/97 18:57  Revived, fixed a sequence problem for MCL4


###################################################################### |#

(defvar *dump-ht*)                      ; Bound
(defvar *prepass* nil)
(defvar *prelude-vars*)
(defvar *dumper-gensym-counter*)

(defvar *dump-temp-package* (make-package "DUMP-TEMP" :nicknames '("DT")))

(defun dump (thing)
  (let ((*dump-ht* (make-hash-table :test 'eq))
        (*prelude-vars* nil)
        (*dumper-gensym-counter* 0))
    (let ((*prepass* t))
      (dump-form thing))
    (let ((form (dump-form thing)))
      (values `(let ,*prelude-vars* ,form)
              *dump-ht*))))
   
(defmethod dump-form :around ((r standard-object))
  (if *prepass*
    (let ((hash-result (gethash r *dump-ht*)))
      (if hash-result
        (case (car hash-result)
          (:one-ref 
           (setf (gethash r *dump-ht*) (cons :multi-ref (cdr hash-result)))
           (cdr hash-result))
          (:in-progress (warn "losing circularity on ~A" r)))
        ;; this could be a case clause but MCL has a bug
        (progn
          (setf (gethash r *dump-ht*) '(:in-progress))
          (let ((result (call-next-method)))
            (setf (gethash r *dump-ht*) (cons :one-ref result))
            result))))
    ;; Second pass
    (let ((hash-result (gethash r *dump-ht*)))
      (case (car hash-result)
        ;; Just one ref, no prob
        (:one-ref (call-next-method))
        ;; Mult ref, first time
        (:multi-ref
         (let ((sym (intern (concatenate 'string "TEMP" (princ-to-string (incf *dumper-gensym-counter*)))
                            *dump-temp-package*)))
           (push sym *prelude-vars*)
           (setf (gethash r *dump-ht*)
                 `(:in-progress ,sym))
           (prog1
             `(setf ,sym ,(call-next-method))
             (setf (gethash r *dump-ht*)
                   `(:second-ref ,sym)))))
        (:in-progress
         (error ";;; shit this can't work"))
        (:second-ref
         (cadr hash-result))
        (t (error "ugh"))))))

;;; Most things dump as themselves
(defmethod dump-form ((d t))
  d)

(defmethod dump-form ((d standard-object))
  `(make-instance ',(class-name (class-of d)) ,@(slot-dump-forms d)))

(defgeneric slot-dump-forms (d)
  (:method-combination nconc))

(defmethod slot-dump-forms nconc ((d standard-object))
  nil)

;;; defaults to all initable slots
;;; +++ this may lose if class isn't properly initalized at macroexpand time
#+MCL
(defmacro make-slot-dumper (class &rest slots)
  (unless slots (setf slots (ccl::class-make-instance-initargs class)))
  `(defmethod slot-dump-forms nconc ((x ,class))
     (list ,@(loop for s in slots
                   collect (ccl::keywordify s)
                   collect `(dump-form (slot-value x ',s))))))

(defmethod dump-form ((l list))
  (cond ((null l) nil)
        ((null (cdr (last l)))
         `(list ,@(mapcar #'dump-form l)))      ; regular lists
        (t 
         `(cons ,(dump-form (car l)) ,(dump-form (cdr l))))))   ; dotted lists

;;; Pretty inefficient, since it translates sequences to/from lists
(defmethod dump-form ((s sequence))
  `(rebuild-sequence ',(type-of s) ,(length s) ,@(mapcar #'dump-form (coerce s 'list))))

#|  this may work better in MCL4, types are now length specific and LENGTH does not return the right thing sometimes
(defmethod dump-form ((s sequence))
  `(rebuild-sequence ',(type-of s) ,(array-dimension s 0) ,@(mapcar #'dump-form (coerce s 'list))))
|#

;;; Strings dump as themselves.
(defmethod dump-form ((d string))
  d)

(defun rebuild-sequence (type size &rest elements)
  (let ((seq (make-sequence type size)))
    (do ((rest elements (cdr rest))
         (n 0 (1+ n)))
        ((null rest) seq)
      (setf (aref seq n) (car rest)))))

;;; if an object lives in a global variable, mix this in.

(defclass globally-named-object-mixin () 
  ((global-name :initarg :global-name :initform nil)))

(defmethod dump-form :around ((o globally-named-object-mixin))
  (with-slots (global-name) o
    (if global-name
      `(setf ,global-name ,(call-next-method))
      (call-next-method))))

(defmethod slot-dump-forms nconc ((o globally-named-object-mixin))
  `(:global-name ',(slot-value o 'global-name)))

;;; File interface

;;; Note:  it would be nice to dump directly to a compiled form, but MCL doesn't
;;; have hooks for this.

(defvar *dumping-to-file* nil)

(defun dump-to-file (thing file &key (compile t) prelude (package *package*))
  (let ((*package* (find-package package))
        (*dumping-to-file* (pathname file)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (print `(in-package ,(package-name *package*)) stream)
      (print prelude stream)
      (let ((*print-array* t))
        (print (dump thing) stream)))
    (when compile
      (compile-file file))))
