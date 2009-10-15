(in-package :mt)

#| ######################################################################

 CTRACE - internal tracing facility

Copyright © 1994-97 Michael Travers 

Permission is given to use and modify this code
as long as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

An advanced tracing facility.

The standard trace package does not work well in situations where I/O is being
done, such as event handlers. This package performs tracing by creating a list
structure to record function entrances and exits. This structure can be displayed
or processed after the fact. The package also includes the concept of a tracing
level, so that varying amount of detail can be recorded.

Todo:
- handle multiple processes (use let-globally, record *current-process*)
- include options for live (printing) tracing as well as structure building
- perhaps use objects rather than lists for recording structure

Quick start:
(ctrace-function 'my-function1)
(ctrace-function 'my-function2) etc.
(ct (my-computation))                   
will run (my-computation) tracing calls to my-function1 and 2, and displays
the resulting trace in a Fred window.

Documentation:
(CTRACE item &optional level)
Record item if trace level is >= to level. CTRACE is used for putting trace
points in code.

(WITH-CTRACE (item &optional level) . body)
Record an item as in CTRACE, and all traces that occur in body will be
recorded as subitems.

(CTRACE-FUNCTION function-name &optional level)
Advises the function so that it has a WITH-CTRACE wrapped around it.

(UNCTRACE-FUNCTION &optional function-name)
Remove the WITH-CTRACE wrapper from a function (or all such functions if none is specified)

(CTRACE-ALL &optional (package *package*))
ctrace all functions in the given package

(WITH-CTRACING (&optional name level) . body)
Enables ctracing around the execution of body, and returns the the trace

(START-CTRACE &optional name level)
Enable ctracing globally

(STOP-CTRACE)
DIsable global ctracing, and returns the accumulated trace structure.

(VIEW-CTRACE &optional trace)
View a trace in its own window, pretty-printed.

(CT &body body)
Equivalent to (view-ctrace (with-ctracing () <body>)).

Todo:

separate out MCL dependent parts. Can I replace advise with pure CL?

History:  

Ancestry: Henry Lieberman's ZStep, my own PSTEP/PTRACE.
6/25/95  13:08 packaged as separate facility.
6/26/95  17:50 made level arguments optional, flushed name args, improved UNCTRACE-FUNCTION
7/10/95  23:01 fix view-ctrace to invalidate view. Clean up examples.  
               Make trace headers (:local-ctrace, etc) into lists to make the thing easier to parse)
               Interface to Lynch's twist-down for viewing (very preliminary)
10/13/95 18:23:  add CTRACE-ALL. Too slow though, because ADVISE is slow
10/15/95 16:52   add *last-ctrace*, cleaned up function messages
12/10/95 22:51  with-ctracing was setting rather than binding *ctrace-top*
                added error handling
                added ct for convenience
12/20/95 1:21  add fboundp check and error message to ctrace-function
1/6/96  18:13   small improvements to twist-down hack
1/8/96  17:37   updated documentation and code formatting
1/14/96 19:15  added ctt, use geneva for trace-window
3/29/96 17:14  play safe, copy all list structure before recording it. 
               no, that's too much gunk, just do it for ctrace-function, in other cases
               make callers handle it
11/23/96 13:13 ADVISE internals are different in 3.1/4.0
 1/12/97 17:47 fix copy-list-recursive to deal with dotted-pair arguments
 3/18/97 12:36 add without-interrupts to deal with multiple processes somewhat better
 8/07/01 10:09 Make it run under ACL
11/09/07       Run under ABCL (NOT WORKING)
###################################################################### |#
;;; Bureaucracy

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+:ALLEGRO (pushnew :ACL *features*))

; ABCL relies on this encapsulate package, snarfed from OpenMCL. 
;#+:ABCL
;(load "/misc/sourceforge/lsw/trunk/util/encapsulate.lisp") 

;;; CTRACE

(defvar *ctrace-level* 0)
(defvar *ctrace-current-item* nil)
(defvar *ctrace-top*)
(defvar *last-ctrace*)

(defun copy-list-recursive (item)
  (if (consp item)
    (cons (copy-list-recursive (car item))
          (copy-list-recursive (cdr item)))
    item))

(defun ctrace-item-out (item)
  (let ((newbie (list item)))           
    (when (cdr (last *ctrace-current-item*)) (error "foo?"))    ; make sure we aren't clobbering anything
    (rplacd (last *ctrace-current-item*) newbie)
    (setf *ctrace-current-item* newbie)))

; make a ctrace entry. this is a macro so that the computation of the item
; can be done lazily.
(defmacro ctrace (item &optional (level 1))
  `(when (<= ,level *ctrace-level*)
     (ctrace-item-out ,item)
     nil))

; make an entry that includes all activity inside dynamic context
(defmacro with-ctrace ((item &optional (level 1)) &body body)
  `(let ((*ctrace-current-item*
          (if (<= ,level *ctrace-level*)
            (car (ctrace-item-out (list ,item)))
            *ctrace-current-item*)))
     ,@body))

(defvar *ctraced-functions* nil)

;;; put a ctrace around an existing function
(defun ctrace-function (function-name &optional (level 1))
  (assert (fboundp function-name) ()
          "No definition for ~A" function-name)
  (pushnew function-name *ctraced-functions*)
  #+:CCL				;Written for ancient CCL, not clear if it still works
  (let ((newsym (gensym "CTRACE")))
    (ccl::advise-2
     (eval (ccl::advise-global-def
            function-name newsym :around 
            `(with-ctrace (`(calling (,',function-name ,@(copy-list-recursive arglist))) ,level)
               (let ((result (:do-it)))
                 (ctrace `(,',function-name returned ,(copy-list-recursive result)) ,level)
                 result))))
     newsym nil function-name :around 'ctrace nil))
  #+:ACL
  (excl:advise-1 function-name :around :ctrace 1
		 `((with-ctrace (`(calling (,',function-name ,@(copy-list-recursive arglist))) ,level)
		     (let ((result :do-it))
		       (ctrace `(,',function-name returned ,(copy-list-recursive result)) ,level)
		       result))))
  ;; not working yet.  encapsulate:advise doesn't eval its args, so this wont work
  ;; wait, it's from LSW and OpenMCL...still doesn't work though (? say what?)
  #+:ABCL
  (let ((newsym (gensym "CTRACE")))
    (encapsulate::advise-2
      (encapsulate::advise-global-def
            function-name newsym :around 
            `(with-ctrace (`(calling (,',function-name ,@(copy-list-recursive encapsulate::arglist))) ,level)
               (let ((result (:do-it)))
                 (ctrace `(,',function-name returned ,(copy-list-recursive result)) ,level)
                 result)))
     newsym nil function-name :around 'ctrace nil))
  #-(OR :ACL :CCL :ABCL)
  (error "i don't know how to advise functions in this lisp")
  )


(defun unctrace-function (&optional function-name)
  (if function-name
      (unctrace-function-1 function-name)
      (dolist (fn *ctraced-functions* ())
        (unctrace-function-1 fn))))

(defun unctrace-function-1 (function-name)
  (unctrace-function-2 function-name)
  (deletef function-name *ctraced-functions*))

#+:CCL
(defun unctrace-function-2 (function-name)
  (ccl::unadvise-1 function-name :around 'ctrace))

#+:ACL
(defun unctrace-function-2 (function-name)
  (excl:unadvise-1 function-name :around :ctrace))

#+:ABCL
;;; ??? who knows if this is right
(defun unctrace-function-2 (function-name)
  (encapsulate::unadvise-1 function-name))

(defun ctrace-all (&optional (package *package*))
  (let ((package (if (symbolp package)
                   (find-package package)
                   package)))
    (do-symbols (symbol package)
      (when (and (eq (symbol-package symbol) package)
                 (fboundp symbol)
                 (not (macro-function symbol)))
	(ctrace-function symbol)))))


; top-level form to enable ctracing
(defmacro with-ctracing ((&optional (level 1)) &body body)
  `(let ((*ctrace-level* ,level)
         (*ctrace-top* (setf *ctrace-current-item* (list '(:local-ctrace)))))
     (catch :ctrace-err
       (handler-bind
         ((error #'(lambda (condition)
                     (ctrace `(error ,(with-output-to-string (stream)
                                        #+:CCL (ccl::report-condition condition stream)
					#-:CCL (print condition stream); +++
					)))
                     (throw :ctrace-err *ctrace-top*))))
         ,@body))
     (setf *last-ctrace* *ctrace-top*)))

;;; for maximal convenience
(defmacro ct (&body body)
  `(view-ctrace 
    (with-ctracing ()
      ,@body)))

;;; enable ctracing in global context
(defun start-ctrace (&optional (level 1))
  (setf *ctrace-level* level)
  (setf *ctrace-top* (setf *ctrace-current-item* (list '(:global-ctrace)))))

(defun stop-ctrace ()
  (setf *ctrace-level* 0)
  (prog1 (setf *last-ctrace* *ctrace-top*)
    (setf *ctrace-top* nil)))

; view the trace in an editor window

;;; mostly so they can all be flushed easily
(defclass trace-window (fred-window) ()
  (:default-initargs :scratch-p t))

(defun view-ctrace (&optional (trace *ctrace-top*))
  (pprint trace))

;;; the default method computes width based on the maximum width of chars. If you 
;;; use Geneva, it actually fits LESS chars into a line. This is a crock to compensate.
(defmethod stream-line-length ((w trace-window))
  (* 2 (call-next-method)))


#|
;;; Interface to twist-down
;;; very preliminary

;;; I wish opening was smoother (it redraws entiree bottom part of screen).

(load "others;lynch lib:ll-init")
; oodles-of-utils must have alias in CCL folder
(require :twist-down)

(defun string-truncate (string length)
  (if (> (length string) length)
    (subseq string 0 length)
    string))

(defclass ctrace-twist-dialog (dialog) ())

(defmethod set-view-size :after ((view ctrace-twist-dialog) h &optional v)
  (set-view-size (elt (view-subviews view) 0) (subtract-points (view-size view) #@(22 22))))

(defun view-ctrace-twist (ctrace)
  (let ((w (make-instance 'ctrace-twist-dialog 
             :view-size #@(422 422)
             :grow-icon-p t)))
    (add-subviews w
                  (make-instance 'cl-user::twist-down
                    :view-nick-name 'ctrace
                    :view-font '("Geneva" 9 :plain)
                    :view-size #@(400 400)
                    :root ctrace
                    :node-string-function #'(lambda (node)
                                              (string-truncate
                                               (princ-to-string
                                                (if (listp (first node))
                                                  (first node)
                                                  node))
                                               255))
                    :children-function #'(lambda (node)
                                           (if (listp (first node))
                                             (rest node) 
                                             nil))))
    w))

(defmacro ctt (&body body)
  `(view-ctrace-twist
    (with-ctracing ()
      ,@body)))
|#

#|
Examples:

(defun fact (n) 
  (declare (notinline fact))            ; necessary to trace recursive functions
  (if (zerop n) 1 (* n (fact (1- n)))))

(ctrace-function 'fact)

(ct (fact 6))

;;; alternatively
(unctrace-function 'fact)

(defun fact (n)
  (with-ctrace (`(calling (fact ,n)))
    (let ((value (if (zerop n) 1 (* n (fact (1- n))))))
      (ctrace `(returned ,value))
      value)))

(ct (fact 6))

|#
