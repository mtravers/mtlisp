;;; see http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html

;;;-*- Mode: Lisp; Package: LISP-UNIT -*-

#|
Copyright (c) 2004-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#


;;; A test suite package, modelled after JUnit.
;;; Author: Chris Riesbeck
;;; 
;;; Update history:
;;;
;;; 04/07/06 added ~<...~> to remaining error output forms [CKR]
;;; 04/06/06 added ~<...~> to compact error output better [CKR]
;;; 04/06/06 fixed RUN-TESTS to get tests dynamically (bug reported
;;;          by Daniel Edward Burke) [CKR]
;;; 02/08/06 added newlines to error output [CKR]
;;; 12/30/05 renamed ASSERT-PREDICATE to ASSERT-EQUALITY [CKR]
;;; 12/29/05 added ASSERT-EQ, ASSERT-EQL, ASSERT-EQUALP [CKR]
;;; 12/22/05 recoded use-debugger to use handler-bind, added option to prompt for debugger, 
;;; 11/07/05 added *use-debugger* and assert-predicate [DFB]
;;; 09/18/05 replaced Academic Free License with MIT Licence [CKR]
;;; 08/30/05 added license notice [CKR]
;;; 06/28/05 changed RUN-TESTS to compile code at run time, not expand time [CKR]
;;; 02/21/05 removed length check from SET-EQUAL [CKR]
;;; 02/17/05 added RUN-ALL-TESTS [CKR]
;;; 01/18/05 added ASSERT-EQUAL back in [CKR]
;;; 01/17/05 much clean up, added WITH-TEST-LISTENER [CKR] 
;;; 01/15/05 replaced ASSERT-EQUAL etc. with ASSERT-TRUE and ASSERT-FALSE [CKR]
;;; 01/04/05 changed COLLECT-RESULTS to echo output on *STANDARD-OUTPuT* [CKR]
;;; 01/04/05 added optional package argument to REMOVE-ALL-TESTS [CKR]
;;; 01/04/05 changed OUTPUT-OK-P to trim spaces and returns [CKR]
;;; 01/04/05 changed OUTPUT-OK-P to not check output except when asked to [CKR]
;;; 12/03/04 merged REMOVE-TEST into REMOVE-TESTS [CKR]
;;; 12/03/04 removed ability to pass forms to RUN-TESTS [CKR]
;;; 12/03/04 refactored RUN-TESTS expansion into RUN-TEST-THUNKS [CKR]
;;; 12/02/04 changed to group tests under packages [CKR]
;;; 11/30/04 changed assertions to put expected value first, like JUnit [CKR]
;;; 11/30/04 improved error handling and summarization [CKR]
;;; 11/30/04 generalized RUN-TESTS, removed RUN-TEST [CKR]
;;; 02/27/04 fixed ASSERT-PRINTS not ignoring value [CKR]
;;; 02/07/04 fixed ASSERT-EXPANDS failure message [CKR]
;;; 02/07/04 added ASSERT-NULL, ASSERT-NOT-NULL [CKR]
;;; 01/31/04 added error handling and totalling to RUN-TESTS [CKR]
;;; 01/31/04 made RUN-TEST/RUN-TESTS macros [CKR]
;;; 01/29/04 fixed ASSERT-EXPANDS quote bug [CKR]
;;; 01/28/04 major changes from BUG-FINDER to be more like JUnit [CKR]


#|
How to use
----------

1. Read the documentation in lisp-unit.html.

2. Make a file of DEFINE-TEST's. See exercise-tests.lisp for many
examples. If you want, start your test file with (REMOVE-TESTS) to
clear any previously defined tests.

2. Load this file.

2. (use-package :lisp-unit)

3. Load your code file and your file of tests.

4. Test your code with (RUN-TESTS test-name1 test-name2 ...) -- no quotes! --
or simply (RUN-TESTS) to run all defined tests.

A summary of how many tests passed and failed will be printed,
with details on the failures.

Note: Nothing is compiled until RUN-TESTS is expanded. Redefining
functions or even macros does not require reloading any tests.

For more information, see lisp-unit.html. 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defpackage #:lisp-unit
  (:use #:common-lisp)
  (:export #:define-test #:run-all-tests #:run-tests
           #:assert-eq #:assert-eql #:assert-equal #:assert-equalp
           #:assert-error #:assert-expands #:assert-false 
           #:assert-equality #:assert-prints #:assert-true
           #:get-test-code #:get-tests
           #:remove-all-tests #:remove-tests
           #:logically-equal #:set-equal
           #:use-debugger
	   #:run-tests-with-failure-continuation
	   #:assert-runs
	   ))

(in-package #:lisp-unit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tests* (make-hash-table))

;;; Used by RUN-TESTS to collect summary statistics
(defvar *test-count* 0)
(defvar *pass-count* 0)

;;; handlers
(defvar *assertion*)
(defvar *assertion-passed* #'(lambda (form) (declare (ignore form)) (incf *pass-count*)))
(defvar *succeed*)
(defvar *error*)
(defvar *fail*)

;;; Set by RUN-TESTS for use by SHOW-FAILURE
(defvar *test-name* nil)

;;; If nil, errors in tests are caught and counted.
;;; If :ask, user is given option of entering debugger or not.
;;; If true and not :ask, debugger is entered.
(defparameter *use-debugger* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun s (thing) (princ-to-string thing))

(defvar *collection*)

(defmacro collecting (&body body)
  `(let ((*collection* nil))
     ,@body
     (nreverse *collection*)))

(defmacro collect (thing)
  `(push ,thing *collection*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINE-TEST

(defmacro define-test (name &body body)
  `(progn
     (store-test-code ',name ',body)
     ',name))

;;; ASSERT macros

(defmacro assert-eq (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'eq))

(defmacro assert-eql (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'eql))

(defmacro assert-equal (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'equal))

(defmacro assert-equalp (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'equalp))

(defmacro assert-error (condition form &rest extras)
 (expand-assert :error form (expand-error-form form)
                condition extras))

(defmacro assert-expands (&environment env expansion form &rest extras)
  (expand-assert :macro form 
                 (expand-macro-form form #+lispworks nil #-lispworks env)
                 expansion extras))

(defmacro assert-false (form &rest extras)
  (expand-assert :result form form nil extras))
 
(defmacro assert-equality (test expected form &rest extras)
 (expand-assert :equal form form expected extras :test test))

(defmacro assert-prints (output form &rest extras)
  (expand-assert :output form (expand-output-form form)
                 output extras))

(defmacro assert-true (form &rest extras)
  (expand-assert :result form form t extras))

(defun expand-assert (type form body expected extras &key (test #'eql))
  `(internal-assert 
    ,type ',form #'(lambda () ,body) #'(lambda () ,expected) ,(expand-extras extras), test))
  
(defun expand-error-form (form)
  `(handler-case ,form
     (condition (error) error)))

(defun expand-output-form (form)
  (let ((out (gensym)))
    `(let* ((,out (make-string-output-stream))
            (*standard-output* (make-broadcast-stream *standard-output* ,out)))
       ,form
       (get-output-stream-string ,out))))

(defun expand-macro-form (form env)
  `(macroexpand-1 ',form ,env))

(defun expand-extras (extras)
  `#'(lambda ()
       (list ,@(mapcan #'(lambda (form) (list `',form form)) extras))))

;;; RUN-TESTS

;;; mt addition: run tests in multiple packages with unified running total

(defmacro run-all-tests (package &rest tests)
  `(let ((*package* (find-package ',package)))
     (run-tests
      ,@(mapcar #'(lambda (test) (find-symbol (symbol-name test) package))
          tests))))

(defmacro run-tests (&rest names)
  `(let ((runner (make-instance 'test-runner))) ;hook from old to new system
     (dolist (test (or ',names (get-tests *package*)))
       (run-test runner test))))

(defun use-debugger (&optional (flag t))
  (setq *use-debugger* flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-test-code (name &optional (package (symbol-package name)))
  (let ((table (get-package-table package)))
    (unless (null table)
      (gethash name table))))

(defun get-tests (&optional (package *package*))
  (let ((l nil)
        (table (get-package-table package)))
    (cond ((null table) nil)
          (t
           (maphash #'(lambda (key val)
                        (declare (ignore val))
                        (push key l))
                    table)
           (sort l #'string< :key #'string)))))

(defun remove-tests (names &optional (package *package*))
  (let ((table (get-package-table package)))
    (unless (null table)
      (if (null names)
          (clrhash table)
        (dolist (name names)
          (remhash name table))))))

(defun remove-all-tests (&optional (package *package*))
  (if (null package)
      (clrhash *tests*)
    (remhash (find-package package) *tests*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINE-TEST support

(defun get-package-table (package &key create)
  (let ((table (gethash (find-package package) *tests*)))
    (or table
        (and create
             (setf (gethash package *tests*)
                   (make-hash-table))))))

(defun get-test-name (form)
  (if (atom form) form (cadr form)))

(defun store-test-code (name code &optional (package *package*))
  (setf (gethash name
                 (get-package-table package :create t))
        code))

;;; ASSERTION support

(defun test-passed-p (type expected actual test)
  (ecase type
    (:error
     (or (eql (car actual) (car expected))
         (typep (car actual) (car expected))))
    (:equal
     (and (<= (length expected) (length actual))
          (every test expected actual)))
    (:macro
     (equal (car actual) (car expected)))
    (:output
     (string= (string-trim '(#\newline #\return #\space) 
                           (car actual))
              (car expected)))
    (:result
     (logically-equal (car actual) (car expected)))
    ))


;;; RUN-TESTS support


(defun use-debugger-p (e)
  (and *use-debugger*
       (or (not (eql *use-debugger* :ask))
           (y-or-n-p "~A -- debug?" e))))

;;; OUTPUT support

(defun get-failure-message (type)
  (case type
    (:error "~&~@[Should have signalled ~{~S~^; ~} but saw~] ~{~S~^; ~}")
    (:macro "~&Should have expanded to ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    (:output "~&Should have printed ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    (t "~&Expected ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    ))

(defun show-failure-to-string (&rest stuff)
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (apply #'show-failure stuff))))

(defun show-failure (type msg name form expected actual extras)
  (format t "~&~@[~S: ~]~S failed: " name form)
  (format t msg expected actual)
  (format t "~{~&   ~S => ~S~}~%" extras)
  type)

(defun show-summary (name test-count pass-count &optional error-count)
  (format t "~&~A: ~S assertions passed, ~S failed~@[, ~S execution errors~]."
          name pass-count (- test-count pass-count) error-count))

(defun collect-form-values (form values)
  (mapcan #'(lambda (form-arg value)
              (if (constantp form-arg)
                  nil
                (list form-arg value)))
          (cdr form)
          values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful equality predicates for tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (LOGICALLY-EQUAL x y) => true or false
;;;   Return true if x and y both false or both true

(defun logically-equal (x y)
  (eql (not x) (not y)))

;;; (SET-EQUAL l1 l2 :test) => true or false
;;;   Return true if every element of l1 is an element of l2
;;;   and vice versa.

(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))


;;; MT addition -- this asserts that the body runs without errors.
(defmacro assert-runs (token &body body)
  `(multiple-value-bind (result err)
       (ignore-errors ,@body)
     (internal-assert :equal ,token
		      #'(lambda () (when err (s err)))
		      #'(lambda () nil)
		      #'(lambda () nil)
		      #'eql)))
		      

;;; returs sxml
;;; counts are not right (+++)
(defmacro with-output-to-junit (&body body)
  `(let ((cases nil))
     (with-test-listener (lambda (passed type name form expected actual extras test-count pass-count)
			   (push `((:|testcase| :|name| ,(string name))
				   ,@(unless passed
					     `(((:|failure|
						  :|type| ,(string type)
						  :|message| ,(show-failure-to-string type (get-failure-message type)
										      name form expected actual extras))))))
				 cases))
       ,@body)
     `((:|testsuite|  :|tests| ,(princ-to-string *test-count*)  :|failures| ,(princ-to-string (- *test-count* *pass-count*)))  ;  :|errors| ,*error-count*
       ,@cases)
     ))


(provide "lisp-unit")


#|
New arch

run-test-suite (package)
run-test-thunk (symbol)
internal-assert ...


All of these have settable hooks (need to be dynamically nested).

Alternatively, be OOP about it.  
defclass junit-runner, interactive-runner, etc, and have the above be methods.

(defmethod run-test-suite :around ((runner junit-runner) package)
  (let ...
    (call-next-method)
    (write-xml ...)))
  


|#
(defclass test-runner () 
  ((packages :initarg :packages)))

(defvar *test-runner*)

(defmethod run ((runner test-runner))
  (dolist (package (slot-value runner 'packages))
    (run-test-suite runner package)))

(defvar *test-package* nil)

(defmethod run-test-suite ((runner test-runner) package)
  (let ((*test-package* package)
	(*test-runner* runner))
    (dolist (test (get-tests package))	;get-tests is old
      (run-test runner test)
      )))

(defmethod record-test ((runner test-runner) test tests passes errors time)
  (show-summary test tests passes))

(defmethod handle-error ((runner test-runner) test error)
  (let ((*print-escape* nil))
    (format t "~&~S: Error: ~W" test error)))

(defun get-test-thunk-n (name &optional (package (or *test-package* (symbol-package name))))
  (assert (get-test-code name package) (name package)
          "No test defined for ~S in package ~S" name package)
  (coerce `(lambda () ,@(get-test-code name)) 'function))

(defvar *test*)

;;; test is symbol
;;; This should be API exported
(defmethod run-test ((runner test-runner) test)
  (let ((*test* test)
	(*test-runner* runner)
	(thunk (get-test-thunk-n test)))
    (format t "~%Running test ~A" test)
    (prog () ; ((start-time (get-internal-real-time)))
       (handler-bind 
	   ((error #'(lambda (e)
		       (handle-error runner test e)
		       (if (use-debugger-p e) e (go exit)))))
	 (unless thunk
	   (error "Test ~S not found" test))
	 (acl-compat.mp:with-timeout (30 (error "Timed out"))
	   (funcall thunk)))
       exit
					;(return (values *test-count* *pass-count* error-count (- (get-internal-real-time) start-time)))
       )))
  
;;; methodize???
(defun internal-assert (type form code-thunk expected-thunk extras test)
  (incf *test-count*)
  (let* ((expected (multiple-value-list (funcall expected-thunk)))
         (actual (multiple-value-list (funcall code-thunk)))
         (passed (test-passed-p type expected actual test)))
    (record-result-n *test-runner* *test* 
		     passed type form expected actual extras)
    (when passed
      (incf *pass-count*))
    passed))

(defmethod record-result-n ((runner test-runner) test passed type form expected actual extras)
  (unless passed
    (let ((msg (get-failure-message type)))
      (format t "~&~@[~S: ~]~S failed: " test form)
      (format t msg expected actual)
      (format t "~{~&   ~S => ~S~}~%" (funcall extras))
      type)))

#|
JUnit output format can be sucked up and displayed by Hudson.
It's pretty undocumented, but samples abound, eg:
  http://dingyichen.livejournal.com/25054.html
|#

(defclass junit-test-runner (test-runner)
  ())

;;; Returns s-xml which the caller must write out
(defmethod run :around ((runner junit-test-runner))
  `(:|testsuites|
    ,@(collecting
       (call-next-method))
    ))

(defvar *test-count*)
(defvar *fail-count*)
(defvar *error-count*)

(defmethod run-test-suite :around ((runner junit-test-runner) package)
  (let* ((*test-count* 0)
	 (*fail-count* 0)
	 (*error-count* 0)
	 (results (collecting (call-next-method))))
    (collect `((:|testsuite| :|name| ,(string package) :|tests| ,(s *test-count*) :|failures| ,(s *fail-count*) :|errors| ,(s *error-count*))
	       ,@results))))

(defvar *assertion-counter*)

; :|time| ,(format nil "~d" (/ (third result) 1000))

(defmethod run-test :around ((runner junit-test-runner) test)
  (let ((*assertion-counter* 0))
    (call-next-method)))

(defmethod record-result-n ((runner junit-test-runner) test passed type form expected actual extras)
  (incf *test-count*)
  (unless passed (incf *fail-count*))
  (collect `((:|testcase| :|name| ,(format nil "~A.~A" test (incf *assertion-counter*)))
	       ,@(unless passed 
			 `((:|failure| :|message| ,(failure-message runner test type form expected actual extras)))))))

(defmethod handle-error ((runner junit-test-runner) test error)
  (incf *error-count*)
  (let ((*print-escape* nil))
    (collect `((:|testcase| :|name| ,(format nil "~A" test))
	       ((:|error| :|message| ,(format nil "~&~S: Error: ~W" test error)))))))

(defmethod failure-message ((runner test-runner) test type form expected actual extras)
  (with-output-to-string (s)
    (format s "~&~@[~S: ~]~S failed: " test form)
    (format s (get-failure-message type) expected actual)
    (format s "~{~&   ~S => ~S~}~%" (funcall extras))))






#|
(setq runner (make-instance 'junit-test-runner :packages '(:nl)))
(run runner)

|#





