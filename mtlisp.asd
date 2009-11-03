(in-package :cl-user)

(asdf:defsystem :mtlisp
    :version "0.1"
    :author "Mike Travers (mt@alum.mit.edu)"
    :description "MT's Common Lisp utilities."
    :licence "Lessor Lisp General Public License"
    :serial t
    :depends-on (#-ACL :acl-compat)
    :components 
    ((:file "mt-pkg")
     (:file "pre-utils")
     (:file "mt-utils")
     (:file "closstar")
     (:file "clos-dumper")
     (:file "ctrace")
; this has disappeared
;     #+:allegro
;     (:file "acl-compat")
     (:file "lisp-unit")
     ))
