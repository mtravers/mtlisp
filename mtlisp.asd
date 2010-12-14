(in-package :cl-user)

(asdf:defsystem :mtlisp
    :version "0.1"
    :author "Mike Travers (mt@alum.mit.edu)"
    :description "MT's Common Lisp utilities."
    :licence "Lesser Lisp General Public License"
    :serial t
;    :depends-on (#-ACL :acl-compat)
    :components 
    ((:file "lisp-unit")
     (:file "mt-pkg")
     (:file "pre-utils")
     (:file "mt-utils")
     (:file "closstar")
     (:file "clos-dumper")
     (:file "ctrace")
     ))
