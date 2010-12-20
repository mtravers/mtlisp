(in-package :cl-user)

(asdf:defsystem :mtlisp
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :description "MT's Common Lisp utilities."
    :licence "Lesser Lisp General Public License"
    :serial t
    :components 
    ((:file "lisp-unit")
     (:file "mt-pkg")
     (:file "pre-utils")
     (:file "mt-utils")
     (:file "closstar")
     (:file "clos-dumper")
     (:file "ctrace")
     ))
