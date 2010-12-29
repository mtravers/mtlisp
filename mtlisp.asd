(in-package :cl-user)

(asdf:defsystem :mtlisp
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :description "MT's Common Lisp utilities."
    :licence "Lesser Lisp General Public License"
    :depends-on (#-:ALLEGRO :acl-compat) ;only needed in a couple of places which can be commented out if need be
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
