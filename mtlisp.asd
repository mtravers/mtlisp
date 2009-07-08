(in-package :cl-user)

(asdf:defsystem :mtlisp
    :version "0.1"
    :serial t
    :components 
    ((:file "mt-pkg")
     (:file "mt-utils")
     (:file "closstar")
     (:file "clos-dumper")
     (:file "ctrace")
     (:file "slog")
     (:file "lisp-unit")
     ))
