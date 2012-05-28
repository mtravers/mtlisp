(require :asdf)

(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :defaults "mtlisp.asd"))

(asdf:operate 'asdf:load-op :mtlisp)

(provide :mtlisp)
