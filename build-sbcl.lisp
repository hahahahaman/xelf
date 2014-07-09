(require 'sb-posix)

;; (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

;; (ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(asdf:load-system :xelf)

(push #p"/home/dto/cypress/" asdf:*central-registry*)

(setf sb-impl::*default-external-format* :utf-8)
(asdf:oos 'asdf:load-op 'cypress)
(sb-ext:save-lisp-and-die "cypress.bin"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf xelf::*executable* t)
				      (setf xelf::*suppress-warnings* t)
				      (cypress:cypress)
				      0)
			  :executable t)

