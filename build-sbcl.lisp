(require 'sb-posix)

(load #P"~/quicklisp/setup.lisp")

(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

;; (ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(push #p"~/quicklisp/local-systems/" asdf:*central-registry*)

(asdf:load-system :xelf)

(push #p"~/quicklisp/local-systems/3x0ng/" asdf:*central-registry*)

(setf sb-impl::*default-external-format* :utf-8)
(asdf:oos 'asdf:load-op '3x0ng)
(sb-ext:save-lisp-and-die "3x0ng.bin"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf xelf::*executable* t)
				      (setf xelf::*suppress-warnings* t)
				      (3x0ng:3x0ng)
				      0)
			  :executable t)

