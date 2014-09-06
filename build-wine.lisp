(require 'sb-posix)

;; (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(push #p"/home/dto/skyw0r/" asdf:*central-registry*)
(push #p"/home/dto/xelf/" asdf:*central-registry*)
(ql:quickload :xelf)
(setf sb-impl::*default-external-format* :utf-8)
(asdf:oos 'asdf:load-op 'skyw0r)
(sb-ext:save-lisp-and-die "skyw0r.exe"
			  :application-type :gui
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf xelf::*executable* t)
				      (setf xelf::*suppress-warnings* t)
				      (skyw0r:skyw0r)
				      0)
			  :executable t)

