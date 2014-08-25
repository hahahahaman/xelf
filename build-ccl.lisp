;; (push (merge-pathnames "lib/" *default-pathname-defaults*)
;;       asdf:*central-registry*)
;; (push #P"/home/dto/blocky/"
;;       asdf:*central-registry*)
;; (push #P"/home/dto/2x0ng/"
;;       asdf:*central-registry*)
(load "~/quicklisp/setup.lisp")
(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(asdf:load-system :3x0ng)
(ccl:save-application "3x0ng.bin" 
		      ;; :application-type :console
		      :clear-clos-caches t
		      :prepend-kernel t
		      :toplevel-function '3x0ng:3x0ng)



