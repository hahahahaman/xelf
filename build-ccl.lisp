;; (push (merge-pathnames "lib/" *default-pathname-defaults*)
;;       asdf:*central-registry*)
;; (push #P"/home/dto/blocky/"
;;       asdf:*central-registry*)
;; (push #P"/home/dto/2x0ng/"
;;       asdf:*central-registry*)
(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(asdf:load-system :cypress)
(ccl:save-application "cypress.exe" 
		      :application-type :gui
		      :prepend-kernel t
		      :toplevel-function 'cypress:cypress)



