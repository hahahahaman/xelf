;;; trees.lisp --- generic folding hierarchical list widget with
;;;                indentation and headlines, a la orgmode

;; Copyright (C) 2011, 2012, 2013  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :xelf)

;;; Trees

(defvar *tree-depth* 0)

(defmacro deeper (&rest body)
  `(let ((*tree-depth* (1+ *tree-depth*)))
     ,@body))

(defparameter *depth-gray-slope* -4)
(defparameter *depth-gray-base* 50)

(defun depth-gray (depth)
  (percent-gray (+ *depth-gray-base* (* depth *depth-gray-slope*))))

(defblock (tree phrase)
  (category :initform :structure)
  (treep :initform t)
  (always-visible :initform nil)
  (style :initform :rounded)
  (method :initform nil)
  (draw-frame :initform t)
  (indentation-width :initform (dash 2))
  (top-level :initform nil)
  (locked :initform nil)
  (temporary :initform t)
  action target (expanded :initform nil) (visible :initform t))

(defun treep (thing)
  (is-a 'tree thing))

(define-method children tree () %inputs)

(defmethod initialize ((self tree)
			       &key action target top-level inputs pinned locked method category
				    expanded (draw-frame t) label)
  (with-local-fields
      (setf %action action
	    %pinned pinned
	    %draw-frame draw-frame
	    %expanded expanded
	    %category category
	    %locked locked
	    %target target
	    %method method
	    %top-level top-level
	    %label label)
    (when inputs (setf %inputs inputs))
    ;; become the parent
    (when inputs
      (dolist (each inputs)
	(pin each)
	(set-parent each self)))))

(define-method evaluate tree ()
  (deeper (mapcar #'evaluate %inputs)))

(define-method toggle-expanded tree (&optional force)
  (with-fields (expanded locked) self
    (when (or force (not locked))
      (setf expanded (if expanded nil t))
      (invalidate-layout self))))

(define-method expandedp tree ()
  %expanded)

(define-method expand tree (&optional force)
  (when (or force (not %locked))
    (setf %expanded t)
    (invalidate-layout self)))

(define-method unexpand tree (&optional force)
  (when (or force (not %locked))
    (setf %expanded nil)
    (invalidate-layout self)))

(define-method tap tree (x y)
  (declare (ignore x y))
  (toggle-expanded self))

(define-method display-string tree ()	    
  (with-fields (action label top-level) self
    (let ((ellipsis (concatenate 'string (or label "") *null-display-string*)))
      (if action
	  (etypecase action
	    ((or string xelf:object) ellipsis)
	    (keyword (pretty-string action)))
	  (if top-level (or label "") ellipsis)))))

(define-method layout-as-string tree (string)
  (with-fields (height width) self
    (setf height (dash 1 (font-height *font*)))
    (setf width 
	  (+ (dash 2) (font-text-width string *font*)))))

(define-method layout tree ()
  (with-fields (expanded x y always-visible height inputs label width) self
    (if expanded 
	;; we're an expanded subtree. lay it out
	(progn 
	  ;; lay out the children as in a typical list
	  (layout-vertically self)
	  ;; add a little padding to the bottom
	  (incf height (dash 2))
	  ;; handle the case that the label is wider than the content.
	  (when label 
	    (setf width 
		  (max width 
		       (dash 6 (font-text-width label *font*)))))
	  ;; make all inputs equally wide
	  (dolist (each inputs)
	    (setf (field-value :width each) (- width (dash 2))))
	  ;; possibly adjust to stay onscreen 
	  (when always-visible
	    (multiple-value-bind (top left bottom right)
		(window-bounding-box (current-buffer))
	      (let ((overlap (- bottom  
				(+ y height))))
		(when (minusp overlap)
		  (incf y overlap)
		  (layout-vertically self))))))
	;; we're not expanded. just lay out for label.
	(layout-as-string self (display-string self)))))
  
(define-method header-height tree ()
  (if %label (font-height *font*) 0))

(define-method header-width tree ()
  (if %expanded
      (dash 2 (font-text-width (display-string self) *font*))
      %width))

(define-method hit tree (mouse-x mouse-y)
  (with-field-values (x y expanded inputs width height) self
    (when (within-extents mouse-x mouse-y x y (+ x width) (+ y height))
      (flet ((try (item)
	       (hit item mouse-x mouse-y)))
	(if (not expanded)
	    self
	    ;; we're expanded. is the mouse to the left of this
	    ;; tree's header tab thingy?
	    (if %top-level
		(when (and (< mouse-x (+ x (header-width self)))
			   (< (header-height self) mouse-y))
		  (some #'try inputs))
		(or (some #'try inputs) self)))))))
		
;;       (let ((hh (header-height self))
;; 	    (hw (header-width self)))
;; ;;	(message "HIT TREE")
;; 	(if (< y mouse-y (+ y hh))
;; 	    ;; we're even with the header text for this tree.
;; 	    ;; are we touching it?
;; 	    (if (< x mouse-x (+ x hw))
;; 		;; mouse is over tree title. return self to get event
;; 		;; we're in the corner (possibly over top of the text
;; 		;; of the next tree item's title in the tree bar). 
;; 		;; so, we close this tree.
;; 		(prog1 nil (unexpand self)))
;; 	    (labels ((try (it)
;; 		       (hit it mouse-x mouse-y)))
;; 	      (some #'try inputs)))))))

(define-method draw-hover tree ()
  nil)

(define-method draw-border tree (&optional (color *selection-color*)))

(define-method draw-highlight tree () 
  nil)

(define-method draw-expanded tree (&optional label)
  (with-field-values (x y width height parent inputs) self
    (let ((display-string (or label *null-display-string*))
	  (header (header-height self)))
      ;; possibly draw a background
      (when (or (null parent)
		(not (null inputs))
		(not (treep parent)))
	(draw-patch self x y (+ x width) (+ y height)))
	  ;; possibly colored by depth
	  ;; (when (plusp *tree-depth*)
	  ;;   (draw-box x y width height :color (depth-gray *tree-depth*))))
      (draw-label-string self display-string)
      ;; (draw-indicator :down-triangle-open
      ;; 		      (+ %x (font-text-width display-string)
      ;; 			 (dash 4))
      ;; 		      (+ %y (dash 2))
      ;; 		      :scale 1.6
      ;; 		      :color "gray60")
      (when %label 
	(draw-line (+ x 1) (dash 2 y header) 
		   (+ x width -1) (dash 2 y header)
		   :color (find-color self :highlight))))))
  
(define-method draw-unexpanded tree (&optional label)
;  (draw-background self)
  (let ((string (or label (display-string self))))
    (draw-label-string self string)
    (draw-indicator :down-triangle-closed 
		    (+ %x (font-text-width string)
		       (dash 4))
		    (+ %y (dash 2))
		    :scale 1.6
		    :color "yellow")))

(define-method draw-subtree tree ()
  (deeper 
   (dolist (each %inputs)
     (draw each))))

(define-method draw tree ()
  (with-fields (visible draw-frame expanded label inputs) self
    (when visible
      (with-style %style
	(if expanded 
	    (progn 
	      (when draw-frame
		(draw-expanded self label))
	      (draw-subtree self))
	    (when draw-frame (draw-unexpanded self label)))))))

;; see system.lisp for example tree menu
(defun make-tree (items &key target category (tree-prototype "XELF:TREE"))
  (labels ((xform (item)
	     (if (listp item)
		 (if (listp (first item))
		     (mapcar #'xform item)
		     (apply #'clone tree-prototype
			    :target target
			    :category category
			    (mapcar #'xform item)))
		 item)))
    (xform items)))

;;; Menus

(defblock (menu tree)
  (action :initform nil)
  (always-visible :initform t)
  (style :initform :rounded)
  (top-level :initform nil)
  (category :initform :menu)
  (tags :initform '(:menu)))

(defun menup (thing)
  (is-a 'menu thing))

(define-method siblings menu ()
  (when %parent 
    (remove-if-not #'menup (%inputs %parent))))

(defvar *menu-prototype* nil)

(defun make-menu (items &key target)
  (make-tree items 
	     :target target 
	     :category :menu
	     :tree-prototype 
	     "XELF:MENU"))

;; menu items should not accept any dragged widgets.
(define-method accept menu (arg) nil)

(define-method can-pick menu ()
  ;; allow making code blocks from menu items
  (or %method
      (or (keywordp %action) 
	  ;; disallow pulling main menus
	  (not %top-level))))

(define-method pick menu ()
  (when %target
    (if (keywordp %method)
	(let ((message
		(message-for-method %method %target)))
	  (prog1 message 
	    (with-fields (x y) message
	      (setf x %x y %y))))
	self)))

(define-method alternate-tap menu (x y)
  (when (or (null %parent)
	    (not (is-a 'menu %parent)))
    (alternate-tap%super self x y)))

(define-method tap menu (x y)
  (declare (ignore x y))
  (with-fields (action target) self
    (typecase action 
      (function (funcall action))
      (string (evaluate action)) 
      (keyword (when (has-method action target)
		 (send action (or target (symbol-value '*system*)))))
      (otherwise
       ;; we're a submenu, not an individual menu command.
       ;; first close any other open menus
       (mapc #'unexpand (siblings self))
       (toggle-expanded self)))))

(defparameter *menu-tab-color* "gray60")
(defparameter *menu-title-color* "white")

(define-method draw-expanded menu (&optional label)
  (with-field-values (action x y width height parent inputs top-level) self
    (let ((header (header-height self)))
      (if top-level
	  ;; draw the header a bit differently to avoid over-drawing
	  ;; other headers in a menu bar situation.
	  (progn 
	    (assert parent)
	    (draw-patch self x (+ 1 y)
			(+ (dash 2) x (header-width self))
			(dash 3 y header)
			:color *menu-tab-color*)
	    (draw-label-string 
	     self (or label *null-display-string*) *menu-title-color*)
	    ;; draw the rest of the tree background
	    (draw-patch self
			x (dash 2 y header)
			(dash 0 x width)
			(- (dash 1 y height) (dash 1))))
	  ;; nope, draw in the typical fashion.
	  (draw-expanded%super self label))
      ;; draw status indicator on submenus
      (when (and (not %locked) parent (menup parent))
	(draw-indicator :down-triangle-open 
			(+ %x (font-text-width (or label *null-display-string*))
			   (dash 4))
			(+ %y (dash 2))
			:scale 1.6
			:color "gray50")))))


(define-method draw-unexpanded menu (&optional label)
  (with-fields (action target top-level) self
    (let ((text (or label (display-string self))))
      (draw-label-string self 
			 text
			 (find-color self :foreground)))))
		 
(define-method draw-highlight menu ()
  (with-fields (y height expanded parent top-level) self
    (when parent
      (with-fields (x width) parent
	;; don't highlight top-level trees.
	(when (and (not expanded) (not top-level))
	  (draw-box (+ x (dash 2))
		    (+ y (dash 1)) 
		    (- width (dash 4))
		    (+ height 1)
		  :color *highlight-background-color*)
	  (draw-label-string self (display-string self)))))))

;;; trees.lisp ends here
