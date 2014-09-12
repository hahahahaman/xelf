;;; shell.lisp --- editor ui

;; Copyright (C) 2012, 2013  David O'Toole

;; Author: David O'Toole <dto@xelf.io>
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

;;; Message output widget

(defblock messenger :category :terminal :messages nil)

(defmethod initialize ((self messenger) &key messages)
  (with-local-fields 
    (typecase messages
      ((stringp messages)
       (setf %messages (list messages)))
      ((consp messages)
       (setf %messages messages)))))

(define-method add-message messenger (message-string)
  (assert (stringp message-string))
  (push message-string %messages))

(defparameter *messenger-columns* 80)
(defparameter *messenger-rows* 7)

(define-method get-messages messenger ()
  (or %messages *message-history*))

(define-method layout messenger ()
  (setf %height (+ (* (font-height *font*) *messenger-rows*)
		   (dash 4)))
  (let ((width 0))
    (block measuring
      (dotimes (n *messenger-rows*)
	(if (<= (length (get-messages self)) n)
	    (return-from measuring nil)
	    (setf width 
		  (max width 
		       (font-text-width 
			(nth n (get-messages self))
			*block-font*))))))
    (setf %width (+ width (dash 5)))))
			     
(defparameter *messenger-color* "gray80")

(define-method draw messenger ()
;;  (draw-background self)
  (with-fields (x y width height) self
      (let ((y0 (+ y height (- 0 (font-height *font*) (dash 2))))
	    (x0 (+ x (dash 3))))
	(dotimes (n *messenger-rows*)
	  (unless (<= (length (get-messages self)) n)
	    (draw-string (nth n (get-messages self))
			 x0 y0
			 :color *messenger-color*
			 :font *block-font*)
	    (decf y0 (font-height *font*)))))))

;;; Modeline

(defun-memo modeline-position-string (x y)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "X:~S Y:~S" x y))

(defun-memo modeline-database-string (local global)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "~S/~S objects" local global))

(define-block-macro modeline
    (:super phrase
     :fields 
     ((orientation :initform :horizontal)
      (no-background :initform t)
      (spacing :initform 4))
     :inputs (:buffer-id (new 'label :read-only t)
	      :position (new 'label :read-only t)
	      :mode (new 'label :read-only t)
	      :objects (new 'label :read-only t))))

(define-method update modeline ()
  (mapc #'pin %inputs)
  (set-value %%buffer-id (or (%buffer-name (current-buffer)) "*untitled-buffer*"))
  (set-value %%objects (modeline-database-string (hash-table-count (%objects (current-buffer)))
						 (hash-table-count *database*)))
  (set-value %%position
	     (modeline-position-string
	      (%window-x (current-buffer))
	      (%window-y (current-buffer))))
  (set-value %%mode
	     (if (current-buffer)
		 (if (field-value :paused (current-buffer))
		     "(paused)"
		     "(playing)")
		 "(empty)")))

(define-method draw modeline ()
  (with-fields (x y width height) self
    (call-next-method)
    (draw-line x y (+ x width) y :color "gray50")))

;;; Shell prompt

(defblock (shell-prompt entry)
  (result :initform nil)
  (background :initform nil))

(define-method can-pick shell-prompt () nil)

(define-method pick shell-prompt ()
  nil)

(define-method evaluate-expression shell-prompt (sexp0)
  (with-fields (result) self
    (let ((sexp (first sexp0)))
      (if (and (not (null sexp))
	       (not (keywordp sexp))
	       (symbolp sexp))
	  (setf result (new sexp))
	  (setf result (eval sexp))))))

(define-method enter shell-prompt (&optional no-clear)
  (prompt%enter self)
  (with-fields (result) self
    (when result
      (replace-output (shell) (list (make-phrase result))))))

(define-method lose-focus shell-prompt ()
  (cancel-editing self))

;;; Command forms

(defun-memo command-name-string (thing)
    (:key #'first :test 'equal :validator #'identity)
  (let ((name (etypecase thing
		(symbol (symbol-name thing))
		(string thing))))
    (coerce 
     (string-capitalize 
      (substitute #\Space #\- 
		  (string-trim " " name)))
     'simple-string)))

(defun-memo command-argument-string (thing)
    (:key #'first :test 'equal :validator #'identity)
  (concatenate 'string (command-name-string thing) ": "))

(defun arglist-input-forms (argument-forms)
  (mapcar #'(lambda (f)
	      `(make-sentence 
		(list
		 (new 'keyword :value ,(make-keyword (first f)) :read-only t)
		 (new 'expression :value ,(second f) :read-only nil))))
	  argument-forms))

(defun command-inputs (name arglist)
  `((let ((label (new 'label :read-only t :font "sans-condensed-bold-18")))
     (prog1 label (set-value label ,(command-name-string (symbol-name name)))))
    (make-paragraph (list ,@(arglist-input-forms arglist)))))

(defmacro define-command (name arglist &body body)
  `(progn
     (defun ,name (&key ,@arglist) ,@body)
     (export ',name)
     (define-block-macro ,name 
	 (:super phrase
	  :fields ((orientation :initform :vertical))
	  :inputs ,(command-inputs name arglist)))
     (define-method evaluate ,name ()
       ;; call the command function
       (apply #'funcall #',name
	      ;; with the evaluated results of
	      (mapcar #'evaluate 
		      ;; all the argument names/values
		      (mapcan #'identity 
			      (mapcar #'%inputs 
				      ;; from the dialog box
				      (%inputs (second %inputs)))))))))

;;; The shell is a command prompt and message output area.

(defparameter *minimum-shell-width* 400)
(defparameter *shell-background-color* "gray20")

(defparameter *default-command-prompt-string* "> ")

(defun make-label (string &optional font)
  (let ((label (new 'label)))
    (prog1 label
      (set-value label string)
      (set-read-only label t)
      (when font
	(setf (%font label) font)))))

(define-block-macro shell
    (:super phrase
     :fields 
     ((orientation :initform :vertical)
      (frozen :initform t)
      (category :initform :system)
      (spacing :initform 4)
      ;;
      (entry-index :initform 0)
      (target-x :initform 0)
      (target-y :initform 0))
     :inputs
     (:output (new 'phrase)
      :modeline (new 'modeline)
      :command-area (make-sentence 
		     (list
		      (make-label *default-command-prompt-string*)
		      (new 'shell-prompt))))))

(define-method drag shell (x y)
  (with-fields (target-x target-y) self
    (setf target-x (- x (window-x)))
    (setf target-y (- y (window-y)))
    (move-to self x y)))
      
(define-method layout shell ()
  (with-fields (target-x target-y) self
    (move-to self 
	     (+ target-x (window-x))
	     (+ target-y (window-y)))
  (with-fields (inputs orientation) self
    (if (null inputs)
	(layout-as-null self)
	(ecase orientation
	  (:horizontal (layout-horizontally self))
	  (:vertical (layout-vertically self)))))))


(define-method insert-output shell (item)
  (unfreeze %%output)
  (accept %%output item)
  (freeze %%output))

(define-method destroy-output shell ()
  (mapc #'destroy (%inputs %%output))
  (setf (%inputs %%output) nil))

(define-method replace-output shell (items)
  (destroy-output self)
  (dolist (item items)
    (insert-output self item)))

(define-method hit shell (x y)
  (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
    (flet ((try (thing)
	     (hit thing x y)))
      (or (some #'try (%inputs %%output))
	  self))))

(define-method tap shell (x y)
  (focus-on-entry self))

(define-method alternate-tap shell (x y) nil)

(define-method get-prompt-label shell () (first (%inputs %%command-area)))
(define-method set-prompt-label shell (label) (set-value (get-prompt-label self) label))
(define-method get-prompt shell () (second (%inputs %%command-area)))
(define-method set-prompt-line shell (line) (set-value (get-prompt self) line))
(define-method get-modeline shell () %%modeline)
(define-method get-output shell () %%output)
(define-method get-output-items shell () 
  (let ((phrase (first (%inputs (get-output self)))))
    (when phrase (%inputs phrase))))
(define-method get-dialog shell ()
  (first (%inputs (get-output self))))

(define-method get-argument-phrases shell ()
  (let ((container (second (get-output-items self))))
    (when (xelfp container) (%inputs container))))

(define-method get-entries shell ()
  (cons (get-prompt self)
	(mapcar #'second (mapcar #'%inputs (get-argument-phrases self)))))

(define-method evaluate-output shell ()
  (replace-output self (list (make-phrase (evaluate (get-dialog self))))))

(define-method current-entry shell ()
  (let ((entries (get-entries self)))
    (with-fields (entry-index) self 
      (setf entry-index (mod entry-index (length entries)))
      (nth entry-index entries))))

(define-method focus-on-entry shell ()
  (let ((entry (current-entry self)))
    (set-read-only entry nil)
    (grab-focus entry)
    (end-of-line entry)))

(define-method next-entry shell ()
  (incf %entry-index)
  (focus-on-entry self))

(define-method previous-entry shell ()
  (decf %entry-index)
  (focus-on-entry self))

(define-method draw shell ()
  (with-style :rounded
    (draw-background self))
  (mapc #'draw %inputs))

(defun shell () *shell*)
(defun shell-prompt () (get-prompt (shell)))
(defun shell-modeline () (get-modeline (shell)))
(defun shell-output () (get-output (shell)))
(defun shell-insert-output (object) (insert-output (shell) object))
(defun shell-destroy-output () (destroy-output (shell)))
(defun shell-evaluate-output ()  (evaluate-output (shell)))

;;; shell.lisp ends here
