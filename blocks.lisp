;;; blocks.lisp --- core visual language model for Xelf

;; Copyright (C) 2010, 2011, 2012, 2013 David O'Toole

;; Author: David O'Toole <dto@blocky.io>
;; Keywords: oop, languages, mouse, lisp, multimedia, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hopes that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; Please see the included files README.org and guide.org for an
;; overview.

;;; Code:

(in-package :xelf)

(defun cfloat (f) (coerce f 'single-float))

(defun-memo pretty-string (thing)
    (:key #'first :test 'equal :validator #'identity)
  (let ((name (etypecase thing
		(symbol (symbol-name thing))
		(string thing))))
    (coerce 
     (substitute #\Space #\- 
		 (string-downcase 
		  (string-trim " " name)))
     'simple-string)))

(defun-memo ugly-symbol (string)
    (:key #'first :test 'equal :validator #'identity)
  (intern 
   (string-upcase
    (substitute #\- #\Space 
		(string-trim " " string)))))

(define-prototype node (:super xnode)
  (tags :initform nil)
  (events :initform nil :documentation "Event bindings, if any. See also `bind-event'.")
  (default-events :initform nil)
  ;; visual layout
  (x :initform (cfloat 0) :accessor x :documentation "X coordinate of this block's position.")
  (y :initform (cfloat 0) :accessor y :documentation "Y coordinate of this block's position.")
  (z :initform (cfloat 0) :accessor z :documentation "Z coordinate of this block's position.")
  (heading :initform 0.0 :documentation "Heading angle of this block, in radians. See also `radian-angle'.")
  (quadtree-node :initform nil)
  ;; 
  (last-x :initform nil)
  (last-y :initform nil)
  (last-z :initform nil)
  ;; blending and image rotation
  (blend :initform :alpha)
  (opacity :initform 1.0)
  (image-heading :initform nil)
  ;; collisions
  (collision-type :initform :default)
  ;; dimensions
  (width :initform 32 :documentation "Width of the block, in GL units.")
  (height :initform 32 :documentation "Height of the block, in GL units.")
  (depth :initform 32 :documentation "Depth of block, in GL units. Currently ignored.")
  (pinned :initform nil) ;; when non-nil, do not allow dragging
  (visible :initform t)
  ;; morphic style halo
  (halo :initform nil)
  (mode :initform nil)
  (name :initform nil)
  (needs-layout :initform t)
  (label :initform nil)
  (tasks :initform nil)
  (image :initform nil :documentation "Name of texture to be displayed, if any."))

(defmethod proper-name ((self node))
  (pretty-string (class-name (class-of self))))

(defmethod initialize ((self node) &key))

;;; Defining blocks

(defmacro defblock (spec &body args)
  "Define a new block.
The first argument SPEC is either a
symbol naming the new block, or a list of the form
 (SYMBOL . PROPERTIES) Where SYMBOL is similarly a name symbol but
PROPERTIES is a keyword property list whose valid keys
are :SUPER (specifying which prototype the newly defined block will
inherit behavior from) and :DOCUMENTATION (a documentation string.)
The remaining arguments ARGS are field specifiers, each of which is
either a symbol naming the field, or a list of the form (SYMBOL
. PROPERTIES) with :INITFORM and :DOCUMENTATION as valid keys."
  (let ((name0 nil)
	(super0 'xelf:node))
    (etypecase spec
      (symbol (setf name0 spec))
      (list (destructuring-bind (name super) spec
	      (setf name0 name)
	      (when super (setf super0 super)))))
    `(define-prototype ,name0 
	 (:super ,super0)
       ,@(if (keywordp (first args))
	  (plist-to-descriptors args)
	  args))))

(defmethod duplicate-safely ((thing node))
  (let ((dupe (duplicate thing)))
    (prog1 (find-object dupe)
      (setf (field-value 'quadtree-node dupe) nil))))

(defparameter *block-categories*
  '(:system :motion :event :message :looks :sound :structure :data :button
    :expression :menu :hover :control :parameters :comment :sensing :operators :variables)
  "List of keywords used to group blocks into different functionality
areas.")

(defun new (class &rest args)
  (apply #'make-instance class args))

(define-method create nil ()
  (new (find-super (find-object self))))

(define-method forward-message nil (method args)
  (apply #'send method self args))

(define-method set-field nil (field value)
  (setf (field-value field (evaluate self)) value))

(define-method get-field nil (field)
  (field-value field (evaluate self)))

;;; Adding nodes to the simulation

(define-method start nil ()
  "Add this block to the simulation so that it receives update events."
  (unless (find self *blocks* :test 'eq :key #'find-object)
    (setf *blocks* (adjoin (find-uuid self) *blocks* :test 'equal))))

(define-method start-alone nil ()
  (setf *blocks* (list self)))

(define-method stop nil ()
  "Remove this block from the simulation so that it stops getting update
events."
  (setf *blocks* (delete (find-uuid self) *blocks* :test #'equal)))

;;; Read-only status

(define-method toggle-read-only nil ()
  (setf %read-only (if %read-only nil t)))

(define-method read-only-p nil () %read-only)

(define-method set-read-only nil (&optional (read-only t))
  (setf %read-only read-only))

(define-method child-updated nil (child))

;;; Defining composite blocks more simply

;(declaim (inline input-block))

(defun input-block (object input-name)
  (nth (position input-name 
		 (%input-names object))
       (%inputs object)))

(defmacro define-block-macro (name 
     (&key (super 'node) fields documentation inputs)
     &body body)
  "Define a new block called NAME according to the given options.

The argument SUPER should be the name (a symbol or string) of the base
prototype to inherit traits (data and behavior) from. The default is
`node' so that if you don't specify a SUPER argument, you still
inherit all the inbuilt behaviors of nodes.

The argument FIELDS should be a list of field descriptors, the same as
would be given to `define-prototype'.

The INPUTS argument is a list of forms evaluated to produce argument
blocks. 

DOCUMENTATION is an optional documentation string for the entire
macro.

The BODY forms are evaluated when the resulting block is evaluated;
they operate by invoking `evaluate' in various ways on the INPUTS.

The method `recompile' emits Lisp code that has the same result as
invoking `evaluate', but with zero or more blocks in the entire visual
expression subtree being replaced by (possibly shorter and more
efficient) 'plain' Lisp code. This is trivially true for the default
implementation of `recompile', which emits a statement that just
invokes `evaluate' when evaluated. When subsequently redefining the
`recompile' method on a block-macro, the 'equivalence' between the
results of invoking `recompile' and invoking `evaluate' depends solely
on the implementor, who can write a `recompile' method which operates
by invoking `recompile' in various ways on the macro-block's
`%inputs', and emitting Lisp code forms using those compiled code
streams as a basis. 
"
  (let ((input-names (remove-if-not #'keywordp inputs)))
    `(progn 
       ;; define input accessor functions
       ,@(mapcar #'make-input-accessor-defun-forms input-names)
       (defblock (,name ,super) 
	 (label :initform ,(pretty-string name))
	 (input-names :initform ',input-names)
	 ,@fields)
       (defmethod initialize ((self ,name) &key inputs)
	 (with-local-fields
	     (setf %inputs (list ,@(remove-if #'keywordp inputs)))
	   (update-parent-links self)
	   (mapc #'pin %inputs)
	   ,@body))
       (define-method recompile ,name () `(evaluate self)))))

;;; Block lifecycle

(defmethod initialize-instance :after ((self node) &key)
  (setf (uuid self) (make-uuid))
  (register-uuid self))

(define-method after-revive node () nil)

(defun destroy-maybe (x)
  (when (xelfp x) (destroy (find-object x))))

(define-method destroy nil ()
  "Throw away this block."
  (mapc #'destroy-maybe %tasks)
  (destroy-events self)
  (remove-thing-maybe (current-buffer) self)
  (when %quadtree-node 
    (quadtree-delete self %quadtree-node))
  (setf %quadtree-node nil)
  (let (uuid (find-uuid self))
    (remove-object-from-database self)
    (prog1 t
      (assert (not (find-object uuid :no-error))))))

(define-method dismiss nil ()
  ;; (if (windowp %parent)
  ;;     (dismiss %parent)
      (destroy self))

(define-method exit nil ()
  (remove-object *buffer* self))

(define-method make-duplicate nil ()
  (duplicate self))

(define-method make-clone nil ()
  (find-uuid (clone (find-super self))))

(define-method register-uuid nil ()
  (add-object-to-database self))

;;; Block tags, used for categorizing blocks

(define-method has-tag nil (tag)
  "Return non-nil if this block has the specified TAG.

Blocks may be marked with tags that influence their processing by the
engine. The field `%tags' is a set of keyword symbols; if a symbol
`:foo' is in the list, then the block is in the tag category `:foo'.
"
  (member tag %tags))

(define-method add-tag nil (tag)
  "Add the specified TAG symbol to this block."
  (pushnew tag %tags))

(define-method remove-tag nil (tag) 
  "Remove the specified TAG symbol from this block."
  (setf %tags (remove tag %tags)))

;;; Serialization hooks

(define-method before-serialize nil ())

(define-method after-deserialize nil ()
  "Prepare a deserialized block for running."
  (bind-any-default-events self)
  (register-uuid self))

;;; Expression structure (blocks composed into trees)

(define-method adopt nil (child)
  (when (get-parent child)
    (unplug-from-parent child))
  (set-parent child self))

(define-method update-parent-links nil ()
  (dolist (each %inputs)
    (set-parent (find-object each) self)))

(define-method can-accept nil () nil)

(defmethod will-accept ((container node) (item node)) nil)

(define-method accept nil (other-block)
  "Try to accept OTHER-BLOCK as a drag-and-dropped input. Return
non-nil to indicate that the block was accepted, nil otherwise."
  nil)

(defvar *buffers* nil
  "When non-nil, the UUID of the current buffer object.")

(define-method contains nil (block)
  (block finding
    (dolist (this %inputs)
      (when (object-eq block this)
	(return-from finding this)))))

  ;; (find (find-object block)
  ;; 	%inputs
  ;; 	:test 'eq
  ;; 	:key #'find-object))

(define-method input-position nil (input)
  (assert (not (null input)))
  (position (find-uuid input) %inputs :key #'find-uuid :test 'equal))

(defun input (self name)
  (with-fields (inputs) self
    (assert (not (null inputs)))
    (nth (input-position self name) inputs)))

(defun (setf input) (self name node)
  (with-fields (inputs) self
    (assert (not (null inputs)))
    (set-parent node self)
    (setf (nth (input-position self name) inputs)
	  ;; store the real link
  	  (find-object node))))

(define-method position-within-parent nil ()
  (input-position %parent self))

(define-method set-parent nil (parent)
  "Store a UUID link to the enclosing block PARENT."
  (assert (not (null parent)))
  (assert (valid-connection-p parent self))
  (setf %parent (when parent 
		  ;; always store uuid to prevent circularity
		  (find-uuid parent))))
	       
(define-method get-parent nil ()
  %parent)

(define-method find-parent nil ()
  (when %parent (find-uuid %parent)))

(defun valid-connection-p (sink source)
  (assert (or sink source))
  ;; make sure source is not actually sink's parent somewhere
  (block checking
    (prog1 t
      (let ((pointer sink))
	(loop while pointer do
	  (if (eq (find-object pointer)
		  (find-object source))
	      (return-from checking nil)
	      (setf pointer (find-parent (find-object pointer)))))))))

(define-method update-result-lists nil ()
  (let ((len (length %inputs)))
    (setf %input-widths (make-list len :initial-element 0))
    (setf %results (make-list len))))

(define-method delete-input nil (block)
  (with-fields (inputs) self
    (prog1 t
      (assert (contains self block))
      (setf inputs (remove block inputs
			   :key #'find-object
			   :test 'eq))
      (assert (not (contains self block))))))

(define-method default-inputs nil ()
  nil)

(define-method this-position nil ()
  (with-fields (parent) self
    (when parent
      (input-position parent self))))

(define-method plug nil (thing n)
  "Connect the block THING as the value of the Nth input."
  (set-parent thing self)
  (setf (input self n) thing))

(define-method after-unplug-hook nil (parent))
  ;; (setf %parent nil)
  ;; (add-object (current-buffer) self))

(define-method after-release-hook nil ())

(define-method unplug nil (input)
  "Disconnect the block INPUT from this node."
  (with-fields (inputs parent) self
    (assert (contains self input))
    (prog1 input
      (setf inputs 
	    (delete input inputs 
		    :test 'eq :key #'find-object))
      (after-unplug-hook input self))))

(define-method unplug-from-parent nil ()
  (when %parent
    (prog1 t
      (with-fields (parent) self
	(assert (not (null parent)))
	(assert (contains (find-object parent) self))
	(unplug (find-object parent) self)
;	(assert (not (contains parent self)))
	(setf parent nil)))))

(define-method drop nil (new-block &optional (dx 0) (dy 0) (dz 1))
  "Add a new object to the current buffer at the current position.
Optionally provide an x-offset DX and a y-offset DY. The optional
z-offset DZ defaults to 1, which stacks the object on top of Self.
See also `drop-at'."
  (add-object (current-buffer) new-block (+ %x dx) (+ %y dy) (+ %z dz)))

(define-method drop-at nil (new-block x y &optional z)
  "Add the NEW-BLOCK to the current buffer at the location X,Y."
  (assert (and (numberp x) (numberp y)))
  (add-object (current-buffer) new-block x y z))

(defmethod clear-buffer-data ((self node))
  (clear-saved-location self)
  (setf (field-value 'quadtree-node self) nil))

;;; Defining input events for blocks

;; see also definition of "task" blocks below.

(define-method initialize-events-table-maybe nil (&optional force)
  (when (null (field-value 'events self))
    (setf (field-value 'events self) (make-hash-table :test 'equal))))

(define-method bind-event-to-task nil (event-name modifiers task)
  "Bind the described event to invoke the action of the TASK.
EVENT-NAME is either a keyword symbol identifying the keyboard key, or
a string giving the Unicode character to be bound. MODIFIERS is a list
of keywords like :control, :alt, and so on."
  (assert (find-object task))
  (initialize-events-table-maybe self)
  (let ((event (make-event event-name modifiers)))
    (setf (gethash event %events)
	  task)))

(define-method unbind-event nil (event-name modifiers)
  "Remove the described event binding."
  (remhash (normalize-event (cons event-name modifiers))
	   %events))

(define-method handle-event nil (event)
  "Look up and invoke the block task (if any) bound to
EVENT. Return the task if a binding was found, nil otherwise. The
second value returned is the return value of the evaluated task (if
any)."
  (with-fields (events) self
    (when events
      (let ((task 
	      ;; unpack event
	      (destructuring-bind (head &rest modifiers) event
		;; if head is a cons, check for symbol binding first,
		;; then for unicode binding. we do this because we'll
		;; often want to bind keys like ENTER or BACKSPACE
		;; regardless of their Unicode interpretation 
		(if (consp head)
		    (or (gethash (cons (car head) ;; try symbol
				       modifiers)
				 events)
			(gethash (cons (cdr head) ;; try unicode
				       modifiers)
				 events))
		    ;; it's not a cons. 
		    ;; just search event as-is
		    (gethash event events)))))
	(if task
	    (values (find-object task)
		     (evaluate (find-object task)))
	    (values nil nil))))))

(define-method handle-text-event nil (event)
  "Look up events as with `handle-event', but insert
unhandled/unmodified keypresses as Unicode characters via the `insert'
function."
  (unless (joystick-event-p event)
    (with-fields (events) self
      (destructuring-bind (key . unicode) (first event)
	(when (or (handle-event self event)
		  ;; treat non-alt-control Unicode characters as self-inserting
		  (when 
		      (and (not (eq :return key))
			   unicode 
			   (not (member :alt (rest event)))
			   (not (member :control (rest event))))
		    (prog1 t
		      (send :insert-string self unicode))))
	  (prog1 t (invalidate-layout self)))))))
  
(defun bind-event-to-method (block event-name modifiers method-name)
  "Arrange for METHOD-NAME to be sent as a message to this object
whenever the event (EVENT-NAME . MODIFIERS) is received."
  (destructuring-bind (key . mods) 
      (make-event event-name modifiers)
    (bind-event-to-task block 
			   key
			   mods
			   (new 'task :method-name method-name :target block))))

(define-method bind-event nil (event binding)
  "Bind the EVENT to invoke the action specified in BINDING.
EVENT is a list of the form:

       (NAME modifiers...)

NAME is either a keyword symbol identifying the keyboard key, or a
string giving the Unicode character to be bound. MODIFIERS is a list
of keywords like :control, :alt, and so on.

Examples:
  
  (bind-event self '(:up) :move-up)
  (bind-event self '(:down) :move-down)
  (bind-event self '(:q :control) :quit)
  (bind-event self '(:escape :shift) :menu)

See `keys.lisp' for the full table of key and modifier symbols.

"  (destructuring-bind (name &rest modifiers) event
    (etypecase binding
      (symbol (bind-event-to-method self name modifiers binding))
      (list 
       ;; create a method call 
       (let ((task (new 'task
			(first binding)
			self
			:arguments (rest binding))))
	 (bind-event-to-task self name modifiers task))))))

(define-method bind-any-default-events nil ()
  (with-fields (default-events) self
    (when default-events
      (initialize-events-table-maybe self :force)
      (dolist (entry default-events)
	(apply #'bind-event self entry)))))

(define-method destroy-events nil ()
  (when %events
    (loop for event being the hash-values of %events do 
      (destroy-maybe event))))

(defun bind-event-to-text-insertion (self key mods text)
  (bind-event-to-task self key mods 
			 (new 'task :insert-string self (list text))))
    
(define-method insert nil (&optional x y z)
  (add-node (current-buffer) self x y z))

(define-method insert-string nil (string)
  (declare (ignore string))
  nil)

(defvar *lowercase-alpha-characters* "abcdefghijklmnopqrstuvwxyz")
(defvar *uppercase-alpha-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *numeric-characters* "0123456789")
(defvar *graphic-characters* "`~!@#$%^&*()_-+={[}]|\:;\"'<,>.?/")

(defparameter *text-qwerty-keybindings*
  '((:f (:control) :forward-char)
    (:b (:control) :backward-char)
    (:n (:alt) :forward-history)
    (:p (:alt) :backward-history)
    (:right nil :forward-char)
    (:left nil :backward-char)
    (:backspace nil :backward-delete-char)
    (:delete nil :delete-char)
    (:d (:control) :delete-char)
    (:a (:control) :beginning-of-line)
    (:e (:control) :end-of-line)
    (:tab nil :tab)
    (:tab (:shift) :backtab)
    (:return nil :enter)
    (:return (:control) :execute)
    ;; (:return (:control) :evaluate-here)
    ;; (:delete (:alt) :delete-word)
    ;; (:d (:alt) :delete-word)
    (:x (:control) :exit)
    (:g (:control) :exit)
    (:escape nil :exit)))

(defparameter *arrow-key-text-navigation-keybindings*
  '(
    (:up nil :previous-line)
    (:down nil :next-line)
    (:left nil :backward-char)
    (:right nil :forward-char)
    (:up (:alt) :previous-line)
    (:down (:alt) :next-line)
    (:left (:alt) :backward-word)
    (:right (:alt) :forward-word)
    (:home nil :beginning-of-line)
    (:end nil :end-of-line)))

(defun keybinding-event (binding)
  (cons (first binding)
	(second binding)))

(defun keybinding-action (binding)
  (nthcdr 2 binding))

(define-method install-keybindings nil (keybindings)
  (dolist (binding keybindings)
    (bind-event self 
		(keybinding-event binding)
		(keybinding-action binding))))
        
(define-method install-text-keybindings nil (&optional (keybindings *text-qwerty-keybindings*))
  ;; install UI keys that will vary by locale
  (with-fields (events) self
    (setf events (make-hash-table :test 'equal))
    (dolist (binding keybindings)
      (destructuring-bind (key mods result) binding
	(etypecase result
	  (keyword (bind-event-to-method self key mods result))
	  (string (bind-event-to-text-insertion self key mods result)))))))

;;; Pointer events (see also buffers.lisp)

(define-method select nil () nil)

(define-method tap nil (x y))

(define-method alternate-tap nil (x y)
  (when (shell-open-p) 
    (toggle-halo self)))

(define-method scroll-tap nil (x y)
  (declare (ignore x y))
  nil)

(define-method scroll-up nil ())

(define-method scroll-down nil ())

(define-method scroll-left nil ())

(define-method scroll-right nil ())

(define-method handle-point-motion nil (x y)
  (declare (ignore x y)))

(define-method press nil (x y &optional button)
  (declare (ignore x y button)))

(define-method release nil (x y &optional button)
  (declare (ignore x y button)))

(define-method can-pick nil () 
  (not %pinned))

(define-method pick nil ()
  (with-fields (pinned parent) self
    (if (not pinned)
	self
	(when (and parent
		   (can-pick parent))
	  (pick parent)))))

(define-method topmost nil ()
  (let ((this self)
	(next nil))
    (block searching
      (loop while this do
	(setf next (%parent this))
	(when (or (null next)
		  (is-a 'buffer next))
	  (return-from searching this))
	(setf this next)))))

(defmethod after-add-hook ((self node)) nil)

(defmethod after-drag-hook ((self node)) nil)

;;; Focus events (see also buffers.lisp)

(define-method focus nil () (setf %focused-p t))

(define-method lose-focus nil () (setf %focused-p nil))

(define-method grab-focus nil () 
  (send :focus-on (current-buffer) self :clear-selection nil))

(define-method pick-focus nil () self)

;;; Squeak-style pop-up halo with action handles

;; see also halo.lisp

(define-method make-halo nil ()
  (when (null %halo)
    (setf %halo (new 'halo :target self))
    (add-block (current-buffer) %halo)))

(define-method destroy-halo nil ()
  (when (xelfp %halo)
    (destroy %halo))
  (setf %halo nil))

(define-method toggle-halo nil (&optional force)
  (if %halo
      (destroy-halo self)
      (when (or force (not %pinned))
	(make-halo self))))

(define-method align-to-pixels nil ()
  (setf %x (truncate %x))
  (setf %y (truncate %y)))

(define-method drag nil (x y)
  (move-to self x y))

(define-method as-drag nil (x y)
  self)

(define-method as-target nil () self)

(define-method can-escape nil ()
  t)

;;; Tasks and updating

;; See also definition of "task" blocks below.

(define-method add-task nil (task)
  (assert (xelfp task))
  (pushnew (find-uuid task) %tasks :test 'equal))

(define-method remove-task nil (task)
  (destroy-maybe task)
  (setf %tasks (delete task %tasks :test 'equal)))
 
(define-method run nil ()) ;; stub for with-turtle

(define-method run-tasks nil ()
  ;; don't run tasks on objects that got deleted during UPDATE
  (when %quadtree-node
    (dolist (task %tasks)
      (let ((t2 (find-object task :no-error)))
	(when (xelfp t2)
	  (unless (running (find-object t2))
	    (remove-task self (find-object t2))))))))

(define-method update nil ())
   
;;; Block movement

(define-method save-location nil ()
  (setf %last-x %x
	%last-y %y
	%last-z %z))

(defmethod clear-saved-location ((self node))
  (with-fields (last-x last-y last-z) self
    (setf last-x nil last-y nil last-z nil)))

(define-method restore-location nil ()
  ;; is there a location to restore? 
  (when %last-x
    (quadtree-delete-maybe self)
    (setf %x %last-x
	  %y %last-y
	  %z %last-z)
    (quadtree-insert-maybe self)))

(define-method set-location nil (x y)
  (setf %x x %y y))

(defmethod move-to ((node node) x0 y0 &optional z0)
  (quadtree-delete-maybe node)
  (with-slots (x y z) node
    (setf x x0 y y0)
    (when z (setf z z0)))
  (quadtree-insert-maybe node))

(define-method raise nil (distance)
  (incf %z distance))

(define-method lower nil (distance)
  (decf %z distance))
  
(define-method move-to-depth nil (depth)
  (setf %z (cfloat depth)))

(define-method move-toward node (direction &optional (steps 1))
    "Move this block STEPS steps in the direction given by KEYWORD.
The KEYWORD must be one of:

 :up :down :left :right :upright :upleft :downleft :downright
"
  (with-fields (x y) self
    (multiple-value-bind (x0 y0)
	(step-in-direction x y (or direction :up) (or steps 5))
      (move-to self x0 y0))))

(defun radian-angle (degrees)
  "Convert DEGREES to radians."
  (* degrees (cfloat (/ pi 180))))

(defun heading-degrees (radians)
  (* radians (cfloat (/ 180 pi))))

(define-method turn-left nil (radians)
  "Turn this object's heading to the left by RADIANS."
  (decf %heading radians))

(define-method turn-right nil (radians)
  "Turn this object's heading to the right by RADIANS."
  (incf %heading radians))

(defun step-coordinates (x y heading &optional (distance 1))
  (values (+ x (* distance (cos heading)))
	  (+ y (* distance (sin heading)))))

(define-method step-toward-heading nil (heading &optional (distance 1))
  "Return as values the X,Y coordinate of the point DISTANCE units
away from this object, in the angle HEADING."
  (multiple-value-bind (x y) (center-point self)
    (step-coordinates x y heading distance)))

(define-method move nil (heading distance)
  "Move this object DISTANCE units toward the angle HEADING."
  (multiple-value-bind (x0 y0) (step-coordinates %x %y heading distance)
    (move-to self x0 y0)))

(define-method forward nil (distance)
  "Move this object DISTANCE units toward its current heading."
  (move self %heading distance))

(define-method backward nil (distance)
  "Move this object DISTANCE units away from its current heading."
  (move self (- (* 2 pi) %heading) distance))

(define-method heading-to-thing2 nil (thing)
  "Compute the heading angle from this object to the other object THING."
  (multiple-value-bind (x1 y1) (center-point thing)
    (multiple-value-bind (x0 y0) (center-point self)
      (find-heading x0 y0 x1 y1))))

(define-method heading-to-thing nil (thing)
  (multiple-value-bind (x0 y0) (center-point thing)
    (find-heading %x %y x0 y0)))

(define-method heading-to-cursor nil ()
  "Compute the heading angle from this object to the cursor."
  (heading-to-thing self (get-cursor *buffer*)))

;;; Show methods in Emacs Glass

(define-method show-method nil (method)
  (let ((sym (definition method (find-object self))))
    (assert (symbolp sym))
    (let ((name (string-upcase 
		 (format nil "~A::~A"
			 (package-name (symbol-package sym))
			 (symbol-name sym)))))
      (eval-in-emacs `(glass-show-definition ,name)))))

(define-method show-definition nil ()
  (let ((name 
	  (concatenate 'string 
		       (package-name *package*)
		       "::"
		       (prototype-variable-name 
			(find-super-prototype-name self)))))
    (message "SHOWING DEF ON CL SIDE: ~S" name)
    (eval-in-emacs `(glass-show-definition ,name))))

;;; Visibility

(define-method show nil ()
  (setf %visible t))

(define-method hide nil ()
  (setf %visible nil))

(define-method toggle-visibility nil ()
  (if %visible
      (hide self)
      (show self)))

(define-method visiblep nil ()
  %visible)

;;; Evaluation and recompilation: compiling node diagrams into equivalent sexps

(define-method evaluate-inputs nil ()
  "Evaluate all nodes in %INPUTS from left-to-right. Results are
placed in corresponding positions of %RESULTS. Override this method
when defining new nodes if you don't want to evaluate all the inputs
all the time."
  (with-fields (inputs results) self
    (let ((arity (length inputs)))
      (when (< (length results) arity)
	(setf results (make-list arity)))
      (dotimes (n arity)
	(when (nth n inputs)
	  (setf (nth n results)
		(evaluate (nth n inputs))))))
    results))

(define-method evaluate nil () self)

(define-method evaluate-here nil ())

(define-method recompile nil ()
  (mapcar #'recompile %inputs))

(defun count-tree (tree)
  "Return the number of nodes enclosed in this node, including the
current node. Used for taking a count of all the nodes in a tree."
  (cond ((null tree) 0)
	;; without inputs, just count the root
	((null (field-value 'inputs tree)) 1)
	;; otherwise, sum up the counts of the children (if any)
	(t (apply #'+ 1 
		  (mapcar #'count-tree 
			  (field-value 'inputs tree))))))

;;; Drawing nodes with complete theme customization

;; Very important for individuals with colorblindness.

(defparameter *background-color* "white"
  "The default background color of the XELF user interface.")

(defparameter *socket-color* "gray80"
  "The default background color of node sockets.")

(defparameter *node-font* "sans-11"
  "Name of the font used in drawing node labels and input data.")

(defparameter *node-bold* "sans-bold-11")

(defmacro with-font (font &rest body)
  "Evaluate forms in BODY with FONT as the current font."
  `(let ((*font* ,font))
     ,@body))

(defparameter *sans* "sans-11"
  "Name of the default sans-serif font.")

(defparameter *serif* "serif-11"
  "Name of the default serif font.")

(defparameter *monospace* "sans-mono-12"
  "Name of the default monospace (fixed-width) font.")

(defvar *dash* 3
  "Size in pseudo-pixels of (roughly) the size of the space between
two words. This is used as a unit for various layout operations.
See also `*style'.")

(defun dash (&optional (n 1) &rest terms)
  "Return the number of pixels in N dashes. Add any remaining
arguments. Uses `*dash*' which may be configured by `*style*'."
  (apply #'+ (* n *dash*) terms))

(defvar *text-baseline* nil 
"Screen Y-coordinate for text baseline.
This is used to override layout-determined baselines in cases where
you want to align a group of text items across layouts.")

(defparameter *node-colors*
  '(:motion "cornflower blue"
    :system "gray40"
    :expression "gray60"
    :button "orange"
    :terminal "gray25"
    :event "gray80"
    :menu "white"
    :hover "red"
    :socket "gray60"
    :data "gray40"
    :structure "gray50"
    :comment "khaki1"
    :looks "purple"
    :sound "orchid"
    :message "orange"
    :parameters "YellowGreen"
    :control "orange1"
    :variables "maroon3"
    :fields "MediumOrchid"
    :operators "OliveDrab3"
    :sensing "DeepSkyBlue3")
  "X11 color names of the different node categories.")

(defparameter *node-highlight-colors*
  '(:motion "sky blue"
    :system "gray60"
    :hover "dark orange"
    :button "gold"
    :expression "gray90"
    :event "gray90"
    :menu "gray80"
    :fields "orchid"
    :terminal "gray30"
    :comment "gray88"
    :parameters "GreenYellow"
    :looks "medium orchid"
    :socket "gray80"
    :data "gray60"
    :structure "gray60"
    :sound "plum"
    :message "gold"
    :control "gold"
    :variables "maroon2"
    :operators "OliveDrab1"
    :sensing "DeepSkyBlue2")
  "X11 color names of highlights on the different node categories.")

(defparameter *node-shadow-colors*
  '(:motion "royal blue"
    :system "gray42"
    :event "gray70"
    :socket "gray90"
    :data "gray25"
    :expression "gray50"
    :fields "DarkOrchid"
    :menu "gray80"
    :terminal "gray21"
    :button "DarkOrange"
    :parameters "OliveDrab"
    :structure "gray35"
    :comment "gray70"
    :hover "orange red"
    :looks "dark magenta"
    :sound "violet red"
    :message "DarkOrange"
    :control "dark orange"
    :variables "maroon4"
    :operators "OliveDrab4"
    :sensing "steel blue")
  "X11 color names of shadows on the different node categories.")

(defparameter *node-foreground-colors*
  '(:motion "white"
    :system "white"
    :button "yellow"
    :event "gray40"
    :expression "white"
    :terminal "white"
    :comment "gray20"
    :fields "white"
    :socket "gray20"
    :hover "yellow"
    :parameters "white"
    :data "white"
    :menu "gray60"
    :structure "white"
    :message "white"
    :looks "white"
    :sound "white"
    :control "white"
    :variables "white"
    :operators "white"
    :sensing "white")
  "X11 color names of the text used for different node categories.")

(define-method find-color nil (&optional (part :background))
  "Return the X11 color name of this node's PART as a string.
If PART is provided, return the color for the corresponding
part (:BACKGROUND, :SHADOW, :FOREGROUND, or :HIGHLIGHT) of this
category of node."
  (let* ((colors (ecase part
		  (:background *node-colors*)
		  (:highlight *node-highlight-colors*)
		  (:shadow *node-shadow-colors*)
		  (:foreground *node-foreground-colors*)))
	 (category (if (keywordp %category) %category :system))
	 (result (getf colors category)))
      (prog1 result 
	(assert category)
	(assert result))))

(defparameter *selection-color* "red" 
  "Name of the color used for highlighting objects in the selection.")

(defparameter *styles* '((:rounded :dash 3)
			 (:flat :dash 1))
  "Graphical style parameters for node drawing.")

(defvar *style* :flat "The default style setting; must be a keyword.")

(defmacro with-style (style &rest body)
  "Evaluate the forms in BODY with `*style*' bound to STYLE."
  (let ((st (gensym)))
  `(let* ((,st ,style)
	  (*style* ,st)
	  (*dash* (or (getf *styles* ,st)
		      *dash*)))
     ,@body)))

(defmacro with-node-drawing (&body body)
  "Run BODY forms with drawing primitives.
The primitives are CIRCLE, DISC, LINE, BOX, and TEXT. These are used
in subsequent functions as the basis of drawing nested diagrams of
nodes."
  `(let* ((foreground (find-color self :foreground))
	  (background (find-color self :background))
	  (highlight (find-color self :highlight))
	  (shadow (find-color self :shadow))
	  (radius (+ 6 *dash*))
	  (diameter (* 2 radius)))
     (labels ((circle (x y &optional color)
		(draw-circle x y radius
			     :color (or color background)
			     :blend :alpha))
	      (disc (x y &optional color)
		(draw-solid-circle x y radius
				   :color (or color background)
				   :blend :alpha))
	      (line (x0 y0 x1 y1 &optional color)
		(draw-line x0 y0 x1 y1
			   :color (or color background)))
	      (box (x y r b &optional color)
		(draw-box x y (- r x) (- b y)
			  :color (or color background)))
	      (text (x y string &optional color2)
		(draw-string string x 
			     (or *text-baseline* y)
			     :color (or color2 foreground)
			     :font *font*)))
       ,@body)))

(define-method draw-rounded-patch nil (x0 y0 x1 y1
				    &key depressed dark socket color)
  "Draw a standard XELF node notation patch with rounded corners.
Places the top left corner at (X0 Y0), bottom right at (X1 Y1). If
DEPRESSED is non-nil, draw an indentation; otherwise a raised area is
drawn. If DARK is non-nil, paint a darker region. If SOCKET is
non-nil, cut a hole in the node where the background shows
through. If COLOR is non-nil, its value will override all other
arguments."
  (with-node-drawing 
    (let ((bevel (or color (if depressed shadow highlight)))
	  (chisel (or color (if depressed highlight shadow)))
	  (fill (or color (if socket
			      *socket-color*
			      (if dark background background)))))
;      (disc (- x0 10) (- y0 10) fill) ;; a circle by itself
      ;; y1 x1
      (disc (- x1 radius ) (- y1 radius ) fill)
      (circle (- x1 radius ) (- y1 radius ) chisel) ;; chisel
      ;; y1 left
      (disc (+ x0 radius ) (- y1 radius ) fill)
      (circle (+ x0 radius ) (- y1 radius) chisel)
      ;; top left
      (disc (+ x0 radius ) (+ y0 radius) fill)
      (circle (+ x0 radius ) (+ y0 radius) bevel) ;;bevel
      ;; top x1
      (disc (- x1 radius ) (+ y0 radius ) fill)
      (circle (- x1 radius ) (+ y0 radius ) chisel) ;; chisel
      ;; y1 (bottom) 
      (box (+ x0 radius) (- y1 diameter)
	   (- x1 radius 1) y1
	   fill)
      (line (+ x0 radius -2) (1- y1)
	    (- x1 radius 1) y1 chisel)
      ;; top
      (box (+ x0 radius) y0
	   (- x1 radius) (+ y0 diameter)
	   fill)
      (line (+ x0 radius) (+ y0 0)
	    (- x1 radius -4) (+ y0 1) bevel)
      ;; left
      (box x0 (+ y0 radius)
	   (+ x0 diameter) (- y1 radius)
	   fill)
      (line (+ x0 1) (+ y0 radius)
	    (+ x0 1) (- y1 radius -3) bevel)
      ;; x1
      (box (- x1 diameter) (+ y0 radius)
	   x1 (- y1 radius)
	   fill)
      (line x1 (+ y0 radius)
	    x1 (- y1 radius) chisel)
      ;; content area
      (box (+ x0 radius) (+ y0 radius)
	   (- x1 radius) (- y1 radius)
	   fill)
      ;; cover seams
      (disc (- x1 radius 1) (- y1 radius 1) fill) ;; y1 x1
      (disc (+ x0 radius 1) (- y1 radius 1) fill) ;; y1 left
      (disc (+ x0 radius 1) (+ y0 radius 1) fill) ;; top left
      (disc (- x1 radius 1) (+ y0 radius 1) fill) ;; top x1
      )))

(define-method draw-flat-patch nil (x0 y0 x1 y1
				    &key depressed dark socket color)
  "Draw a square-cornered Xelf notation patch. 
Places its top left corner at (X0 Y0), bottom right at (X1 Y1). If
DEPRESSED is non-nil, draw an indentation; otherwise a raised area is
drawn. If DARK is non-nil, paint a darker region."
  (with-node-drawing 
    (let ((bevel (or color (if depressed shadow highlight)))
	  (chisel (or color (if depressed highlight shadow)))
	  (fill (or color (if socket
			      *socket-color*
			      (if dark background background)))))
      ;; content area
      (box x0 y0  
	   x1 y1
	   fill)
      ;; bottom
      (line x0 y1 
	    x1 y1 
	    chisel)
      ;; top
      (line x0 y0
	    x1 y0 
	    bevel)
      ;; left
      (line x0 y0
	    x0 y1 
	    bevel)
      ;; right
      (line x1 y0
	    x1 y1 
	    chisel)
      )))

(define-method draw-patch nil (x0 y0 x1 y1 
				    &key depressed dark socket color (style *style*))
  "Draw a Xelf notation patch in the current `*style*'.
Places its top left corner at (X0 Y0), bottom right at (X1 Y1)."
  (let ((draw-function (ecase style
			 (:rounded #'draw-rounded-patch)
			 (:flat #'draw-flat-patch))))
    (funcall draw-function self
	     x0 y0 x1 y1 
	     :depressed depressed :dark dark 
	     :socket socket :color color)))

;;; Standard ways of blinking a cursor

(defparameter *cursor-blink-time* 8 
  "The number of frames the cursor displays each color while blinking.")

(defparameter *cursor-color* "magenta" 
  "The color of the cursor when not blinking.")

(defparameter *cursor-blink-color* "cyan"
  "The color of the cursor when blinking.")

(define-method draw-cursor nil (&rest ignore)
  "Draw the cursor. By default, it is not drawn at all."
  nil)

(defparameter *highlight-background-color* "gray90")

(defparameter *highlight-foreground-color* "gray10")

(define-method draw-focus nil ()
  "Draw any additional indications of input focus." nil)

(define-method draw-highlight nil () 
  "Draw any additional indications of mouseover." nil)

(defparameter *hover-color* "cyan" 
  "Name of the color used to indicate areas where objects can be
dropped.")

(defparameter *hover-alpha* 0.8)

(define-method draw-cursor nil (&rest args)
  (draw-indicator :drop
		  (- %x (dash 1)) 
		  (- %y (dash 1))
		  :color "white"
		  :scale 1.0
		  :background "gray70"))

(define-method draw-hover nil ()
  "Draw something to indicate that this object can recieve a drop.
See buffers.lisp for more on the implementation of drag-and-drop."
  (with-fields (x y width height inputs) self
    (draw-box x y width height
	      :color *hover-color* :alpha *hover-alpha*)
    (dolist (input inputs)
      (draw input))))

(define-method will-obstruct-p nil (path-finder) nil)

(define-method resize-to-image nil ()
  (when *resources*
    (with-fields (image height width) self
      (when (find-resource-object image :noerror)
	(setf width (image-width image))
	(setf height (image-height image))))))

(define-method scale nil (x-factor &optional y-factor)
  (let ((image (find-resource-object %image)))
    (resize self 
	    (* (sdl:width image) x-factor)
	    (* (sdl:height image) (or y-factor x-factor)))))

(define-method change-image node (image)
  "Change this sprite's currently displayed image to IMAGE, resizing
the object if necessary."
  (when image
    (setf %image image)
    (resize-to-image self)))
  
(define-method draw nil ()
  "Draw this node as a sprite. By default only %IMAGE is drawn.
The following node fields will control sprite drawing:

   %OPACITY  Number in the range 0.0-1.0 with 0.0 being fully transparent
             and 1.0 being fully opaque.

   %BLEND    Blending mode for OpenGL compositing.
             See the function `set-blending-mode' for a list of modes."
  (with-fields (image x y z image-heading width color height blend opacity) self
    (if image 
	(draw-image image x y :z (or z 0)
		    :blend blend :opacity opacity
		    :height height :width width)
	(draw-box x y width height :color color))))
	
	;; (progn (draw-patch self x y (+ x width) (+ y height))
	;;        (mapc #'draw %inputs)))))

(define-method draw-border nil (&optional (color *selection-color*))
  (let ((dash *dash*))
    (with-fields (x y height width) self
      (draw-patch self (- x dash) (- y dash)
		   (+ x width dash)
		   (+ y height dash)
		   :color color))))

;; (define-method draw-background nil (&key color)
;;   (with-fields (x y width height) self
;;     (draw-patch self x y (+ x width) (+ y height) :color color)))

(define-method draw-ghost nil ()
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height)
		 :depressed t :socket t)))

(define-method header-height nil () 0)

(define-method header-width nil () %width)

(defparameter *socket-width* (* 18 *dash*))

(define-method fancy-format-expression nil (expression)
  (assert (not (xelf::object-p expression)))
  (string-downcase
   (typecase expression
     (symbol
	(substitute #\Space #\- (symbol-name expression)))
     (otherwise (format nil "~s" expression)))))

(defun expression-width (expression &optional (font *font*))
  (if (xelf::object-p expression)
      *socket-width*
      (font-text-width (fancy-format-expression expression) font)))

(define-method set-label-string nil (label)
  (assert (stringp label))
  (setf %label label))

(define-method label-string nil ()
  %label)

(define-method label-width nil ()
  (if (or (null %label) (string= "" %label))
      0
      (+ (dash 2)
	 (font-text-width %label *node-font*))))
    
(define-method draw-label-string nil (string &optional color)
  (with-node-drawing 
    (with-fields (x y) self
      (let* ((dash *dash*)
	     (left (+ x (* 2 dash)))
	     (y0 (+ y dash 1)))
	(text left y0 string color)))))

(define-method draw-label nil (expression)
  (draw-label-string self (fancy-format-expression expression)))

;;; Layout management

(define-method center nil ()
  "Automatically center the node on the screen."
  (with-fields (window-x window-y) *buffer*
    (with-fields (x y width height) self
      (let ((center-x (+ window-x (/ *gl-screen-width* 2)))
	    (center-y (+ window-y (/ *gl-screen-height* 2))))
	(setf x (+ (- center-x (/ width 2))))
	(setf y (+ (- center-y (/ width 2))))))))

(define-method center-as-dialog nil ()
  (center self)
  (align-to-pixels self))

(define-method pin nil ()
  "Prevent dragging and moving of this node."
  (setf %pinned t))

(define-method unpin nil () 
  "Allow dragging and moving of this node."
  (setf %pinned nil))

(define-method pinnedp nil ()
  "When non-nil, dragging and moving are disallowed for this node."
  %pinned)

(define-method resize nil (width height)
  "Change this object's size to WIDTH by HEIGHT units."
  (quadtree-delete-maybe self)
  (setf %height height)
  (setf %width width)
  (quadtree-insert-maybe self)
  nil)
;;  (invalidate-layout self))

(define-method layout nil () 
  (when %image 
    (resize-to-image self)))

     ;; (with-fields (height width label) self
      ;; 	(with-field-values (x y inputs) self
      ;; 	  (let* ((left (+ x (label-width self)))
      ;; 		 (max-height (font-height *font*))
      ;; 		 (dash (dash 1)))
      ;; 	    (dolist (input inputs)
      ;; 	      (move-to input (+ left dash) y)
      ;; 	      (layout input)
      ;; 	      (setf max-height (max max-height (field-value 'height input)))
      ;; 	      (incf left (dash 1 (field-value 'width input))))
      ;; 	    ;; now update own dimensions
      ;; 	    (setf width (dash 1 (- left x)))
      ;; 	    (setf height (+  (if (null inputs)
      ;; 				     dash 0) max-height)))))))

;;; Sound 

(define-method play-sound node (name)
  (when (or (null (cursor))
	    (and (cursor) (%hearing-distance (cursor))))
    (when (<= (distance-to-cursor self)
	      (%hearing-distance (cursor)))
      (play-sample name))))

;;; Collision detection and UI hit testing

(define-method hit nil (mouse-x mouse-y)
  "Return this nil (or child input node) if the coordinates MOUSE-X
and MOUSE-Y identify a point inside the nil (or input node.)"
  (with-fields (x y width height inputs) self
    (when (and x y width height)
      (when (within-extents mouse-x mouse-y x y
			    (+ x width) (+ y height))
	(labels ((try (it)
		   (hit it mouse-x mouse-y)))
	  (or (some #'try inputs) 
	      self))))))

(define-method bounding-box nil ()
  "Return this object's bounding box as multiple values.
The order is (TOP LEFT RIGHT BOTTOM)."
  (when (null %height)
    (resize-to-image self))
  (with-fields (x y width height) self
    (values 
     (cfloat y)
     (cfloat x)
     (cfloat (+ x width))
     (cfloat (+ y height)))))

(define-method center-point nil ()
  "Return this object's center point as multiple values X and Y."
  (multiple-value-bind (top left right bottom)
      (the (values float float float float) (bounding-box self))
    (let ((half (cfloat 0.5)))
      (declare (single-float half top left right bottom) (optimize (speed 3)))
      (values (* half (+ left right))
	      (* half (+ top bottom))))))

(define-method location nil ()
  (values %x %y))

(define-method left-of nil (&optional other)
  (let ((width (field-value 'width (or other self))))
    (values (- %x width) %y)))
  
(define-method right-of nil ()
  (values (+ %x %width) %y))

(define-method above nil (&optional other)
  (let ((height (field-value 'height (or other self))))
    (values (- %x %width) %y)))
  
(define-method below nil ()
  (values %x (+ %y %height)))

(define-method left-of-center nil (&optional other)
  (multiple-value-bind (x y) (left-of self other)
    (values x (+ y (/ %height 2)))))

(define-method right-of-center nil ()
  (multiple-value-bind (x y) (left-of-center self)
    (values (+ x %width) y)))

(define-method above-center nil (&optional other)
  (multiple-value-bind (x y) (above self other)
    (values (+ x (/ %width 2)) y)))

(define-method below-center nil ()
  (multiple-value-bind (x y) 
      (above-center self)
    (values x (+ y %height))))

(define-method handle-collision nil (thing)
  (collide self thing))

(define-method collide nil (object)
  (declare (ignore object))
  "Respond to a collision detected with OBJECT. The default implementation does nothing."
  nil)

(define-method special-collision nil () nil)

(defun point-in-rectangle-p (x y width height o-top o-left o-width o-height)
  (declare (single-float x y width height o-top o-left o-width o-height) (optimize (speed 3)))
  (not (or 
	;; is the top below the other bottom?
	(<= (+ o-top o-height) y)
	;; is bottom above other top?
	(<= (+ y height) o-top)
	;; is right to left of other left?
	(<= (+ x width) o-left)
	;; is left to right of other right?
	(<= (+ o-left o-width) x))))

(define-method touching-point-p nil (x y)
  (within-extents x y %x %y (+ %x %width) (+ %y %height)))

(define-method colliding-with-rectangle-p nil (o-top o-left o-width o-height)
  ;; you must pass arguments in Y X order since this is TOP then LEFT
  (with-fields (x y width height) self
    (point-in-rectangle-p (cfloat x) (cfloat y) (cfloat width) (cfloat height) 
			  (cfloat o-top) (cfloat o-left) (cfloat o-width) (cfloat o-height))))

(defun colliding-with-bounding-box-p (self top left right bottom)
  ;; you must pass arguments in Y X order since this is TOP then LEFT
  (with-fields (x y width height) self
    (when (and width height)
      (point-in-rectangle-p (cfloat x) (cfloat y) (cfloat width) (cfloat height)
			    (cfloat top) (cfloat left)
			    (cfloat (- right left)) (cfloat(- bottom top))))))

;; (define-method contained-in-bounding-box nil (bounding-box)
;;   (bounding-box-contains bounding-box (multiple-value-list (bounding-box self))))

(defmethod colliding-with-p ((self node) (thing node))
  "Return non-nil if this node collides with THING."
  (multiple-value-bind (top left right bottom) 
      (bounding-box thing)
    (colliding-with-bounding-box-p self top left right bottom)))

(define-method direction-to nil (thing)
  "Return a direction keyword approximating the direction to THING."
  (with-fields (x y) thing
    (find-direction %x %y x y)))

(define-method heading-between nil (thing)
  (multiple-value-bind (x y) (center-point self)
    (multiple-value-bind (x0 y0) (center-point thing)
      (find-heading x y x0 y0))))

(define-method aim-at nil (node)
  "Aim the current heading at the other NODE."
  (setf %heading (heading-between self node)))

(define-method aim nil (heading)
  "Aim this NODE toward the angle HEADING."
  (assert (numberp heading))
  (setf %heading heading))

(define-method distance-between nil (thing)
  "Return the straight-line distance between here and THING.
Note that the center-points of the objects are used for comparison."
  (multiple-value-bind (x0 y0) (center-point self)
    (multiple-value-bind (x y) (center-point (find-object thing))
      (distance x0 y0 x y))))

;;; Simple scheduling mechanisms

(defblock task method-name nil target nil arguments nil clock nil subtasks nil finished nil)

(defmethod initialize-instance :after ((self task) 
		       &key method-name target arguments clock subtasks)
    (assert method-name)
    (assert (listp arguments))
    (assert (xelfp target))
    (assert (or (eq t clock)
		(null clock)
		(and (integerp clock)
		     (plusp clock))))
    (setf (field-value 'method-name self) method-name
	  (field-value 'arguments self) arguments
	  (field-value 'target self) (find-uuid target)
	  (field-value 'subtasks self) subtasks
	  (field-value 'clock self) clock))

(define-method finish task ()
  (setf %finished t))

(define-method evaluate task ()
  (when (xelfp %target)
    (apply (symbol-function %method-name) (find-object %target) %arguments)))

(define-method running task ()
  (with-fields (method-name target arguments clock finished) self
    (cond 
      ;; if finished, quit now.
      (finished nil)
      ;; countdown exists and is finished.
      ((and (integerp clock)
	    (zerop clock))
       (prog1 nil 
	 (evaluate self)
	      ))
      ;; countdown not finished. tell manager to keep running, 
      ;; but don't evaluate at this time
      ((and (integerp clock)
	    (plusp clock))
       (prog1 t 
	 (decf clock)))
      ;; no countdown, but we should test the output.
      ;; if non-nil, manager keeps us running.
      ((eq t clock)
       (let ((result (evaluate self)))
	 (prog1 result
	   (if result
	       (mapc #'running %subtasks)
	       (mapc #'finish %subtasks)))))
      ;; no countdown or testing. just keep running.
      ((null clock)
       (prog1 t (evaluate self)))
      ;; shouldn't reach here
      (t (error "Invalid task.")))))

(defun seconds->frames (seconds)
  "Return the time in SECONDS as an integer number of frames.
Based on the current *FRAME-RATE*."
  (truncate (* seconds *frame-rate*)))

(defun time-until (updates)
  (assert (>= updates *updates*))
  (- updates *updates*))
  
(defun time-as-frames (value)
  (etypecase value
    (integer value)
    (float (seconds->frames value))))

(defun make-task-form (delay expression &optional subexpressions)
  (destructuring-bind (method target &rest arguments) expression
    (let ((target-sym (gensym))
	  (delay-sym (gensym)))
      `(let ((,target-sym ,target)
	     (,delay-sym ,delay))
	 (add-task ,target-sym
		   (new 'task 
			:method ',(make-non-keyword method)
			:target ,target-sym
			:subtasks (list ,@(make-tasks delay-sym subexpressions))
			:arguments (list ,@arguments)
			:clock ,delay))))))

(defun make-tasks (delay forms)
  (mapcar #'(lambda (form)
	      (make-task-form delay form))
	  forms))

(defmacro later (delay &rest forms)
  (assert (every #'consp forms))
  (let ((clock (time-as-frames delay))) 
    `(progn ,@(make-tasks clock forms))))

(defmacro later-at (absolute-time &body forms)
  `(later ,(time-until absolute-time) ,@forms))

(defmacro later-while (test-expression &body subtask-expressions)
  `(later ,(make-task-form t test-expression subtask-expressions)))

(define-method after-drop-hook nil ())

;;; blocks.lisp ends here
 
