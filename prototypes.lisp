;;; prototypes.lisp --- an alternative object system for Common Lisp

;; Copyright (C) 2007-2013  David O'Toole
;; Author: David O'Toole dto@blocky.io
;; Keywords: oop
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code: 

(in-package :xelf)

(defvar *self* nil)

(defun split-string-on-lines (string)
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil)
	  while line collect line)))

(defvar *debug-on-error* nil)

(defvar *author* nil)

(defvar *ccl-copyright-notice*
"-----------------------------------------------------------------
This distribution of Xelf is compiled with Clozure Common Lisp.
Clozure CL is (C) 2009 by Clozure Associates, and is licensed under
the terms of the Lisp Lesser General Public License (LLGPL). The LLGPL
consists of a preamble together with the GNU Lesser General Public
License. Where these conflict, the preamble takes precedence.  The
full text of these licenses may be found in this distribution in the
directory ./licenses, in the files PREAMBLE.CCL.txt and LGPL.CCL.txt. 
More information on Clozure CL, and complete source code, may be found
at the Clozure Associates website: http://ccl.clozure.com/
")

(defvar *sbcl-copyright-notice* 
"-----------------------------------------------------------------
This distribution of Xelf is compiled with Steel Bank Common Lisp (SBCL).
Steel Bank Common Lisp (SBCL) is free software, and comes with
absolutely no warranty. Please see the file named
./licenses/COPYING.SBCL.txt The PCL implementation is (C) 1985-1990
Xerox Corporation.  Portions of LOOP are Copyright (c) 1986 by the
Massachusetts Institute of Technology. Portions of LOOP are
Copyright (c) 1989-1992 by Symbolics, Inc.  More information on SBCL
and complete source code may be found at the SBCL website: http://sbcl.org
")

(defvar *compiler-copyright-notice*
  #+ccl *ccl-copyright-notice*
  #+sbcl *sbcl-copyright-notice*)

(defvar *copyright-notice*
"-----------------------------------------------------------------
Welcome to Xelf. 
Xelf is Copyright (C) 2006-2013 by David T O'Toole <dto@blocky.io>
http://xelf.me/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program, in the included file named \"COPYING\".
If not, see <http://www.gnu.org/licenses/>.

On some platforms, Xelf is distributed along with libSDL 1.2 (Simple
Direct Media Layer), which is provided under the terms of the GNU
Lesser General Public License. See the included file
xelf/licenses/README-SDL.txt for more details.

Some functions in the file logic.lisp are based on code written by
Peter Norvig in his book 'Paradigms of Artificial Intelligence
Programming'. See logic.lisp for details.

Some of the OpenGL functions in console.lisp are derived from code in
Bart Botta's CL-OPENGL tutorials; see http://3bb.cc/tutorials/cl-opengl/

This program includes the free DejaVu fonts family in the subdirectory
./standard/. For more information, see the file named
DEJAVU-FONTS-LICENSE.txt in the xelf/licenses subdirectory.

Please see the included text files \"COPYING\" and \"CREDITS\" for
more information.
")

(defvar *full-copyright-notice*
  (concatenate 'string *compiler-copyright-notice* *copyright-notice*))

;;; Extended argument lists

;; TODO remove obsolete code

(defun extended-arglist-p (lambda-list)
  "An extended argument list is like an ordinary CL argument list,
but with each argument's entry replaced by a triple:

  (ARGUMENT-NAME DATA-TYPE &rest OPTIONS)

These triples may be arranged in the extended argument list just as
in a `destructuring-bind', i.e. `&optional', `&key', and all the
other destructuring features:

 ((POSITIONAL-ARG1 TYPE OPTIONS) (POSITIONAL-ARG2 TYPE OPTIONS)
  &KEY (KEYWORD-ARG1 TYPE OPTIONS) (KEYWORD-ARG2 TYPE OPTIONS))

ARG is the argument name (a symbol). DATA-TYPE is a Common Lisp
identifier such as `integer' or `(or integer symbol)' or the like.
See the documentation for the function `schema-option' for more
information on the OPTIONS field.

NOTE: &key and &optional are not yet implemented for extended
arglists.
"
  (and (not (null lambda-list))
       (listp (first lambda-list))
       (not (null (first lambda-list)))))

(defun schemap (datum)
  (and (consp datum)
       (every #'consp datum)
       (every #'symbolp
	      (mapcar #'first datum))))

(defun schema-name (schema)
  (first schema))

(defun schema-type (schema)
  (second schema))

(defun schema-options (schema)
  (nthcdr 2 schema))

(defun schema-option (schema option)
  "Find the value (if any) of the option named OPTION within the
  extended argument list schema SCHEMA. The following keywords are
  valid for OPTION:

  :DEFAULT   The default value for the argument. With no default,
             the presentation history is consulted for a value.

  :DOCUMENTATION     The documentation string.

  :LABEL   User-visible name of the argument. If left unset, the
                  default is `foo bar baz' for the command
                  `foo-bar-baz'.

  :WHEN           Only read the value if this predicate-form returns 
                  non-nil when invoked on the value.

  Not yet supported:

  :PROMPT    A string (or a form evaluating to a string) used as the
             prompt for this argument.

  :PROMPT-MODE   :raw means that prompt is just printed.
                 :normal (the default) specifies standard reformatting:
               
                       Command Name (type1) :  <---- bright red input star
                               (type2 [default: foo) ...
                               (keywords) :Keyword Name (type3)


  :DEFAULT-TYPE   The presentation type of the argument value. Use
                  this with :default when the default value could
                  be interpreted more than one way.

  :PROVIDE-DEFAULT  When non-nil, the above options relating to
                    defaults are activated.

  :DISPLAY-DEFAULT   When non-nil, the default is printed in the
                     prompt. Default is t.
  :CONFIRM ..."
  (assert (keywordp option))
  (getf (schema-options schema) option))

(defun make-lambda-list-entry (entry)
  "Make an ordinary lambda list item corresponding to ENTRY, an
element of an extended argument list."
  (assert (and (not (null entry))
	       (listp entry)))
  (let ((name (schema-name entry)) 
	(default (schema-option entry :default)))
    (if (null default)
	name
	(list name default))))

(defun make-lambda-list (arglist)
  "Return an ordinary function lambda list corresponding to the
extended argument list ARGLIST."
  (cons '&optional (mapcar #'make-lambda-list-entry arglist)))

;;; Method dictionary 

(defvar *methods* nil)

(defun initialize-methods ()
  (setf *methods* (make-hash-table :test 'equal)))

(initialize-methods)

(defun make-method-id (prototype method)
  (let ((name (name (find-prototype prototype))))
    (assert (stringp name))
    (concatenate 'string 
		 (subseq name (1+ (position (character ":") name)))
		 "%" 
		 (symbol-name method))))

(defun find-method-id (prototype method &optional create)
  (assert prototype)
  (assert method)
  (let ((pointer (find-prototype prototype)))
    (block searching
      (loop while pointer do
	(let ((id (make-method-id pointer method)))
	  (let ((result (gethash id *methods*)))
	    (if (or result create)
		(return-from searching id)
		(prog1 nil 
		  (setf pointer (find-super pointer))))))))))

(defun find-method-data (name method &optional no-error)
  (assert (hash-table-p *methods*))
  (let ((id (find-method-id name method no-error)))
    (let ((result (gethash id *methods*)))
      (if result 
	  (values-list result)
	  (unless no-error (error "Cannot find method: ~S" 
				  (list name method)))))))
  
(defun add-method-to-dictionary (prototype method arglist &optional options)
  (when (null *methods*)
    (initialize-methods))
  (let ((id (find-method-id prototype method :create)))
    (assert (stringp id))
    (setf (gethash id *methods*) (list arglist options prototype method))
    (values id arglist)))

(defun method-defun-symbol (method-symbol-name prototype-name)
  (intern (concatenate 'string
		       prototype-name "%" method-symbol-name )))

(defun method-options (name method &optional noerror)
  (multiple-value-bind (schema options)
      (find-method-data name method noerror)
    (declare (ignore schema))
    options))

(defun method-option (name method option)
  (getf (method-options name method)
	option))
	           
(defun method-schema (prototype method)
  (assert (hash-table-p *methods*))
  (let ((id (find-method-id (name (find-prototype prototype)) 
			    (make-keyword method))))
    (assert (stringp id))
    (let ((result (gethash id *methods*)))
      (when result
	(first result)))))

(defun method-argument-entry (prototype method index)
  (assert (integerp index))
  (let ((schema (method-schema prototype method)))
    (assert (< index (length schema)))
    (nth index schema)))
    
(defun method-argument-count (prototype method)
  (length (method-schema prototype method)))

(defun method-argument-type (prototype method index)
  (schema-type (method-argument-entry prototype method index)))

(defun method-argument-name (prototype method index)
  (schema-name (method-argument-entry prototype method index)))

(defun method-argument-options (prototype method index)
  (schema-options (method-argument-entry prototype method index)))

;;; Prototype dictionary

(defvar *prototypes* nil)

(defun initialize-prototypes ()
  (setf *prototypes* (make-hash-table :test 'eq)))

(initialize-prototypes)

(defun add-prototype (object)
  (when (null *prototypes*)
    (initialize-prototypes))
  (setf (gethash (name object)
		 *prototypes*)
	(find-object object)))

(defun find-prototype (name &optional noerror)
  (assert (hash-table-p *prototypes*))
  (if (object-p name)
      name
      (or (gethash name *prototypes*)
	  (unless noerror
	    (error "Cannot find prototype named ~S" name)))))

;;; UUID object dictionary

(defun make-uuid ()
  (coerce (uuid:print-bytes 
	   nil 
	   (uuid:make-v4-uuid))
	  'simple-string))

(defvar *database* nil)

(defun initialize-database ()
  (setf *database* 
	(make-hash-table :test 'equal :size 8192)))

(initialize-database)

(defun add-object-to-database (object)
  (when (null *database*)
    (initialize-database))
  (setf (gethash 
	 (the simple-string (uuid object))
	 *database*)
	object))

(defun remove-object-from-database (object)
  (let ((total (hash-table-count *database*)))
    (assert (hash-table-p *database*))
    (assert (plusp total))
    (remhash 
     (the simple-string (find-uuid object))
     *database*)))
;    (assert (> total (hash-table-count *database*)))))

(defun find-object-by-uuid (uuid &optional noerror)
  (or (gethash (the simple-string uuid) *database*)
      (unless noerror
	(error "Cannot find object for uuid ~S" uuid))))

(defun purge-all-objects ()
  (flet ((count-entries () (+ (hash-table-count *database*)
		      (hash-table-count *prototypes*))))
    (let ((before-count (count-entries)))
      (message "Searching in ~A objects for externals..." before-count)
      (flet ((purge (id object)
	       (let ((name (name object)))
		 (unless (and (stringp name)
			      (search "XELF:" name))
		   (remhash id *database*)))))
	(maphash #'purge *database*)
	(maphash #'purge *prototypes*)
	(let ((delta (- before-count (count-entries))))
	  (message "Removed ~A external objects." delta))))))
		  
;;; Finding any object by proto-name or UUID

(defun find-object (thing &optional no-error) 
  (when (not (null thing))
    (let ((result 
	    (etypecase thing
	      (symbol (find-prototype thing))
	      (string (or (find-object-by-uuid thing :noerror)
			  (find-prototype thing :noerror)))
	      (xelf-object thing))))
      (prog1 result
	(unless no-error
	  (when (null result)
	    (error "Cannot find object: ~S" thing)))))))
      
(defun find-super (object)
  (super (find-object object)))

(defun find-super-prototype-name (object)
  (let ((super (super (find-object object))))
    (when super (name (find-object super)))))

(defun object-eq (a b)
  (when (and a b)
    (eq (find-object a)
	(find-object b))))

(defun find-schema (method &optional target)
  (let ((source (or (when target
		      (find-super-prototype-name 
		       (send :as-target target)))
		    "XELF:BLOCK")))
    (method-schema source method)))

;;; Emacs Lisp compatibilty macro 

(defmacro while (test &body body)
  `(loop while ,test do ,@body))

;;; Utility functions

(defun-memo make-keyword (S) (:test 'eq) 
  "Make the symbol or string S into a keyword symbol."
  (etypecase S
    (string (intern (string-upcase S) :keyword))
    (symbol (intern (symbol-name S) :keyword))))

(defun make-non-keyword (S)
  "Make the symbol or string S into a non-keyword symbol."
  (etypecase S
    (symbol (intern (symbol-name S)))
    (string (intern (string-upcase S)))))

(defun merge-hashes (a b &optional predicate)
  (prog1 a
    (maphash #'(lambda (key value)
		 (when (or (null predicate)
			   (funcall predicate key))
		   (setf (gethash key a) value)))
	     b)))
	       
(defun make-prototype-id (thing &optional (package (project-package)) create) 
  (let ((delimiter ":"))
    (if (null thing)
	(error "Cannot make a prototype ID for nil.")
	(cond
	  ((xelf:object-p thing)
	   (name thing))
	  ((stringp thing) 
	   (intern thing package))
	  ((symbolp thing) thing)))))


	    ;; (apply #'concatenate 'string 
	    ;; 	   (if (search delimiter thing)
	    ;; 	       (list thing)
	    ;; 	       (list (package-name package)
	    ;; 		     delimiter thing))))

	    ;; (let ((thing-package (symbol-package thing)))
	    ;;   ;; check if name is already in Xelf
	    ;;   (let ((prefix (if (eq thing-package (find-package :common-lisp))
	    ;;   ;; (let ((prefix (if (find-prototype (concatenate 'string "XELF"
	    ;;   ;; 						     (symbol-name thing)))
	    ;; 			;; override if so
	    ;; 			"XELF"
	    ;; 			(package-name thing-package))))
	    ;; 	(concatenate 'string prefix delimiter (symbol-name thing))))))))))

;;; Object data structure

;; Each object's "bookkeeping data" is stored in a structure. The
;; structure represents the object, and typically the programmer will
;; not need to access these structure fields.

(defclass xelf-object ()
  ;; Field collection can be a hash table or list.
  ((fields :initform nil :accessor fields :initarg :fields)
   ;; Objects can inherit field values from a prototype object which
   ;; then influences the new object's behavior. We must store a link
   ;; to this "super" object so that `field-value' can obtain the
   ;; inherited field values.
   (super :initform nil :accessor super :initarg :super )
   ;; Objects may have names. A name is a string that identifies the
   ;; object. Named objects are "prototypes" from which other objects
   ;; may be created or "cloned".
   (name :initform nil :accessor name :initarg :name)
   ;; Here's the uuid string.
   (uuid :initform nil :accessor uuid :initarg :uuid)
   ;; The last few methods called are cached in this alist.
   (cache :initform nil :accessor cache :initarg :cache)))

(add-prototype (make-instance 'xelf-object :fields nil :super nil :name 'xelf-object :uuid (make-uuid))) 

(defmethod initialize ((self xelf-object) &key))

(defun object-p (x)
  (typep x 'xelf:xelf-object))

(defun find-uuid (object)
  (when object
    (uuid (find-object object))))

(defun verify (thing)
  (assert (object-p (find-object thing))))

(defun xelfp (thing)
  (or (xelf:object-p thing)
      (and (stringp thing)
	   (find-object thing :no-error))))

;;; Fields

;; An object's field collection is either a hash table or property
;; list. The function `field-value' implements the chaining field
;; lookups that make behavior inheritance work in XELF.

;; If a field value is not present in a given object's field
;; collection, the object's super is also checked for a value, and
;; then its super, and so on. This is how objects can inherit data
;; and behavior from prototypes. See `field-value'.

;; When you set the value of any field, the super's value is
;; hidden from that point on. There is no way to remove a local field
;; value. See `set-field-value'.

(defvar *lookup-failure* (gensym)
  "A value returned in order to signify the failure of a field lookup.
When looking up fields, we need a default value to indicate that a
lookup failed (see `gethash'). But this value could be a perfectly
legitimate field value, and the system would then falsely report a
field lookup error because it could not tell the difference. By using
an uninterned symbol as that default value, we can be sure that it
won't be `eq' to anything. See `field-value'.

This is only used internally. In most situations, a field access or
method call that references a non-existent field will signal a
`no-such-field' error.")

(define-condition no-such-field (error)
  ((field-name :initarg :field-name :accessor field-name)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     (format stream "No such field ~S in object ~S." 
		     (field-name condition)
		     (object condition)))))

(defun fref (fields key)
  (etypecase fields
    (list (lfref fields key))
    (hash-table (gethash key fields *lookup-failure*))))

(defun lfref (f key)
  (declare (optimize (speed 3)) (list f))
  (loop (if (eq key (first f)) (return)
	    (if (null f) (return)
		(setf f (rest (the list (rest f)))))))
  (if f (second f) *lookup-failure*))

(defun set-fref (fields key value)
  (etypecase fields
    (list (set-lfref fields key value))
;    (list (setf (getf fields key) value))
    (hash-table (values (setf (gethash key fields) value)
			fields))))

(defun set-lfref (f key value)
  (if (null f)
      (values value (list key value))
      (let ((p f))
	(loop (if (eq key (first p)) (return)
		  (if (null p) (return)
		      (setf p (rest (the list (rest p)))))))
	(if p
	    ;; update entry in place
	    (setf (second p) value)
	    ;; cons new entry
	    (setf f (nconc (list key value) f)))
	(values value f))))
  
(defsetf fref set-fref)

(defun field-value (field thing &optional noerror)
  "Return the value of FIELD in the object THING.
If the FIELD has no value in THING, then the object's super is also
checked, and so on. If a value is found during these checks, it is
returned. If a value cannot be found, an error of type `no-such-field'
is signaled, unless NOERROR is non-nil; in that case,
`*lookup-failure*' is returned. See also `has-field'."
  (declare (optimize (speed 3))
	   (inline fref set-fref object-fields object-super))
  (let ((pointer (find-object thing))
	result found)
    ;; search the chain of objects for a field value.
    (loop while (and pointer (not found)) do
	 (setf result (fref (fields pointer) field))
	 (if (eq *lookup-failure* result)
	     ;; it's not here. search the super, if any.
	     (setf pointer (find-object (super pointer)))
	     ;; we found a value in this object.
	     (setf found t)))
    (if found result
	(if noerror 
	    *lookup-failure*   
	    (error 'no-such-field :field-name field :object thing)))))

(defun map-fields (function object)
  "For each field in OBJECT's field collection, the supplied FUNCTION
is invoked with the field-name and corresponding value as its two
arguments."
  (let ((fields (fields object)))
    (etypecase fields
      (hash-table (prog1 nil (maphash function fields)))
      (list (loop while fields 
		  do (funcall function 
			      (pop fields) 
			      (pop fields)))))))

(defun has-local-value (field thing)
  (let ((object (find-object thing)))
    (not (eq *lookup-failure* (fref (fields object) field)))))

(defun set-field-value (field thing value)
  "Set OBJECT's FIELD to VALUE.
The new value overrides any inherited value."
  (declare (inline set-fref object-fields find-object))
  (prog1 value
    (let ((object (find-object thing)))
      (multiple-value-bind (value fields)
	  (set-fref (fields object) field value)
	    ;; don't lose new list heads
	(prog1 value 
	  (when (consp fields)
	    (setf (fields object) fields)))))))
  
(defsetf field-value set-field-value)

(defun has-field (field object)
  "Return non-nil if FIELD has any value in OBJECT or its supers."
  (not (eq *lookup-failure* (field-value field object :noerror))))

(defun has-method (method object)
  (let ((val (field-value method object :no-error)))
    (and val (symbolp val) (fboundp val))))

(defun with-fields-ex (fields expression binding-type body)
  (assert (member binding-type '(let symbol-macrolet)))
  (assert (listp fields))
  (let ((object-sym (gensym)))
    (labels ((make-clause (sym)
	       (list (make-non-keyword sym)
		     `(field-value ,(make-keyword sym) ,object-sym))))
      `(let ((,object-sym ,expression))
	 (,binding-type ,(mapcar #'make-clause fields)
			,@body)))))

(defmacro with-fields (fields expression &body body)
  "Bind FIELDS from the object EXPRESSION to names in the local
environment in evaluating BODY. Local symbol macros are used, meaning
that each reference (get or set) is a slot access on the object."
  (with-fields-ex fields expression 'symbol-macrolet body))

(defmacro with-field-values (fields expression &body body)
  "Bind the names in FIELDS to the values of the corresponding fields
in EXPRESSION, in evaluating BODY. Each slot is accessed only once,
upon binding."
  (with-fields-ex fields expression 'let body))

;;; Methods and messages

;; Methods are function-valued fields whose first argument is the
;; object to operate on. The remaining arguments are the arguments to
;; the method. The `define-method' macro defined later will insert
;; this implicit `self' argument for you, and implement some syntactic
;; sugar.

;; First we implement method caching, which avoids hash table lookups
;; for repeatedly used methods. (This is similar to the GNU Objective C
;; implementation.)

(defconstant +cache-size+ 6)

(defun initialize-method-cache (object)
  (setf (slot-value object 'cache)
	(list (cons nil nil) (cons nil nil) (cons nil nil)
	      (cons nil nil) (cons nil nil) (cons nil nil))))

(defun cache-method (object method func)
  (let ((cache (cache object))
	(entry nil))
    (assert (listp cache))
    (rotatef (sixth cache)
	     (fifth cache)
             (fourth cache)
             (third cache)
    	     (second cache)
	     (first cache))
    (setf entry (first cache))
    (setf (car entry) method)
    (setf (cdr entry) func)))

(defun method-cache-lookup (object method)
  (declare (optimize (speed 3))
	   (type keyword method))
  (cdr (assoc method (slot-value object 'cache))))

;; Next comes the basic function for sending a message synchronously
;; and obtaining a return value.  

;; When a message cannot be delivered because no corresponding
;; function was found, XELF attempts to re-send the message via the
;; object's `forward-message' method (if any).

;; An object's `forward-message' method should accept the method-key as the
;; first argument, and the arguments of the original message as the
;; remaining arguments.

;; We also want to be able to invoke the prototype (or "super's")
;; version of a method; for example during initialization, one might
;; wish to run the super's initializer as the first statement in the
;; child's.

(defvar *forward-message-handler* nil)

(defmacro with-forward-message-handler (form &body body)
  `(let ((*forward-message-handler* ,form)) ,@body))

(defun send (method thing &rest args)
  "Invoke the method identified by the keyword METHOD on the OBJECT with ARGS.
If the method is not found, attempt to forward the message."
  ;; See also `send-queue' and `send-super'
  (let ((object (find-object thing)))
    (when (not (object-p object))
      (error "Cannot send message to non-object: ~A. Did you forget the `self' argument?" object))
    ;; check the cache
    (let ((func (field-value method object))
	  (*self* object))
      (if func
	  ;; cache hit. invoke the method and finish up.
	  (apply func object args)
	  ;; cache miss. look for a value.
	  (progn (setf func (field-value method object :noerror))
		 (if (not (eq *lookup-failure* func))
		     ;; store in local cache and then invoke the method
		     (progn 
		       (cache-method object method func)
		       (apply func object args))
		     ;; no such method. try another handler
		     (let ((handler (or *forward-message-handler* object)))
		       (if (has-field :forward-message handler)
			   (apply (field-value :forward-message handler)
				    method args)
			   (error (format nil "Could not invoke method ~S" method))))))))))

(define-condition null-next (error)
  ((method-key :initarg :message :accessor method-key)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     (format stream "Cannot find next method ~S for object ~S." 
		     (method-key condition)
		     (object condition)))))

(defun definition (method object)
  (block finding
    (loop while object do 
      (when (has-local-value method object)
	(return-from finding 
	  (values (field-value method object) object)))
      (setf object (find-super object)))))

(defun definer (method object)
  (multiple-value-bind (definition definer)
      (definition method object)
    (declare (ignore definition))
    definer))

(defun next-definer (method object)
  (if (has-local-value method object)
      (definer method (find-super object))
      (next-definer method (find-super object))))

(defun next-definition (method object)
  (field-value method (next-definer method object)))

(defvar *next-search-start* nil)

(defun send-super (method object &rest arguments)
  "Invoke next version of METHOD on OBJECT, passing ARGUMENTS.  We do
this by finding the current implementation (via slot lookup), then
finding the next implementation after that."
      (apply (next-definition method (find-object object))
	     object arguments))

;;; Dynamically binding the recipient of a message

(defvar *target* nil)

(defmacro with-target (target &rest body)
  `(let ((*target* ,target))
     ,@body))

;;; Field reference syntax

;; Within method bodies, you can access the fields of `self' with the
;; shorthand
;;
;;   %foo
;;
;; instead of writing
;;
;;   (field-value :foo self)
;;
;; For example:
;;
;;   (princ %name)
;;   (setf %width 10)
;; 

(defvar *field-reference-prefix* "%")

(defun transform-tree (tester transformer tree)
  (cond ((consp tree)
	 ;; it's a cons. process the two subtrees.
	 (destructuring-bind (left . right) tree
	   (cons
	    ;; process left subtree.
	    (if (funcall tester left)
		(funcall transformer left)
		;; nothing to transform here. move on down the left side.
		(if (consp left)
		    (transform-tree tester transformer left)
		    left))
	    ;; process right subtree.
	    (transform-tree tester transformer right))))
	;; it's not a cons. test it.
	((funcall tester tree)
	 (funcall transformer tree))
	;; it failed the test. leave it alone.
	(t tree)))

;;; field references of the form %foo

(defun field-reference-p (form)
  "Return non-nil if FORM is a symbol like %foo."
  (if (symbolp form)
      (let ((name (symbol-name form)))
	(and (> (length name) 1)
	     (or (string= "%" (subseq name 0 1))
		 (string= "@" (subseq name 0 1)))
	     ;; don't catch double %%
	     (not (string= "%" (subseq name 1 2)))))))

(defmacro with-input-values (symbols form &body body)
  (assert (every #'symbolp symbols))
  (let ((thing (gensym)))
    (flet ((make-clause (symbol)
	     `(,symbol (evaluate 
			(input-block ,thing 
				     ,(make-keyword symbol))))))
      `(let* ((,thing ,form)
	      ,@(mapcar #'make-clause symbols))
	 ,@body))))
		      
(defun input-reference-p (form)
  "Return non-nil if FORM is a symbol like %%foo."
  (if (symbolp form)
      (let ((name (symbol-name form)))
	(and (> (length name) 2)
	     (string= "%%" (subseq name 0 2))))))

(defun make-accessor-macrolet-clause (symbol)
  (list symbol
	`(field-value 
	  ,(make-keyword
	     ;; strip percent sign 
	    (subseq (symbol-name symbol) 1))
	  self)))

(defun make-input-accessor-defun-forms (symbol)
  (let ((accessor (make-non-keyword 
		   (concatenate 'string 
				"%%"
				(symbol-name symbol)))))
  `(unless (fboundp ',accessor)
     (defun ,accessor (thing)
       ;; see also blocks.lisp, `define-block-macro'
       (input-block thing ,symbol))
     (export ',accessor))))

(defun make-input-accessor-macrolet-clause (symbol)
  (list symbol
	`(input-block 
	  self
	  ,(make-keyword
	    ;; strip double percent sign 
	    (subseq (symbol-name symbol) 2)))))

(defun make-accessor-flet-clause (symbol)
  `(,symbol (thing)
	    (field-value ,symbol thing)))

(defun transform-method-body (body)
  (let (fields inputs)
    ;; collect %foo symbols used in body
    (transform-tree #'field-reference-p
		    #'(lambda (symbol)
			;; don't modify input
			(prog1 symbol
			  ;; just collect
			  (pushnew symbol fields)))
		    body)
    ;; similarly, collect %%foo symbols
    (transform-tree #'input-reference-p
		    #'(lambda (symbol)
			(prog1 symbol
			  (pushnew symbol inputs)))
		    body)
    ;; arrange for the substitution
    `(symbol-macrolet 
	 ,(append 
	   (mapcar #'make-accessor-macrolet-clause fields)
	   (mapcar #'make-input-accessor-macrolet-clause inputs))
       ,@body)))

(defmacro with-local-fields (&body body)
  (transform-method-body body))

;; (defun transform-field-reference (ref)
;;   "Change the symbol REF from %foo to (field-value :foo self)."
;;   (let ((name (symbol-name ref)))
;;     (list 'field-value 
;; 	  (make-keyword (subseq name 1))
;; 	  'self)))
       
;;; Definining methods

;; TODO upgrade blocks/buffers API where necessary
;; TODO write some new documentation, start with full proper docstrings

;; The `define-method' macro defined below is the main top-level facility
;; for adding methods to prototypes.

(defmacro define-method
    (method-specifier prototype-name arglist &body method-body)
  "Define a new method.

METHOD-NAME is a symbol naming the method.  PROTOTYPE-NAME is the name
of the prototype you are defining a method for. This should be a
symbol (without equals signs---see Prototype Names. ARGLIST is
the argument list for the method. If METHOD-BODY begins with a string,
this string becomes the documentation string for the method.

Any DECLARE forms must appear as the first non-string sexp.

The forms in METHOD-BODY are executed when the method is invoked.
The hidden argument `self' may be referred to as needed within
the method body; it is bound to the object upon which the method
was invoked."
  (assert method-specifier)
  ;; build the components of the defun
  (let ((method-name (etypecase method-specifier
		       (symbol method-specifier)
		       (list (first method-specifier))))
	(options (when (listp method-specifier)
		   (rest method-specifier))))
    (let* ((documentation (if (stringp (first method-body))
			      (first method-body)))
	   (body2 (remove-if #'stringp (transform-method-body method-body)))
	   ;; handle DECLARE forms when these appear first
	   (declaration (when (and (listp (first body2))
				   (eq 'declare (first (first body2))))
			  (first body2)))
	   (declaration2 (append '(declare (ignorable self))
				 (when declaration
				   ;; paste, skipping the declaration keyword
				   (rest declaration))))
	   (pname (or prototype-name 'xelf-object))
	   (prototype-id (make-prototype-id pname nil :create))
	   (field-name (make-keyword method-name))
	   (method-symbol-name (symbol-name method-name))
	   (method-symbol method-name) ;; fixme, unclear naming
	   (defun-symbol (method-defun-symbol 
			  method-symbol-name 
			  (symbol-name prototype-name)))
	   (queue-defun-symbol (intern (concatenate 'string
						    method-symbol-name 
						    "%QUEUE")))
	   (next-defun-symbol (intern (concatenate 'string
						   method-symbol-name
						   "%SUPER")))
	   (method-lambda-list (if (extended-arglist-p arglist)
				   (make-lambda-list arglist)
				   arglist)))
      (let ((name (gensym)))
	`(let ((prototype (find-prototype ',prototype-id :noerror)))
	   ;; make sure it exists
 	   ;; (when (null prototype)
	   ;;   (error (format nil "Cannot define method ~A for nonexistent prototype ~A"
	   ;; 		    ',method-name ',prototype-id)))
	   ;; define the method's Lisp function
	   (defmethod ,method-symbol ((self ,prototype-id) ,@method-lambda-list)
	     ,@(if documentation (list documentation))
	     ,declaration2
	       ;; paste transformed method code
	       ,(if declaration 
		    (rest body2)
		    body2))
	   (export ',method-symbol)
	   ;; store the method's function in the prototype's field
	   (when prototype (setf (field-value ,field-name prototype) ',method-symbol)))))))
	   ;; add this method to the method dictionary
	   ;; (add-method-to-dictionary 
	   ;;  ',prototype-id
	   ;;  ,(make-keyword method-name)
	   ;;  ',arglist
	   ;;  ',options)
	   ;; define the other functions
	   ;; (export ',defun-symbol)
	   ;; (let ((,name ,(make-keyword method-name)))
	   ;;   (unless (fboundp ',method-symbol)
	       ;; ;; tag the symbol as a method
	       ;; (setf (get ',method-symbol 'is-method) t)
	       ;; (defun ,method-symbol (self &rest args)
	       ;; 	 ,@(when documentation (list documentation))
	       ;; 	 (apply #'send ,name self args))
	       ;; (export ',method-symbol)
	       ;; ;; and for message queueing
	       ;; ;; (defun ,queue-defun-symbol (self &rest args)
	       ;; ;; 	 ,@(when documentation (list documentation))
	       ;; ;; 	 (apply #'send-queue ,name self args))
	       ;; (export ',queue-defun-symbol)
	       ;; ;; and for next-method calls.
	       ;; (defun ,next-defun-symbol (self &rest args)
	       ;; 	 ,@(when documentation (list documentation))
	       ;; 	 (apply #'send-super ,name self args))
	       ;; (export ',next-defun-symbol))))))))
  
;;; Defining prototypes
  
;; Objects are created by cloning them from "prototypes". Prototypes
;; are named objects that represent prototypical instances of a
;; certain kind of object. This section implements `define-prototype',
;; the top-level user macro for defining new object prototypes.

;; First we need to define the written syntax for field options, so
;; that we can write these options into prototype declarations later.

(defun transform-declaration (D)
  "Convert the declaration D into a canonical field
descriptor.

The descriptor D must be either a symbol, in which case a field is
defined with no options, or a list of the form:

 (:FIELD-NAME . OPTIONS)

Where OPTIONS is a property list of field options.

The returned entry will be of the form:

 (:FIELD-NAME OPTIONS) 

and will be suitable for use with the functions that operate on field
descriptors, and for inclusion in the association list
%field-descriptors.

See also `define-prototype'.
"
  (etypecase D
    (symbol (list (make-keyword D) nil))
    (list (list (make-keyword (car D)) (cdr D)))))

(defun compose-blank-fields (&optional descriptors (type :hash))
  (ecase type
    (:hash (let ((fields (make-hash-table :test 'eq)))
	     (dolist (d descriptors)
	       (setf (gethash (make-keyword (car d)) fields) nil))
	     fields))
    (:list (let (fields)
	     (dolist (d descriptors)
	       (push nil fields)
	       (push (make-keyword (car d)) fields))
	     fields))))

;; Initforms (i.e. the values of the field option `:initform')
;; initialize fields. A field's initform (if any) is evaluated when an
;; object is cloned from a prototype, and becomes the value of the
;; field for the new instance (instead of inheriting the prototype's
;; value.) See also `define-prototype'.

;; Now we make setf statements (i.e. "field initializers") out of these
;; field initforms.

(defun make-field-initializer (descriptor)
  "Create a setf statement that initializes a field.
The initform is taken from DESCRIPTOR. If there is no initform
specified, no setf statement is generated, because in this case the
slot value is inherited."
  (destructuring-bind (field (&key initform &allow-other-keys)) descriptor
    (if initform `(set-field-value ,field self ,initform))))

(defun plist-to-descriptors (plist)
  (let (descriptors)
    (loop while plist do
      (let* ((field (pop plist))
	     (value (pop plist)))
	(push (list field :initform value)
	      descriptors)))
    (nreverse descriptors)))
	
(defun make-field-accessor-forms (descriptor)
  (let* ((field-name (first descriptor))
	 (accessor-name (make-non-keyword 
			 (concatenate 'string "%" (symbol-name field-name))))
	 (setter-name (make-non-keyword
                         (concatenate 'string "set-" (symbol-name accessor-name)))))
    `((unless (fboundp ',accessor-name)
	(defun ,accessor-name (thing)
	  (field-value ,field-name thing))
	(defun ,setter-name (thing value)
	  (set-field-value ,field-name thing value))
	(defsetf ,accessor-name ,setter-name)
	(export ',accessor-name)
	(export ',setter-name)))))

(defun proto-intern (name)
  (let ((colon (position #\: name)))
    (intern
     (if colon
	 (subseq name (1+ colon))
	 name))))

(defun prototype-variable-name (name)
  (string-upcase 
   (concatenate 'string "%%%" 
		(let ((colon (position #\: name)))
		  (if colon 
		      (subseq name (1+ colon))
		      name)))))

(defmacro define-prototype (name
			    (&key super 
				  documentation
				  &allow-other-keys)
			    &body declarations)
  "Create a new object prototype (possibly based on another prototype).

NAME should be a symbol or string naming the prototype. You can create
objects with

 (clone :foo)

See also `clone'. 

The second argument is a property list of options for the
prototype. Valid keys are:

 :DOCUMENTATION     The documentation string for this prototype.
 :SUPER            The super prototype from which the new prototype will 
                    inherit fields. This form is evaluated.
                     
DECLARATIONS should be a list, each entry of which is
either a list of the form

  (FIELD-NAME . OPTIONS)

or, simply a symbol naming the field---a shorthand for declaring a
field with that name and no options. See also
`transform-declaration'.

OPTIONS is a property list of field options. Valid keys are:

 :INITFORM          A form evaluated to initialize the field
                    upon cloning. If :initform is not provided,
                    the value is inherited from the SUPER.
                    With \":initform nil\", the field is initialized 
                    with the value nil.
 :DOCUMENTATION     Documentation string for the field.
"
  (let* ((pre-descriptors (if (keywordp (first declarations))
			      (plist-to-descriptors declarations)
			      declarations))
	 (descriptors (mapcar #'transform-declaration 
			      pre-descriptors))
	 (prototype-id (make-prototype-id name *project* :create))
	 (field-initializer-body (delete nil (mapcar #'make-field-initializer 
						     descriptors))))
    `(progn  
       (defclass ,name ,(when super (list super)) ())
       (defmethod initialize-fields ((self ,name))
	 (when (fboundp 'call-next-method)
	   (call-next-method))
	 ,@field-initializer-body)
       (defmethod initialize-instance :after ((self ,name) &key)
	 (initialize-fields self))
       (let* ((uuid (make-uuid))
	      (fields (compose-blank-fields ',descriptors))
	      (prototype (make-instance ',name
					:fields fields
					:name ',name
					:uuid uuid
					:super ',super)))
	 ;; create the (%fieldname object) functions
	 ,@(mapcan #'make-field-accessor-forms descriptors)
	 ;; (setf (fref fields :field-descriptors) ',descriptors)
	 ;; (setf (fref fields :documentation) ,documentation)
	 (initialize-method-cache prototype)
	 ;; now add it to the dictionaries
	 (add-prototype prototype)
	 (add-object-to-database prototype)
	 ;; ;; declare a variable so that SLIME can find the definition later
	 ;; (defparameter ,(intern (prototype-variable-name prototype-id))
	 ;;   uuid)
	 ;; return the uuid and object
	 (values uuid prototype)))))
  
  (defun is-a (type thing)
    (and type thing
	 (xelfp thing)
	 (string= (make-prototype-id type)
		(name (super 
			      (find-object thing))))))

;;; Cloning and duplicating objects
  
;; (defun new (prototype-name &rest initargs)
;;   (apply #'clone (make-prototype-id prototype-name) initargs))

(defun clone (prototype &rest initargs)
  "Create a new object from PROTOTYPE and pass INITARGS to the
initializer. The new object is created with fields for which INITFORMS
were specified (if any; see `define-prototype'); the INITFORMS are
evaluated, then any applicable initializer is triggered."
  ;; navigate to parent if a non-prototype is passed
  (when (not (name (find-object prototype)))
    (setf prototype (find-super prototype)))
  ;; now clone it
  (let* ((uuid (make-uuid))
	 (super (find-object prototype))
	 (type (field-value :field-collection-type super))
	 (fields (compose-blank-fields nil type))
	 (new-object (make-instance (name super)
				    :super super
				  :uuid uuid
				  :fields fields)))
    (prog1 uuid
      (apply #'initialize new-object initargs)
      (add-object-to-database new-object))))

;;; Serialization support

;; The functions SERIALIZE and DESERIALIZE convert
;; (almost abitrary) trees of Lisp objects (including XELF objects)
;; into printable S-expressions for storing as plain text, and from
;; this printed representation back into living XELF objects.

;; The method names :BEFORE-SERIALIZE and :AFTER-DESERIALIZE are
;; reserved for serialization use. :BEFORE-SERIALIZE, if such a method
;; is present, is invoked before serialization. The object being
;; serialized may use this hook to pre-process its
;; fields. :AFTER-DESERIALIZE is likewise invoked (if present) after
;; reading the object from disk, and is used to recover from
;; deserialization. The reserved field %EXCLUDED-FIELDS is a list of
;; field names (keyword symbols) which are not serialized; typically
;; these will be properly re-initialized by the :AFTER-DESERIALIZE
;; method.

(defconstant +object-type-key+ :%XELF-1%OBJECT%)
(defconstant +uuid-type-key+ :%XELF-1%UUID%)
(defconstant +hash-type-key+ :%XELF-1%HASH%)

(defvar *already-serialized* nil)

(defun serialize (object)
  "Convert a Lisp object a print-ready S-expression.
Invokes :BEFORE-SERIALIZE on XELF objects whenever present. Any fields
named in the field %EXCLUDED-FIELDS will be ignored."
  ;; use labels here so we can call #'serialize
  (with-standard-io-syntax
    (labels ((hash-to-list (hash)
	       (let (plist)
		 (labels ((collect (k v)
			    (push (serialize v) plist)
			    (push k plist)))
		   (maphash #'collect hash)
		   (cons +hash-type-key+ (cons (hash-table-test hash) plist))))))
      (typecase object 
	(hash-table (hash-to-list object))
	;; handle tree structure
	(cons 
	 (if (consp (cdr object)) ;; it's a list
	     (mapcar #'serialize object)
	     (cons (serialize (car object)) ;; it's a dotted pair
		   (serialize (cdr object)))))
	;; handle strings
	(string
	 (if (xelfp object)
	     (cons +uuid-type-key+ (coerce (copy-tree object) 'simple-string))
	     (coerce (copy-tree object) 'simple-string)))
	;; pass other vectors
	(vector (map 'vector #'serialize object))
	;; flatten xelf objects
	(xelf-object (let ((excluded-fields (when (has-field :excluded-fields object)
					 (field-value :excluded-fields object))))
		  ;; possibly prepare object for serialization.
		  ;; (when (has-method :before-serialize object)
		  ;;   (send :before-serialize object))
		  ;; serialize
		  (let ((super-name (class-name (class-of object)))
			(name (name object))
			(uuid (uuid object))
			(fields (fields object))
			(plist nil))
		    (assert (and super-name 
				 (null name)
				 (stringp uuid)))
		    ;; don't duplicate objects already in database
		    (if (and ;; nil ;; removed for the time being
			     (hash-table-p *already-serialized*)
			     (gethash uuid *already-serialized*))
			uuid
			;; just serialize
			(labels ((collect (field value)
				   (unless (member field excluded-fields)
				     (push (serialize value) plist)
				     (push field plist))))
			  ;; make serialized/cleaned plist 
			  (etypecase fields
			    (hash-table (maphash #'collect fields))
			    (list (loop while fields
					;; dissect plist
					do (let ((field (pop fields))
						 (value (pop fields)))
					     (collect field value)))))
			  ;; prevent duplicates
			  (setf (gethash uuid *already-serialized*) t)
			  ;; cons up the final serialized sexp
			  (prog1 
			      (list +object-type-key+
				    :super super-name
				    :uuid uuid
				    :fields plist)
			    ;; handle any serialization cleanup
			    (when (has-method :after-serialize object)
			      (send :after-serialize object))))))))
	;; pass through other Lisp entities
	(otherwise object)))))

(defun deserialize-hash (data test)
    ;; ;; skip hash key and test indicator
    ;; (when (eq +hash-type-key+ (first data))
    ;;   (pop data)
    ;;   ;; skip test indicator
    ;;   (setf test (or (pop data) :list)))
    ;; fill in the hash with what remains
    (let ((plist data)
	  (hash (make-hash-table :test test)))
      (prog1 hash
	(loop while plist do
	  (let* ((key (pop plist))
		 (value (pop plist)))
	    (setf (gethash key hash) (deserialize value)))))))

(defun deserialize-fields (fields &optional (type :list) (test 'eq))
  (ecase type
    (:list (mapcar #'deserialize fields))
    (:hash (deserialize-hash fields test))))

(defun deserialize (data)
  "Reconstruct Lisp objects (including XELF-derived objects) from an
S-expression made by SERIALIZE. Invokes :AFTER-DESERIALIZE on XELF
objects after reconstruction, wherever present."
  (with-standard-io-syntax 
    (cond 
      ;; handle XELF objects
      ((and (listp data) (eq +object-type-key+ (first data)))
       (destructuring-bind (&key super uuid fields &allow-other-keys)
	   (rest data)
	 (let ((object
		 (make-object :fields (deserialize-fields fields)
			      :uuid uuid
			      :super (find-prototype super))))
	   (prog1 object
	     (initialize-method-cache object)
	     ;; possibly recover from deserialization
	     (when (has-method :after-deserialize object)
	       (send :after-deserialize object))))))
      ;; handle hashes
      ((and (listp data) (eq +hash-type-key+ (first data)))
       ;; pass hash table test key
       (deserialize-fields (rest (rest data)) :hash (second data)))
      ;; handle lists
      ((consp data)
       (if (consp (cdr data))
	   ;; it's a list
	   (mapcar #'deserialize data)
	   ;; it's a dotted pair
	   (cons (deserialize (car data))
		 (deserialize (cdr data)))))
      ;; passthru
      (t data))))

(defun initialize%super (object &rest args)
  (apply #'send-super :initialize object args))

(defun initialize%queue (object &rest args)
  (apply #'send-queue :initialize object args))

(defun duplicate (original0)
 (when original0
    (let ((original (find-object original0)))
      (let ((duplicate 
	      (make-instance (name (super original))
	       :super (super original)
	       :uuid (make-uuid))))
	(prog1 duplicate
	  (initialize-method-cache duplicate)
	  (add-object-to-database duplicate)
	  ;; copy any local field values
	  (let* ((fields (fields original))
		 (fields0 fields)
		 names)
	    (if (hash-table-p fields)
		(setf names (loop for f being the hash-keys of fields collect f))
		(dolist (f fields)
		  (push (pop fields) names)
		  (pop fields)))
	    (dolist (name names)
	      (set-field-value name duplicate 
			       (fref fields0 name)))))))))

;;; Buffer list

(defvar *buffers* nil)

(defun initialize-buffers ()
  (setf *buffers* 
	(make-hash-table :test 'equal)))

(initialize-buffers)
    
(defparameter *buffer-delimiter* #\*)

(defun special-buffer-name-p (name)
  (position *buffer-delimiter* name))

(defun verbose-symbol-name (sym &optional other-package)
  (let ((name (symbol-name sym))
	(package (package-name (symbol-package sym))))
    (concatenate 'string 
		 (or other-package package)
		 (string *buffer-delimiter*) 
		 name)))

(defun project-buffer-name (project)
  (assert (not (special-buffer-name-p project)))
  (concatenate 'string project ":"))

(defun prototype-buffer-name (thing)
  (cond
    ((xelfp thing)
     (make-prototype-id thing))
     ;; passthru for existing prototype names
    ((stringp thing)
     thing)))

(defun method-buffer-name (method &optional (object "XELF:BLOCK"))
  (assert (and (symbolp method)
	       (not (keywordp method))))
  (concatenate 'string 
	       (prototype-buffer-name object)
	       (string *buffer-delimiter*)
	       (string-upcase (symbol-name method))))

(defun buffer-name-project (name)
  (when (special-buffer-name-p name)
    (subseq name 0 (position *buffer-delimiter* name))))

(defun buffer-name-prototype (name)
  (when (special-buffer-name-p name)
    (let* ((colon (position *buffer-delimiter* name))
	   (colon2 (position *buffer-delimiter* 
			     (subseq name (1+ colon)))))
      (subseq name 
	      (+ 1 colon) 
	      (when colon2 (+ 1 colon colon2))))))
  
(defun buffer-name-method (name)
  (when (special-buffer-name-p name)
    (let* ((colon (position *buffer-delimiter* name))
	   (remainder (subseq name (1+ colon)))
	   (colon2 (position *buffer-delimiter* remainder)))
      (when (and colon colon2)
	(make-keyword (subseq remainder (1+ colon2)))))))

(defun add-buffer (name object)
  (assert (xelfp object))
  (when (null *buffers*)
    (initialize-buffers))
  (prog1 t
    (setf (field-value :buffer-name object) name)
    (setf (gethash name *buffers*)
	  (find-uuid object))))

(defun find-buffer (name &key create prototype noerror)
  (or (gethash name *buffers*)
      (if create
	  (let ((buffer (new (or prototype 'buffer) name)))
	    (prog1 buffer (add-buffer name buffer)))
	  (unless noerror
	    (error "Cannot find buffer ~S" name)))))

(defun kill-buffer (name)
  (remhash name *buffers*))

;;; Clipboard

(defvar *clipboard* nil)

(defun initialize-clipboard-maybe (&optional force)
  (when (or force (null *clipboard*))
    (setf *clipboard* (new 'buffer))))

(defun clear-clipboard ()
  (initialize-clipboard-maybe :force))

;;; Printing objects

(defun get-some-name (ob)
  (if (null ob)
      "NULL!!"
      (let ((object (find-object ob)))
	(if (object-p object)
	    (find-super-prototype-name object)
	    "Unknown"))))

(defun object-address-string (ob)
  (subseq (find-uuid ob) 0 5))
  ;; (let ((string (with-output-to-string (s)
  ;; 		  (print-unreadable-object (ob s :identity t)))))
  ;;   (subseq string
  ;; 	    (1+ (search "{" string))
  ;; 	    (search "}" string))))

;; (defun print-blx (foo stream)
;;   (let ((object (find-object foo)))
;;     (format stream "#<% ~A ~A>" 
;; 	    (get-some-name object)
;; 	    (object-address-string object))))

;; (defmethod print-object ((foo xelf:xelf-object) stream)
;;   (print-blx foo stream))

;;; prototypes.lisp ends here
