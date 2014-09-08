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

(defmacro with-fields (fields object &body body)
  `(with-slots ,fields ,object ,@body))

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

;;; Fundamental class

(defclass xnode ()
  ((uuid :initform nil :accessor uuid :initarg :uuid)))

(defun object-p (x)
  (typep x 'xelf:xnode))

(defun find-uuid (object)
  (when object
    (uuid (find-object object))))

(defun xelfp (thing)
  (or (xelf::object-p thing)
      (and (stringp thing)
	   (find-object thing :no-error))))

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

(defun find-object-by-uuid (uuid &optional noerror)
  (or (gethash (the simple-string uuid) *database*)
      (unless noerror
	(error "Cannot find object for uuid ~S" uuid))))
		  
;;; Finding any object by proto-name or UUID

(defun find-object (thing &optional no-error) 
  (when (not (null thing))
    (let ((result 
	    (etypecase thing
	      (string (or (find-object-by-uuid thing :noerror)
			  (find-prototype thing :noerror)))
	      (xnode thing))))
      (prog1 result
	(unless no-error
	  (when (null result)
	    (error "Cannot find object: ~S" thing)))))))
      
(defun object-eq (a b)
  (when (and a b)
    (eq (find-object a)
	(find-object b))))

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

;;; Obsolete field reference syntax

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
	`(slot-value self
	  ',(make-non-keyword
	     ;; strip percent sign 
	    (subseq (symbol-name symbol) 1)))))

(defun field-value (field object)
  (slot-value object field))

(defun set-field-value (field object value)
  (setf (slot-value object field) value))

(defsetf field-value set-field-value)

(defun make-accessor-flet-clause (symbol)
  `(,symbol (thing)
	    (field-value ,symbol thing)))

(defun transform-method-body (body)
  (let (fields)
    ;; collect %foo symbols used in body
    (transform-tree #'field-reference-p
		    #'(lambda (symbol)
			;; don't modify input
			(prog1 symbol
			  ;; just collect
			  (pushnew symbol fields)))
		    body)
    ;; arrange for the substitution
    `(symbol-macrolet 
	 ,(mapcar #'make-accessor-macrolet-clause fields)
       ,@body)))

(defmacro with-local-fields (&body body)
  (transform-method-body body))

(defmacro define-method
    (method-specifier prototype-name arglist &body method-body)
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
	   (field-name (make-keyword method-name))
	   (method-symbol-name (symbol-name method-name)))
      `(defmethod ,(intern method-symbol-name) ((self ,(or prototype-name 'node)) ,@arglist)
	 ,body2))))

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

(defun plist-to-descriptors (plist)
  (let (descriptors)
    (loop while plist do
      (let* ((field (pop plist))
	     (value (pop plist)))
	(push (list field :initform value :initarg (make-keyword field) :accessor field)
	      descriptors)))
    (nreverse descriptors)))

;; (plist-to-descriptors '(:a 1 :b 2))
	
(defmacro define-prototype (name
			    (&key super 
				  documentation
				  &allow-other-keys)
			    &body declarations)
  (let* ((pre-descriptors (if (symbolp (first declarations))
			      (plist-to-descriptors declarations)
			      declarations))
	 (descriptors (mapcar #'transform-declaration 
			      pre-descriptors)))
    `(progn  
       (defclass ,name ,(when super (list super)) ,pre-descriptors))))
  
(defmethod duplicate ((xnode xnode) &rest initargs &key &allow-other-keys)
  (let* ((class (class-of xnode))
	 (new-xnode (allocate-instance class))
	 (slots (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class))))
    (dolist (slot-name slots) 
      (when (slot-boundp xnode slot-name)
	(setf (slot-value new-xnode slot-name) (slot-value xnode slot-name))))
    (apply #'reinitialize-instance new-xnode initargs)))

;;; Clipboard

(defvar *clipboard* nil)

(defun initialize-clipboard-maybe (&optional force)
  (when (or force (null *clipboard*))
    (setf *clipboard* (make-instance 'buffer))))

(defun clear-clipboard ()
  (initialize-clipboard-maybe :force))

;;; prototypes.lisp ends here
