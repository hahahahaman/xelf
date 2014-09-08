;;; doc.lisp --- extract xelf docs into orgmode format

;; Copyright (C) 2009-2011, 2012, 2013  David O'Toole

;; Author: David O'Toole dto@ioforms.org
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

(in-package :xelf)

(defun get-symbols ()
  (let (symbols)
    (do-external-symbols (symbol (find-package :xelf))
      (push symbol symbols))
    symbols))

(defun symbol-category (symbol)
  (let ((entry (find symbol *symbol-categories* :key #'first)))
    (when entry (second entry))))

(defun all-categories ()
  (let (categories)
    (dolist (entry *symbol-categories*)
      (when (second entry)
	(pushnew (second entry) categories)))
    categories))

(defun find-symbols-in-category (category)
  (labels ((p (sym)
	     (eq category (second (find sym *symbol-categories* :key #'first)))))
    (sort (remove-if-not #'p (get-symbols))
	  #'string<)))

(defvar *categories*
  '(BUFFERS RESOURCES PATHFINDING EVENTS MOVEMENT DRAWING
	 COLLISION-DETECTION TEXT LIFECYCLE KEYBOARD MOUSE
	 JOYSTICK SOUND MATH HOOKS nil))

(defun category-name (category)
  (if (null category)
      "System"
      (string-capitalize (string-downcase (pretty-string (symbol-name category))))))
      
(defun find-symbol-categories ()
  (mapc #'find-symbols-in-category *categories*))

(defvar *symbol-count* 0)

(defun methodp (symbol)
  (fboundp symbol))

(defun heading (level text stream)
  (fresh-line stream) 
  (format stream "~A ~A" 
   (make-string level :initial-element (character "*"))
   text)
  (fresh-line stream))

(defun arguments-prefix (stream) 
  (format stream " "))

(defun document-function (symbol stream)
  (let ((doc (documentation symbol 'function)))
    (heading 2 (format nil "~A (~A)" symbol (if (macro-function symbol) "macro"
						(if (find-class symbol nil)
						    "class"
						    (if (typep (fdefinition symbol) 'standard-generic-function)
							"generic function"
							"function"))))
	     stream)
    (arguments-prefix stream)
    (fresh-line stream)
    (fresh-line stream)
    (format stream ": ~S" (sb-introspect:function-lambda-list (fdefinition symbol)))
    (when doc
      (incf *symbol-count*)
      (fresh-line stream)
      (format stream "~A" doc)
      (fresh-line stream))))

(defun document-variable (symbol stream)
  (heading 2 (format nil "~A (variable)" symbol)
	   stream)
  (when (documentation symbol 'variable)
    (incf *symbol-count*)
    (heading 3 "Documentation" stream)
    (format stream "~A" (documentation symbol 'variable)))
  (fresh-line stream))

(defun document-category (category stream)
  (fresh-line stream)
  (heading 1 (category-name category) stream)
  (fresh-line stream)
  (dolist (sym (find-symbols-in-category category))
    (if (fboundp sym)
	(document-function sym stream)
	(document-variable sym stream))))

(defun preamble-file-lines (preamble-file)
  (with-open-file (file preamble-file
			:direction :input
			:if-does-not-exist nil)
    (let* ((len (file-length file))
           (string (make-string len)))
      (read-sequence string file)
      (split-string-on-lines string))))

(defun make-defgeneric (sym stream)
  (format stream "(defgeneric ~A" (string-downcase (symbol-name sym)))
  (fresh-line stream)
  (format stream "  ~A" (sb-introspect:function-lambda-list (fdefinition sym)))
  (fresh-line stream)
  (format stream "\" \")")
  (fresh-line stream)
  (fresh-line stream)
  (fresh-line stream))

(defun document-package (package-name &key (stream t) preamble-file title)
  (let ((package (find-package package-name))
	symbols functions variables
	(*symbol-count* 0))
    ;; header
    (when title 
      (format stream "#+TITLE: ~A" title)
      (fresh-line stream))
    (format stream "#+OPTIONS: toc:2 *:nil")
    (fresh-line stream)
    (format stream "#+INFOJS_OPT: view:info toc:t tdepth:1")
    (fresh-line stream)
    (fresh-line stream)
    (do-external-symbols (symbol package)
      (push symbol symbols))
    ;; print preamble
    (let ((preamble-lines 
	    (when preamble-file
	      (preamble-file-lines preamble-file))))
      (when preamble-file 
	(dolist (line preamble-lines)
	  (format stream "~A " line)
	  (fresh-line stream))))
    (dolist (category *categories*)
      (document-category category stream))
    (message "Documented ~S of ~S symbols." *symbol-count* (length symbols))))

(defun document-package-to-file (package-name output-file &key preamble-file title)
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (document-package package-name :title title :stream stream :preamble-file preamble-file)))

;; (document-function 'align-to-pixels t)
;; (documentation 'align-to-pixels 'function)
(document-package-to-file 
 :xelf
 #P"/home/dto/ioweb/reference.org"
 :title "Xelf documentation reference"
 :preamble-file #P"/home/dto/ioweb/preamble.org")

;;; doc.lisp ends here
