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

(defvar *symbol-count* 0)

(defun methodp (symbol)
  (fboundp symbol))

(defun heading (level text stream)
  (fresh-line stream) 
  (format stream "~A ~A" 
   (make-string level :initial-element (character "*"))
   text)
  (fresh-line stream))

(defun document-function (symbol stream)
  (let ((doc (documentation symbol 'function)))
    (heading 2 (format nil "~A (~A)" symbol (if (macro-function symbol) "macro"
						(if (find-class symbol nil)
						    "class"
						    "function")))
	     stream)
    (heading 3 "Arguments" stream)
    (format stream "~S" (sb-introspect:function-lambda-list (fdefinition symbol)))
    (when doc
      (incf *symbol-count*)
      (heading 3 "Documentation" stream)
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

(defun preamble-file-lines (preamble-file)
  (with-open-file (file preamble-file
			:direction :input
			:if-does-not-exist nil)
    (let* ((len (file-length file))
           (string (make-string len)))
      (read-sequence string file)
      (split-string-on-lines string))))

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
    (fresh-line stream)
    (do-external-symbols (symbol package)
      (push symbol symbols))
    ;; remove method symbols and method defun symbols
    (setf symbols (sort symbols #'string<))
    ;; print preamble
    (let ((preamble-lines 
	    (when preamble-file
	      (preamble-file-lines preamble-file))))
      (when preamble-file 
	(dolist (line preamble-lines)
	  (format stream "~A " line)
	  (fresh-line stream))))
    (dolist (sym symbols)
      (if (fboundp sym)
	  (document-function sym stream)
	  (document-variable sym stream)))
    (message "Documented ~S of ~S symbols." *symbol-count* (length symbols))))

(defun document-package-to-file (package-name output-file &key preamble-file title)
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (document-package package-name :title title :stream stream :preamble-file preamble-file)))

;; (document-function 'align-to-pixels t)
;; (documentation 'align-to-pixels 'function)
;; (document-package-to-file :xelf #P"/home/dto/ioweb/xelf-manual.org" :title "Xelf documentation dictionary")

;;; doc.lisp ends here
