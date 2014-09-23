;;; speech.el --- story frontend to espeak

;; Copyright (C) 2013  David O'Toole

;; Author: David O'Toole <dto@blocky.io>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl) 

(defvar *speech-program* "espeak")

(defvar *speech-parameters* 
  '(:voice "-v"
    :volume "-a"
    :pitch "-p"
    :markup "-m"
    :speed "-s"
    :punctuation "--punct"
    :file "-w"))

(defvar speech-additional-parameters '("--stdin" "-l" "0" "-m"))

(defun make-speech-parameter (entry)
  (destructuring-bind (name . value) entry
    (let ((option (getf *speech-parameters* name)))
      (when option
	(list option 
	      (if (stringp (first value))
		  (first value)
		  (format "%S" (first value))))))))

(defun pairwise (x)
  (when (consp x)
    (cons (list (first x) (second x))
	  (pairwise (rest (rest x))))))

(defun make-speech-parameters (babel-params)
  (apply #'append 
	 (mapcar #'make-speech-parameter
		 (pairwise babel-params))))

(defun speech-command-string (params)
  (mapconcat 'identity 
	     (append (list *speech-program*)
		     speech-additional-parameters
		     (make-speech-parameters params))
	     " "))

(defun speech-file (params) 
  (getf params :file))

(defun speech-render (text params file)
  (let ((command
	  (speech-command-string 
	   (if (null file) 
	       params
	       (setf params (plist-put params :file file))))))
    (with-temp-buffer (insert text)
      (shell-command-on-region (point-min) (point-max) command))
    (message "COMMAND: %S " command)))

(defun speech-play (file)
  (shell-command (format "play %S" file)))

(defvar *screenplay* nil)

(defvar *voices* nil)

(defvar *voice* nil)
(defvar *voice-key* nil)

(defmacro define-voice (name &rest args)
  `(push ',(cons name args) *voices*))

(defun voice-parameters (name &rest args)
  (rest (assoc name *voices*)))

(defmacro with-voice (voice &rest body)
  `(let ((*voice* (voice-parameters ,voice))
	 (*voice-key* ,voice))
     ,@body))

(defun say (key text)
  (let ((file (concat 
	       (substring (symbol-name key) 1)
	       ".wav")))
    (speech-render text *voice* file)
    (speech-play file)
    (push (cons *voice-key* file) *screenplay*)))

;;;;;;;;;;;;;;;;;;;;;

(setf *voices* nil)
(setf *voice* nil)
(setf *voice-key* nil)
(setf *screenplay* nil)


;; (define-voice :alien :pitch 6 :speed 100 :voice mb-sw1-en)

;; (with-voice :alien (say :alien-1 "pi, pi, pi, intruder sense, if. now pursue."))
;; (with-voice :alien (say :alien-2 "halt! stop there."))
;; (with-voice :alien (say :alien-3 "intruder! intruder!"))
;; (with-voice :alien (say :alien-4 "7 6 1 1 pi three seven 1 Y X"))
;; (with-voice :alien (say :alien-5 "charlie tango seven. bravo and cisco are on route."))
;; (with-voice :alien (say :alien-6 "6 3 echelon. this is cisco tango."))
;; (with-voice :alien (say :alien-7 "both are missing. if. now investigate."))

;; (define-voice :sandy :pitch 14 :speed 83 :voice mb-de4-en)
;; (define-voice :navajo :pitch 12 :speed 90 :voice mb-en1)
;; (define-voice :peach :pitch 7 :speed 90 :voice mb-fr4-en)

;; (with-voice :navajo (say :balance-of-power "So, Mister Gold. It
;; seems you are attempting to upset the delicate balance of power
;; between Peach Puff and myself."))

;; (with-voice :peach (say :deception "Yes I'd say it's quite clear,
;; your deception and subterfuge have been unmasked."))

;; (with-voice :sandy (say :can-see-now "Mr. Gold, you were seen
;; this morning distributing pamphlets or papers of an undetermined
;; nature."))

;; (with-voice :navajo (say :drive-us-apart "Perhaps Mr. Gold is
;; trying to sow dissent in an effort to weaken the Neutral Gang's
;; leadership, so that <emphasis>he</emphasis> can sieze power in Two Zong Town!"))

;; (with-voice :peach (say :possible "I think it is quite possible,
;; Mister White."))

;; (with-voice :sandy (say :very-sad "This Commander is very
;; displeased.  I can see now that you Vivids are all alike. We
;; Neutrals have got to stick together, even when we cannot tell
;; each other apart."))

;; (with-voice :navajo (say :now-fight "Now you must die! One color
;; will rule, so as is foretold in the Codex Chromatica! Hoist your
;; Squareball! Prepare for pixellation! There is no Gold in the
;; color of the glorious Tomorrow we are building here in Two Zong Town!"))

(provide 'speech)
;;; speech.el ends here


