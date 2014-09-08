;;; path.lisp --- A* pathfinding for XELF

;; Copyright (C) 2009, 2011, 2012  David O'Toole

;; Author: David O'Toole %dto@blocky.io
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
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Commentary:

;; What follows is an implementation of the well-known A* pathfinding
;; algorithm on a rectangular grid.

;; http://en.wikipedia.org/wiki/A-star_search_algorithm

;;; Code:

(in-package :xelf)

(defstruct path
  finder ;; Who is finding this path? 
  buffer ;; Pointer to associated buffer. 
  grid ;; Array of pathfinding data pnodes.
  height width
  heap ;; Heap array of open pathfinding pnodes.
  end ;; Pointer to last heap array position.
  turn ;; Integer turn number
  )

(defparameter *path-grid-resolution* 180)

(defun row-to-y (path row) 
  (let ((cy (/ (%height (path-buffer path))
	       (path-height path))))
    (cfloat (* cy row))))

(defun column-to-x (path column) 
  (let ((cx (/ (%width (path-buffer path))
	       (path-width path))))
    (cfloat (* cx column))))

(defun within-buffer-boundaries-p (buffer top left right bottom)
  (with-fields (height width) buffer
    (and (<= 0 left right width)
	 (<= 0 top bottom height))))

(defun obstructed (path row column)
  (with-fields (height width) 
      (path-buffer path)
    (let ((*quadtree* (%quadtree (path-buffer path)))
	  (border 8))
      (multiple-value-bind (top left right bottom)
	  (bounding-box (path-finder path))
	(let* ((utop (row-to-y path row))
	       (uleft (column-to-x path column))
	       (vtop (- utop border))
	       (vleft (- uleft border))
	       (vright (+ border vleft (- right left)))
	       (vbottom (+ border vtop (- bottom top))))
	  (if 
	   (not (within-buffer-boundaries-p (current-buffer) vtop vleft vright vbottom))
	   t
	   (block colliding
	     (flet ((check (object)
		      (when (and (xelfp object)
				 (field-value :collision-type object)
				 (not (object-eq object (path-finder path)))
				 (will-obstruct-p object (path-finder path)) )
			(return-from colliding object))))
	       (prog1 nil
		 (quadtree-map-collisions 
		  (cfloat vtop)
		  (cfloat vleft)
		  (cfloat vright)
		  (cfloat vbottom) #'check))))))))))

(defstruct pnode 
  row 
  column
  parent ; previous pnode along generated path
  F ; pnode score, equal to G + H
  G ; movement cost to move from starting point
    ; to (row, column) along generated path
  old-G ; previous value of G
  H ; heuristic cost to reach goal from (row, column)
  closed ; equal to path's path-turn-number when on closed list
  open ; equal to path's path-turn-number when on open list
  )

(defun create-path (finder &key
			 (height *path-grid-resolution*)
			 (width *path-grid-resolution*) 
			 (buffer (current-buffer)))
  (assert (xelfp buffer))
  (let ((path (make-path :buffer buffer
			 :finder finder
			 :grid (make-array (list height width))
			 :heap (make-array (* height width))
			 :height height
			 :width width
			 :turn 1 :end 0)))
    (prog1 path
      (dotimes (r height)
	(dotimes (c width)
	  (setf (aref (path-grid path) r c)
		(make-pnode :row r :column c)))))))
			       
;; The following routines maintain the open and closed sets. We
;; use a minheap to store the open set.

(defun open-pnode (path pnode)
  (let* ((path-heap-end (if (null (path-end path))
			    (setf (path-end path) 1)
			    (incf (path-end path))))
	 (path-heap (path-heap path))
 	 (ptr path-heap-end)
	 (parent nil)
	 (finished nil))
    ;; make it easy to check whether pnode is open
    (setf (pnode-open pnode) (path-turn path))
    ;; add pnode to end of heap 
    (setf (aref path-heap path-heap-end) pnode)
    ;; let pnode rise to appropriate place in heap
    (while (and (not finished) (< 1 ptr))
      (setf parent (truncate (/ ptr 2)))
      ;; should it rise? 
      (if (< (pnode-F pnode) (pnode-F (aref path-heap parent)))
	  ;; yes. swap parent and pnode
	  (progn 
	    (setf (aref path-heap ptr) (aref path-heap parent))
	    (setf ptr parent))
	  ;; no. we're done.
	  (progn (setf finished t)
		 (setf (aref path-heap ptr) pnode))))
    ;; do we need to set pnode as the new root? 
    (if (and (not finished) (equal 1 ptr))
	(setf (aref path-heap 1) pnode))))

(defun close-pnode (path)
  (let* ((path-heap (path-heap path))
	 ;; save root of heap to return to caller
	 (pnode (aref path-heap 1))
	 (last nil)
	 (path-heap-end (path-end path))
	 (ptr 1)
	 (left 2)
	 (right 3)
	 (finished nil))
    ;; is there only one pnode?
    (if (equal 1 path-heap-end)
	(setf (path-end path) nil)
      (if (null path-heap-end)
	  nil
	;; remove last pnode of heap and install as root of heap
	 (progn
	   (setf last (aref path-heap path-heap-end))
	   (setf (aref path-heap 1) last)
	   ;; shrink heap
	   (decf (path-end path))
	   (decf path-heap-end)
	   ;;
	   (setf (pnode-closed pnode) (path-turn path))
	   ;;
	   ;; figure out where former last element should go
	   ;;
	   (while (and (not finished) (>= path-heap-end right))
	     ;;
	     ;; does it need to sink? 
	     (if (and (< (pnode-F last) (pnode-F (aref path-heap left)))
		      (< (pnode-F last) (pnode-F (aref path-heap right))))
		 ;;
		 ;; no. we're done
		 (progn 
		   (setf finished t)
		   (setf (aref path-heap ptr) last))
		 ;;
		 ;; does it need to sink rightward?
		 (if (>= (pnode-F (aref path-heap left)) 
			 (pnode-F (aref path-heap right)))
		     ;;
		     ;; yes
		     (progn
		       (setf (aref path-heap ptr) (aref path-heap right))
		       (setf ptr right))
		     ;;
		     ;; no, sink leftward
		     (progn
		       (setf (aref path-heap ptr) (aref path-heap left))
		       (setf ptr left))))
	     (setf left (* 2 ptr))
	     (setf right (+ 1 left)))
	   ;;
	   ;; 
	   (if (and (equal left path-heap-end)
		    (> (pnode-F last)
		       (pnode-F (aref path-heap left))))
	       (setf ptr left)))))
	;;
	;; save former last element in its new place
	(setf (aref path-heap ptr) last)
    pnode))

;; The ordinary distance algorithm is used to score pnodes.

(defun score-pnode (path pnode path-turn-number new-parent-pnode goal-row goal-column)
  "Update scores for PNODE. Update heap position if necessary."
  (let* ((direction (direction-to (pnode-column new-parent-pnode)
				  (pnode-row new-parent-pnode)
				  (pnode-column pnode)
				  (pnode-row pnode)))
	 (G (+ 1 (pnode-G new-parent-pnode)))
	 
 	 (H (* (distance (pnode-column pnode)
			 (pnode-row pnode)
			 goal-column goal-row)
		;; (max (abs (- (pnode-row pnode) goal-row))
		;;     (abs (- (pnode-column pnode) goal-column)))
	       1))
	 (F (+ G H)))
    ;; 
    ;; is this a new pnode, i.e. not on the open list? 
    (if (not (equal path-turn-number (pnode-open pnode)))
	;;
	;; yes, update its scores and parent
	(progn 
	  (setf (pnode-G pnode) G)
	  (setf (pnode-H pnode) H)
	  (setf (pnode-F pnode) F)
	  (setf (pnode-parent pnode) new-parent-pnode))
      ;;
      ;; no, it's already open. is the path through NEW-PARENT-PNODE
      ;; better than through the old parent?
      (if (and (pnode-G pnode)
	       (< G (pnode-G pnode)))
	  ;;
	  ;; yes. update scores and re-heap.
	  (let ((heap (path-heap path))
		(heap-end (path-end path))
		(ptr 1)
		(par nil)
		(finished nil))
	    (setf (pnode-G pnode) G)
	    (setf (pnode-H pnode) H)
	    (setf (pnode-F pnode) F)
	    (setf (pnode-parent pnode) new-parent-pnode)
	    ;;
	    ;; Better score found.
	    ;; 
	    ;; find current location of pnode in heap
	    (while (and (not finished) (< ptr heap-end))
	      (when (equal pnode (aref heap ptr))
		;; Found pnode.
		;;
		;; its score could only go down, so move it up in the
		;; heap if necessary.
		(while (and (not finished) (< 1 ptr))
		  (setf par (truncate (/ ptr 2)))
		  ;;
		  ;; should it rise? 
		  (if (< (pnode-F pnode) (pnode-F (aref heap par)))
		      ;;
		      ;; yes. swap it with its parent
		      (progn
			(setf (aref heap ptr) (aref heap par))
			(setf ptr par))
		    ;;
		    ;; no, we are done. put pnode in its new place.
		      (progn (setf finished t)
			     (setf (aref heap ptr) pnode))))
		;;
		;; do we need to install the new pnode as heap root?
		(when (and (not finished) (equal 1 ptr))
		  (setf (aref heap 1) pnode)))
	      ;;
	      ;; keep scanning heap for the pnode
	      (incf ptr)))
	;;
	;; new score is not better. do nothing.
					;(setf (pnode-parent pnode) new-parent-pnode)
	  ))))
	      
(defun pnode-successors (path pnode path-turn-number goal-row goal-column)
  (delete nil 
	(mapcar 
	 #'(lambda (direction)
	     (let ((grid (path-grid path))
		   (new-G (+ 1 (pnode-G pnode)))
		   (successor nil))
	       (multiple-value-bind (r c) 
		   (step-in-direction 
		    (pnode-row pnode)
		    (pnode-column pnode)
		    direction)
		 ;; 
		 (if (array-in-bounds-p grid r c)
		     (progn 
		       (setf successor (aref grid r c))
		       
		       (if (or 
			    ;; always allow the goal square even when it's an obstacle.
			    (and (equal r goal-row) (equal c goal-column))
			    ;; ignore non-walkable squares and closed squares,
			    (and (not (obstructed path r c))
				 (not (equal path-turn-number (pnode-closed successor)))))
			   ;; if successor is open and existing path is better
			   ;; or as good as new path, destroy the successor
			   ;; if successor is not open, proceed 
			   (if (equal path-turn-number (pnode-open successor))
			       (if (< new-G (pnode-G successor))
				   successor
				   nil)
			       successor)
			   nil))
		     nil))))
	 *directions*)))
  
  ;; Now we come to the pathfinding algorithm itself. 

(defun address-to-waypoint (path address)
  (destructuring-bind (row column) address
    (list (round (column-to-x path column))
	  (round (row-to-y path row)))))

(defun find-path (path x0 y0 x1 y1)
  "Find a path from the starting point to the goal in PATH using A*.
Returns a list of directional keywords an AI can follow to reach
the goal."
  (let* ((selected-pnode nil)
	 (path-turn-number (incf (path-turn path)))
	 (pos nil)
	 (found nil)
	 (path-height (path-height path))
	 (path-width (path-width path))
	 (buffer-height (%height (path-buffer path)))
	 (buffer-width (%width (path-buffer path)))
	 (cx (/ buffer-width path-width))
	 (cy (/ buffer-height path-height))
	 (target-pnode nil)
	 (coordinates nil)
	 (F 0) (G 0) (H 0)
	 (starting-row (round (/ y0 cy)))
	 (starting-column (round (/ x0 cx)))
	 (goal-row (round (/ y1 cy)))
	 (goal-column (round (/ x1 cx))))
    (if (obstructed path goal-row goal-column)
	(prog1 nil);; (message "Not pathfinding to obstructed area."))
	(progn 
	  ;; reset the pathfinding heap
	  (setf (path-end path) nil)
	  ;; add the starting pnode to the open set
	  (setf G 0)
	  (setf H (max (abs (- starting-row goal-row))
		       (abs (- starting-column goal-column))))
	  (setf F (+ G H))
	  (setf selected-pnode (make-pnode :row starting-row 
					 :column starting-column
					 :old-G 0
					 :parent nil :G G :F F :H H))
	  ;;
	  (open-pnode path selected-pnode)
	  ;; start pathfinding
	  (setf target-pnode
		(block finding
		  ;; select and close the pnode with smallest F score
		  (while (setf selected-pnode (close-pnode path))
		    ;; did we fail to reach the goal? 
		    (when (null selected-pnode)
		      (return-from finding nil))
		    ;; are we at the goal square?
		    (when (and (equal goal-row (pnode-row selected-pnode))
			       (equal goal-column (pnode-column selected-pnode)))
		      (return-from finding selected-pnode))
		    ;; process adjacent walkable non-closed pnodes
		    (mapc #'(lambda (pnode)
			      ;; is this cell already on the open list?
			      (if (equal path-turn-number (pnode-open pnode))
				  ;; yes. update scores if needed
				  (score-pnode path pnode path-turn-number
					      selected-pnode goal-row goal-column)
				  (progn 
				    ;; it's not on the open list. add it to the open list
				    (score-pnode path pnode path-turn-number selected-pnode
						goal-row goal-column)
				    (open-pnode path pnode))))
			  ;; map over adjacent pnodes
			  (pnode-successors path selected-pnode 
					   path-turn-number
					   goal-row goal-column)))))
	  ;; did we find a path? 
	  (if (pnode-p target-pnode)
	      ;; save the path by walking backwards from the target
	      (let ((previous-pnode target-pnode)
		    (current-pnode nil))
		(while (setf current-pnode (pnode-parent previous-pnode))
		  ;; what direction do we travel to get from current to previous? 
		  (push (list (pnode-row current-pnode)
			      (pnode-column current-pnode))
			coordinates)
		  (setf previous-pnode current-pnode))
		;; return the finished path
		coordinates)
	      ;; return nil
	      nil)))))
    
(defun find-path-waypoints (path x0 y0 x1 y1)
  (mapcar #'(lambda (address)
	      (address-to-waypoint path address))
	  (find-path path (truncate x0) 
		     (truncate y0)
		     (truncate x1)
		     (truncate y1))))

;; (defun print-path (foo stream)
;;   (format stream "#<% XELF PATH>"))

;; (defmethod print-object ((foo xelf::path) stream)
;;   (print-path foo stream))

;;; path.lisp ends here
