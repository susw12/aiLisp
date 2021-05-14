;;; Prof. R. Williams				Artificial Intelligence

;;;;;;;;;;;;;;;;;;;; General A* Search Pseudocode ;;;;;;;;;;;;;;;;;;;;;;

(defstruct node
  path
  path-length
  total-length-estimate
  )

(defun find-shortest-path (start goal successors heuristic-dist)
  (do (head-node		; node at head of open list
       path-to-extend	        ; path to state currently visited
       current-state		; state currently visited
       dist-so-far		; length of this path
       extended-paths	        ; list of newly extended paths
       (open			; list of all candidate nodes
	(list (make-node :path (list start)
			 :path-length 0
			 :total-length-estimate
			 (funcall heuristic-dist start goal))))
       (state-distances (make-hash-table :test #'equalp))
       )
      ((null open) nil)	       ; if open list is empty, search fails
     (format t                 ; lets us watch how algorithm works
		"~%open: ~s~%" open)
      (setq head-node (pop open))       ; get node at head of open list
      (setq path-to-extend (node-path head-node)) ; get path itself
      (setq current-state (car path-to-extend)) ; get state this path ends at
      (if (equalp current-state goal)
	  (return head-node))	; success: return path and length found
      (setq dist-so-far (node-path-length head-node))
      (when (less-than dist-so-far (gethash current-state state-distances))
	 (setf (gethash current-state state-distances) dist-so-far)
	 (let (next-state
	       next-dist-so-far
	       (next-nodes nil))
	   (dolist (pair (funcall successors current-state))
	     (setq next-state (car pair))
	     (setq next-dist-so-far (+ (cdr pair) dist-so-far))
	     (if (less-than next-dist-so-far
			    (gethash next-state state-distances))
		 (setf open
		       (merge
			'list
			(list
			 (make-node
			  :path (cons next-state path-to-extend)
			  :path-length next-dist-so-far
			  :total-length-estimate
			  (+ next-dist-so-far
			     (funcall heuristic-dist next-state goal))))
			open
			#'<
			:key #'node-total-length-estimate)))
		)))
      ))


;;; Here the y argument may be nil, which is treated like infinity.

(defun less-than (x y)
  (or (null y) (< x y)))


;;; Designed to test the code in "a-star-hash.lisp" for finding shortest paths
;;; on the small graph discussed in class.  For simplicity, this has been
;;; designed to work usefully only when the goal is state G, but any state
;;; can be used used as a start state.  To find a shortest path from S to G,
;;; for example, evaluate
;;; (find-shortest-path 's 'g #'successors #'heuristic-dist).

(defun successors (state)
  (case state
	((s) '((a . 3) (d . 5)))
	((a) '((s . 3) (b . 4) (d . 5)))
	((b) '((a . 4) (c . 4) (e . 5)))
	((c) '((b . 4)))
	((d) '((s . 4) (a . 5) (e . 2)))
	((e) '((b . 5) (d . 2) (f . 4)))
	((f) '((e . 4) (g . 3)))
	((g) '((f . 3)))
	))

(defun heuristic-dist (state goal)
  (if (eql goal 'g)			; designed to do something interesting
      (case state			;  only when goal state is G
	    ((s) 11.0)
	    ((a) 10.4)
	    ((b) 6.7)
	    ((c) 4.0)
	    ((d) 8.8)
	    ((e) 6.9)
	    ((f) 3.0)
	    ((g) 0.0)
	    )
       0.0				; default is 0 for any other goal state
       ))