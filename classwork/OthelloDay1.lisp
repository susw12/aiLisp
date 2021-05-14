;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;NAME: 
;;QUESTIONS:
;1.  Explain the parameters and function of simple-array*, aref* and bref (* built in to LISP - see function index).
;2.  Which Othello.lisp function returns a list of all valid positions?
;3.  Which Othello.lisp function returns a list of all legal positions for the current player?
;4.  What does the 'make-move' function do? 
;5.  What does 'make-move' return? 
;6.  Does 'make-move' modify the objects that are passed to it? 
;7.  If you wanted to avoid modifying them (e.g. to test a move), what would you need to do first?  
;8.  Explain how the any-legal-moves function works.
;9.  Find and describe the basic strategies contained in the code.
;10. Explain the 'count-difference' function - what is it used for?

(defvar all-directions '(-11 -10 -9 -1 1 9 10 11))

(defconstant empty 0 "An empty square")
(defconstant black 1 "A black piece")
(defconstant white 2 "A white piece")
(defconstant outer 3 "Marks squares outside the 8x8 board")

(deftype piece () `(integer ,empty ,outer))

(defun name-of-piece (piece) (char ".@O?" piece))

(defun opponent (player) (if (eql player black) white black))

(deftype board () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val) 
  `(setf (aref ,board ,square) ,val))

(defun copy-board (board)
  (copy-seq board))

(defvar all-squares
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

(defun initial-board ()
  "Return a board, empty except for four pieces in the middle."
  ;; Boards are 100-element vectors, with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER.  Initially
  ;; the 4 center squares are taken, the others empty.
  (let ((board (make-array 100 :element-type 'piece
                           :initial-element outer)))
    (dolist (square all-squares)
      (setf (bref board square) empty))
    (setf (bref board 44) white   (bref board 45) black
          (bref board 54) black   (bref board 55) white)
    board))

(defun count-difference (player board)
  (- (count player board)
     (count (opponent player) board)))

(defun valid-p (move)
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (and (eql (bref board move) empty)
       (some #'(lambda (dir) (would-flip? move player board dir))
             all-directions)))

(defun make-move (move player board)
  (setf (bref board move) player)
  (dolist (dir all-directions)
    (make-flips move player board dir))
  board)

(defun make-flips (move player board dir)
  "Make any flips in the given direction."
    (let ((bracketer (would-flip? move player board dir)))
 (when bracketer
      	(do ((c (+ move dir)  (+ c dir))) ((eql c bracketer) nil)
	  (setf (bref board c) player))))
  )
         ; (when bracketer
   ;   (loop for c from (+ move dir) by dir until (eql c bracketer)
   ;         do  (format t ":~a " c)
   ;         (setf (bref board c) player)))))

(defun would-flip? (move player board dir)
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond ((eql (bref board square) player) square)
        ((eql (bref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (t nil)))

(defun next-to-play (board previous-player print)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
          ((any-legal-move? previous-player board) 
           (when print
             (format t "~&~c has no moves and must pass."
                     (name-of-piece opp)))
           previous-player)
          (t nil))))

(defun any-legal-move? (player board)
   (some #'(lambda (move) (legal-p move player board))
        all-squares))

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

(defun random-strategy (player board)
  (random-elt (legal-moves player board)))

(defun legal-moves (player board)
 (loop for move in all-squares
	when (legal-p move player board) collect move))
 
(defconstant winning-value most-positive-fixnum)
(defconstant losing-value  most-negative-fixnum)

(defun final-value (player board)
 (case (signum (count-difference player board))
    (-1 losing-value)
    ( 0 0)
    (+1 winning-value)))
 
(defun human (player board)
  "A human player for the game of Othello"
  (format *query-io* "~&~c to move: " (name-of-piece player))
  (force-output *query-io*)
  (parse-integer (read-line *query-io*))
  )
 
(defvar *move-number* 1 "The number of the move to be played")

(defun othello (bl-strategy wh-strategy 
                &optional (print t) (minutes 30))
  "Play a game of othello.  Return the score, where a positive
  difference means black, the first player, wins."
  (let ((board (initial-board))
        (clock (make-array (+ 1 (max black white))
                           :initial-element 
                           (* minutes 60 
                              internal-time-units-per-second))))
    (catch 'game-over
      (loop for *move-number* from 1
            for player = black then (next-to-play board player print)
            for strategy = (if (eql player black) 
                               bl-strategy
                               wh-strategy)
            until (null player)
            do (get-move strategy player board print clock))
      (when print
        (format t "~&The game is over.  Final result:")
        (print-board board clock))
      (count-difference black board))))

(defvar *clock* (make-array 3) "A copy of the game clock")
(defvar *board* (initial-board) "A copy of the game board")

(defun get-move (strategy player board print clock)
   (when print (print-board board clock))
  (replace *clock* clock)
  (let* ((t0 (get-internal-real-time))
         (move (funcall strategy player (replace *board* board)))
         (t1 (get-internal-real-time)))
    (decf (elt clock player) (- t1 t0))
    (cond
      ((< (elt clock player) 0)
       (format t "~&~c has no time left and forfeits."
               (name-of-piece player))
       (THROW 'game-over (if (eql player black) -64 64)))
      ((eq move 'resign)
       (THROW 'game-over (if (eql player black) -64 64)))
      ((and (valid-p move) (legal-p move player board))
       (when print
         (format t "~&~c moves to ~a." 
                 (name-of-piece player) move))
       (make-move move player board))
      (t (warn "Illegal move: ~a" move)
         (get-move strategy player board print clock)))))

(defun print-board (&optional (board *board*) clock)
  "Print a board, along with some statistics."
  ;; First print the header and the current score
  (format t "~2&    1 2 3 4 5 6 7 8   [~c=~2a ~c=~2a (~@d)]"
          (name-of-piece black) (count black board)
          (name-of-piece white) (count white board)
          (count-difference black board))
  ;; Print the board itself
  (loop for row from 1 to 8 do
        (format t "~&  ~d " row)
        (loop for col from 1 to 8
              for piece = (bref board (+ col (* 10 row)))
              do (format t "~c " (name-of-piece piece))))
  )
 

;(othello  #'random-strategy #'random-strategy)