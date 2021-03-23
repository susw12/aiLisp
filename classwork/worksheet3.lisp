; 1) a)
(defun mystery-fun-4 (x) 
    (do ((i (1- x) (1- i)) ; Sets i to a value one less than x, the given value. Each iteration, i is reduced by one.
        (result x)) ; Sets result to x
        ((= 1 i) result) ; If i equals one, then returns result and breaks. Otherwise, it continues to the next statement.
        (setq result (* result i)))) ; Since i did not equal one, result now equals the previous value of result * the current value of i


; 1) b)
(defun mystery-fun-5 (x) 
    (do ((new-list x (cdr new-list)) ; Defines new-list to point to x. Each iteration, new-list loses its first element.
        (counter 0 (+ 1 counter))) ; Defines counter to be 0. Each iteration, counter increases by 1.
        ((null new-list) counter))) ; It checks to see if new-list is empty. If it is, it breaks. Otherwise, it continues the loop.

; 1) c)
(defun mystery-fun-6 (x) 
    (do ((to-do x (cdr to-do)) ; Defines to-do to point to x. Each iteration, to-do loses its first element
        (new-list nil (cons (car to-do) new-list))) ; Defines new-list to nil. Each iteration, new-list adds the first element of to-do to it, effectively appending it.
        ((null to-do) new-list))) ; It checks to see if to-do is empty. If it is, it breaks. Otherwise, it continues the loop.

; 2)

