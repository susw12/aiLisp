; Sujay Swain
; Worksheet 3
; Note: The answers for the tracing and explanation are here as comments.

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
        ((null to-do) new-list))) ; It checks to see if to-do is empty. If it is, it breaks. Otherwise, it continues the loop. If it breaks, then the list returns backwards.

; 2)
(defun mystery-fun-4-update (x)
    (let ((result 1))
        (dotimes (counter (- x 1) result)
            (setf result (* (- x counter) result)))))

; 3)
(defun mystery-fun-5-update (x)
    (let ((result 0))
        (dolist (next x result)
            (setf result (1+ result)))))

; 4)
(defun mystery-fun-6-update (x)
    (let ((result nil))
        (dolist (next x result)
            (setf result (cons next result)))))

; 5) a)
(loop for x in '(do re me fa sol la te do)
    do (print x))

; 5) b)
(loop for x from 0 to 3
    do (print x)
)

;6
(defun question-6 (input)
    (setq f nil) ; Sets f to a nil list
    (dolist (w input) ; It takes in the input list provided by the user, along with a variable "w" which represents the current element it is reading
        (cond ((numberp w)) ; It checks to see if "w" is a number. If it is it does nothing, (essentially this is checking to see if the element is not a number).
        (t (setq f (cons w f))))) ; If "w" was not a number, i.e. the previous statement failed, it appends that element to the list f, which we defined earlier.
f) ; This does nothing? Looks like a typo

(question-6 '(numbers -945 34 are my -45 66 life)) ; Output: (life are my numbers)


;7
(defun question-6-update (input)
    (setf f nil)
    (loop for x in input
        do (cond ((numberp x))
            (t (setq f (cons x f))))))

(question-6-update '(numbers -945 34 are my -45 66 life))
