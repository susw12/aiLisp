; Sujay Swain
; Quick Lab 2

(defun our-last (l1)
    (cond 
        ((eq (car (cdr l1)) nil) (car l1))
        (t (our-last (cdr l1)))
    )
)

(defun count-x-in-y (x y)
    (cond
        ((eq (car y) x) (+ 1 (count-x-in-y x (cdr y))))
        ((eq (car y) nil) 0)
        (t (+ 0 (count-x-in-y x (cdr y))))
    )
)

(defun average-r (l1)
    (/ (summation l1) (counter l1))
)

(defun summation (l1)
    (cond 
        ((eq (car l1) nil) 0)
        ((numberp (car l1)) (+ (car l1) (summation (cdr l1))))
        (t (+ 0 (summation (cdr l1))))
    )
)

(defun counter (l1)
    (cond 
        ((eq (car l1) nil) 0)
        ((numberp (car l1)) (+ 1 (counter (cdr l1))))
        (t (+ 0 (counter (cdr l1))))
    )
)

(defun squash-list (l1)
    (cond
        ((eq (car l1) nil) nil)
        ((listp (car l1)) (append (squash-list(car l1)) (squash-list(cdr l1))))
        (t (append (list (car l1)) (squash-list(cdr l1))))
    )
)

(defun our-member-p (v1 l1)
    (cond
        ((> (count-x-in-y v1 l1) 0) v1)
        ((eq (count-x-in-y v1 l1) 0) nil)
    )
)
