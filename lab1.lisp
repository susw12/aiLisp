; Sujay Swain
; Quick Lab #1

(defun rect-area (v1 v2)
    (* v1 v2)
)

(defun rev-two (l1)
    (cons (car (cdr l1)) (cons (car l1) NIL))
)

(defun rev-all (l1)
    (cond 
        ((eq (car (cdr l1)) nil) (car l1))
        (T (cons (rev-all (cdr l1)) (cons (car l1) nil)))
    )
)
;(write (rev-all '(a b c d e)))

;(defun len (l1)
;    (setq sum 0)
;    (cond 
;        ((eq (car (cdr l1)) nil) (+ sum 0))
;        (T (len(cdr l1)))  
;)

(defun dottep (v1)
    (write (cons (car v1) (cdr v1)))
    (equal v1 (cons (car v1) (cdr v1)))
)
(write (dottep '(1 2)))