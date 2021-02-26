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
        ((eq (car (cdr l1)) nil) (list (car l1)))
        (t (append (rev-all (cdr l1)) (list (car l1))))
    )
)

(defun len (l1)
    (cond 
        (((eq (car (cdr l1)) nil)) (1))
        (t ())
    )
)


(defun dottep (v1)
    (cond 
        ((listp (cdr v1)) NIL)
        (T T)
    )
)