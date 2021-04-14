; Sujay Swain
; Quick Lab 3

(defun remove-element (x y)
    (setq f nil)
    (dolist (n y)
        (cond 
            ((eq x n))
            (t (setq f (append f (list n))))
        )
    )
    f
)

(defun count-x-in-y (x y)
    (setf a 0)
    (loop for n in y
        do (cond
            ((equalp x n) (setq a (+ a 1)))
        )
    )
    a
)

(defun imember-p (x y)
    (setf val nil)
    (do ((l1 y (cdr l1))
        (v1 nil (car l1)))
        ((equalp v1 x) (setf val x))
    )
    val
)

; (write (imember-p 'x '(x y z)))

