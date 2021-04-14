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


(defun combinations (x y z)
    (setf total nil)
    (dolist (l x)
        (dolist (m y)
            (dolist (n z)
                (setf total (append total (list (cons l (cons m (cons n nil))))))
            )
        )
    )
    total
)

(defun remove-redundancies (x)
    (setf simply (list (car x)))
    (dolist (m x)
        (setf test nil)
        (dolist (n simply)
            (cond 
                ((eq test nil) (setf test (equalp m n)))
                ((eq test t)))
        )
        (cond ((eq test nil) (setf simply (append simply (list m)))))
    )
    simply
)
