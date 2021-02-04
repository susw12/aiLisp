; Question 1 
(car '(((a)) (b c d e)))
(cdr '(((((f))))))
(car (cdr '(a b c)))
(car (cdr (cdr '(a b c))))
(cons '(my life as) '(a dog))
(cons (cdr nil) (car nil))

; Question 2
(car (cdr (cdr (cdr '(he is dead jim)))))
(cdr (car '(car (captain (((jim) kirk))))))