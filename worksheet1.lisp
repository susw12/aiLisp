; Question 1 
(car '(((a)) (b c d e)))
(cdr '(((((f))))))
(car (cdr '(a b c)))
(car (cdr (cdr '(a b c))))
(cons '(my life as) '(a dog))
(cons (cdr nil) (car nil))

; Question 2
(car (cdr (cdr (cdr '(he is dead jim)))))
(car (car (car (car (cdr '(captain (((jim) kirk))))))))
(car (cdr (car (car '(((((spock) asked) jim) if) he was all right)))))
(car (car (car (car (car (cdr (cdr (cdr (cdr (car (cdr '(after (looking at the (lizard man) ((((jim))) asked for warp 9))))))))))))))

; Question 3
(reverse '(he is dead jim))
(reverse '(captain (((jim) kirk))))
(reverse '(((((spock) asked) jim) if) he was all right))
(reverse '(after (looking at the (lizard man) ((((jim))) asked for warp 9))))