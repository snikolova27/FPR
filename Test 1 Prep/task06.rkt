#lang racket

(define (get-ascending-sublist-from start-index xs)
  (define (helper current-index result)
    (if (or (= (add1 current-index) (length xs)) (> (list-ref xs current-index) (list-ref xs (add1 current-index))))
        result
        (helper (add1 current-index) (cons (list-ref xs (add1 current-index)) result))
        )
    )
  (if (>= (add1 start-index) (length xs))
      '() ; това е с цел долната рекурсия да знае кога да спре да се върти и да не хвърлям грешки тук (ако е валиден индексът, тази винаги ще връща непразен списък)
      (cons (list-ref xs start-index) (reverse (helper start-index '())))
      )
  )

(define(max-ordered-sublist xs)
  (define (helper current-sublist current-index max-len best-sublist)
    (cond
      [(empty? current-sublist) best-sublist]
      [(> (length current-sublist) max-len) (helper (get-ascending-sublist-from (add1 current-index) xs) (add1 current-index) (length current-sublist) current-sublist)]
      [else (helper (get-ascending-sublist-from (add1 current-index) xs) (add1 current-index) max-len best-sublist)]
      )
    )
  (helper (get-ascending-sublist-from 0 xs) 0 0 '())
  )

(equal? (max-ordered-sublist '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))