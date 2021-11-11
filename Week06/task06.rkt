#lang racket
 (define (count-occurrences ys xs)
   (define (helper cnt left-over)
     (cond
       [(> (length ys) (length left-over)) cnt]
       [(equal? ys (take left-over (length ys))) (helper (add1 cnt) (cdr left-over))]
       [else (helper cnt (cdr left-over))]
     )
   )
   (helper 0 xs)
 )

(= (count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurrences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurrences '(6 6) '(2 2)) 0)