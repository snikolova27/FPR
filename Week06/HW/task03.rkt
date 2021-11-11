#lang racket
;helper function for the main one
(define (cnt-greater el xs)
  (define (helper no-duplicates cnt)
     (cond
    [(null? no-duplicates) cnt]
    [(< el (car no-duplicates)) (helper (cdr no-duplicates) (add1 cnt))]
    [else (helper (cdr no-duplicates) cnt)]
    )
  )
  (helper (remove-duplicates xs) 0)
)

(define (num-bigger-elements xs)
  (define (helper result left-over)
    (cond
      [(null? left-over) (reverse result)]
      [else (helper (cons (cons(car left-over) (cnt-greater (car left-over) xs)) result) (cdr left-over))]
    )
  )
  (helper '() xs)
)

;(num-bigger-elements '(5 6 3 4))
(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))