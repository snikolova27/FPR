#lang racket
;helper function for the main one
;set1 contains only one element
(define (cartesian-single set1 set2)
  (define (helper result current-set2)
    (if (null? current-set2)
        (reverse result)
        (helper (cons (cons set1 (car current-set2)) result) (cdr current-set2))
     )
  )
     (helper '() set2)
)

;(cartesian-single 1 '(2 3 4 5 6))

(define (my-cartesian-product set1 set2)
  (define (helper result current-set1)
    (cond
      [(null? current-set1) result]
      [else (helper (append result (cartesian-single (car current-set1) set2)) (cdr current-set1))]
    )
  )
  (helper '() set1)
)
(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))