#lang racket
(define (bigger-els-than x xs)
  (define (helper cnt left-over)
    (cond
      [(null? left-over) cnt]
      [(> (car left-over) x) (helper (add1 cnt) (cdr left-over))]
      [else (helper cnt (cdr left-over))]
    )
  )
  (helper 0 (remove-duplicates xs)
))

(define (num-bigger-elements lst)
  (define (helper result left-over no-dupls)
    (cond
      [(null? left-over) result]
      [else (helper (append result (list (list (car left-over) (bigger-els-than (car left-over) no-dupls )))) (cdr left-over) no-dupls)]
    )
  )
  (helper '() lst (remove-duplicates lst) 
))

(equal? (num-bigger-elements '(5 6 3 4 ))'((5 1) (6 0) (3 3) (4 2)) ) 
(equal? (num-bigger-elements '(1 1 1 )) '((1 0) (1 0) (1 0)))