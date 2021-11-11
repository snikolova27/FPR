#lang racket
(require racket/trace)

(define (insert-at x idx xs)
  (define (helper left-over result current-idx)
    (cond
      [(null? left-over) (reverse result)]
      [(< current-idx idx) (helper (cdr left-over)(cons (car left-over) result) (add1 current-idx))]
      [(= current-idx idx) (helper left-over (cons x result) (+ 2 current-idx) )]
      [else (helper (cdr left-over) (cons (car left-over) result) (add1 current-idx))]
    )
  )
  (cond
    [(null? xs) (cons x xs)]
    [(= idx 0) (cons x xs)]
    [else
     ; (trace helper)
     (helper xs '() 0) ]
  )
)

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 1 3 '(2 4 5 7 8 )) '(2 4 5 1 7 8))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
