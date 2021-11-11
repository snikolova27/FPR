#lang racket

;първоначална идея :D 
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
    [else (helper xs '() 0) ]
  )
)

(define (concat-proc xs ys)
  (append xs ys)
)

(define (concat-rec xs ys)
  (cond
    [( = (length xs) 1) (cons (car xs) ys)]
    [else (concat-rec ( reverse (cdr (reverse xs))) (cons (last xs) ys))]
  )
)
; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly recursive process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

