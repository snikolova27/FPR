#lang racket
(define (replace xs dict)
  (define (helper result left-over)
    (cond
      [(null? left-over) (reverse result)]
      [(pair? (assoc (car left-over) dict )) (helper (cons (cdr (assoc (car left-over) dict)) result) (cdr left-over))]
      [else (helper (cons (car left-over) result) (cdr left-over))]
    )
  )
  (helper '() xs) 
)
(equal? (replace '(1 2 3 4) '((1 . 11) (2 . 22))) '(11 22 3 4))