#lang racket
(require racket/trace)

(define (shuffle xs)
  (define (helper result current-pos middle)
    (cond
      [(= current-pos middle) result]
      [else (helper (append result (list (list-ref xs current-pos)) (list (list-ref xs(+ middle current-pos))) ) (add1 current-pos) middle)]
    )
  )
  ;(trace helper)
  (helper '() 0 (/ (length xs) 2))
)

(equal? (shuffle ' (2 5 1 3 4 7)) '(2 3 5 4 1 7))
(equal? (shuffle '(1 2 3 4 4 3 2 1)) '(1 4 2 3 3 2 4 1))
(equal? (shuffle '(1 1 2 2)) '(1 2 1 2))