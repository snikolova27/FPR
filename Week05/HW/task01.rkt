#lang racket
(require racket/trace)

(define (remove-all-no-proc element lst)
  (define (helper left-over result)
    (cond
      [(not (member element lst)) lst]
      [(null? lst) (error "Cannot remove element from an empty list!")]
      [(null? left-over) (reverse result)]
      [(equal? (car left-over) element) (helper (cdr left-over) result)]
      [else (helper (cdr left-over) (cons (car left-over) result ))]
    )
  )
  ;(trace helper)
  (helper lst '())
)



(define (remove-all-proc element lst)
  (if (member element lst)
      (remove-all-proc element (remq element lst))
      lst
  )
)
; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
;(equal? (remove-all-no-proc 1 '(1 1 1 2 4 1 2 4 1 5)) '(2 4 2 4 5))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))