#lang racket
(define (get-sublist start-idx end-idx xs)
  (take (drop xs start-idx) (- end-idx start-idx -1))
)
 (get-sublist 2 6 '(1 2 2 3 1 5 6 7 7))
(equal? (get-sublist 2 6 '(1 2 2 3 1 5 6 7 7)) '(2 3 1 5 6))