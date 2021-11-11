#lang racket
(require racket/trace)

(define (rev-fold lst)
  (foldr (lambda (x acc) (+ (* acc 10) x)) 0 lst)
)

(define (rev-lin-rec lst)
  (if (null? lst)
      (+ 0)
      (+ (* (last lst) (expt 10 (sub1 (length lst) ))) (rev-lin-rec (remq (last lst) lst)))
))

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

;(trace rev-lin-rec)

; using a linearly recursive procedure
(= (rev-lin-rec '(1 2 3)) 321)
(= (rev-lin-rec '(1 2 3 4 5 6 7 8 9)) 987654321)


