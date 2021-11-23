#lang racket
 (define (switchsum f g n)
   (lambda (x)
     (if (= 1 n)
         (f x)
         (+ (f x) ((switchsum g f (sub1 n)) (f x)))
         
   )
 )
)

(= ((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 1) 2) 3)
(= ((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 2) 2) 9)
(= ((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 3) 2) 16)
(= ((switchsum (λ (x) (+ x 1)) (λ (x) (* x 2)) 4) 2) 30)
