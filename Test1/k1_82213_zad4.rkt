#lang racket

;return the product of digits of a nummber n
(define (product-of-digits n)
  (define (helper left-over res)
    (if (zero? left-over)
        res
        (helper (quotient left-over 10) (* res (remainder left-over 10)))
    )
  )
  (helper n 1)
)
;(product-of-digits 999)

;return a list of products until a number that's less than 10 is reached
(define (get-list-of-products n)
  (define (helper current-product res)
    (if (< current-product 10)
        (append res (list current-product ))
    (helper (product-of-digits current-product) (append res (list current-product)))
  )
 )
    (helper (product-of-digits n) '()
))
;(get-list-of-products 39)


(define (persistence n)
  (cons (get-list-of-products n) (length (get-list-of-products n)))
)

(persistence 39); → '((27 14 4) . 3) ; 3*9=27, 2*7=14, 1*4=4
(persistence 126); → '((12 2) . 2) ; 1*2*6=12, 1*2=2
(persistence 4); → '((4) . 1)
(persistence 999) ;→ '((729 126 12 2) . 4)
