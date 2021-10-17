#lang racket
(require racket/trace)

(define (count-digits-iter num)
    (define (helper remaining digits)
      (if( <  remaining 10)
         (+ 1 digits)
         (helper (quotient remaining 10) (+ 1 digits))
         )
    )
    (cond
    [ (< num 0) (error "n was negative")]
    [(< num 10) 1]
    [else (helper num 0)]
    )
)

(define (automorphic? n)

(define (compare-digit cnt-digits-n num squared-n)
    (cond
    [ (zero? cnt-digits-n) #t]
    [(and (< 0 cnt-digits-n) (= (remainder num 10) (remainder squared-n 10))) (compare-digit (sub1 cnt-digits-n) (quotient num 10) (quotient squared-n 10))]
    [else #f]
    )
 
)

    (cond
    [(<= n 0) (error "n was not natural")]
    [(< n 10) (= (remainder (expt n 2) 10) n)]
    [else      
    ;(trace compare-digit )
     (compare-digit  (count-digits-iter n ) n (expt n 2))]
    )

)
(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
; (automorphic? -1) ; error: n was not natural
; (automorphic? 0) ; error: n was not natural