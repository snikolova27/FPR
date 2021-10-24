#lang racket
 (require racket/trace)

(define (count-digits number)
  (cond
    [(< number 10) 1]
    [else (add1 (count-digits (quotient number 10)))]
      )
)

(define (digit-occurs? number digit)
  
  (cond
    [(= number digit) #t]
    [(= (remainder number 10) digit) #t]
    [(and(< number 10) (not (= number digit))) #f]
    [else   (digit-occurs? (quotient number 10) digit)]

 )
)

(define (remove-first-occurrence number digit)
  (define (helper result left-over current-pos cnt-digits time-occurring)
    (cond
      [(and (zero? time-occurring)(= digit (remainder left-over 10)))  (helper result (quotient left-over 10)  current-pos  cnt-digits (add1 time-occurring))]
      [(zero? current-pos) result] 
      [else (helper (+ result (* (remainder left-over 10) (expt  10 (- cnt-digits current-pos)))) (quotient left-over 10) (sub1 current-pos) cnt-digits time-occurring)]

     )
   )

  
  (cond
    [(not(digit-occurs? number digit)) (error "Digit doesn't occur in this number")]
    [else
     ;(trace helper)
     (helper 0 number (count-digits number)(count-digits number) 0)]
  )
)

(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)