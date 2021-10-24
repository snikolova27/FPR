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

(define (find-max n)
  (define (helper current-n max)
    (cond
      [(zero? current-n) max]
      [(> (remainder current-n 10) max) (helper (quotient current-n 10) (remainder current-n 10))]
      [else (helper (quotient current-n 10) max)]
      )
  )
  (helper (quotient n 10) (remainder n 10))
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

(define (sort-n number)
  (define (helper left-over result current-pos max-digit)
    (cond
      [(zero? current-pos) result]
      [else (helper (remove-first-occurrence left-over max-digit)  (+ result (* max-digit (expt 10 (sub1 current-pos)))) (sub1 current-pos) (find-max (remove-first-occurrence left-over max-digit)))]
    )
  )
     ;(trace helper)
  (helper  number 0 (count-digits number) (find-max number))
)

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)