#lang racket
(require math/number-theory)
(require racket/trace) 

(define (nth-cubic n)
  (define (helper cnt first-num)
    (cond
      [(and (= cnt 0) (prime? (- (expt first-num 3) (expt (sub1 first-num) 3)))) (- (expt first-num 3) (expt (sub1 first-num) 3))]
      [(prime? (- (expt (+ 1 first-num) 3) (expt first-num 3))) (helper (sub1 cnt) (+ 1 first-num))]
      [else (helper cnt (+ 1 first-num))]
    )
  )

  (cond
    [(> n 0)
    ; (trace helper)
     (helper n 1)]
      [else (error "n should be positive") ]
     
  )
)

(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61)
(= (nth-cubic 50) 55897)
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
; (nth-cubic 0) ; should return an error