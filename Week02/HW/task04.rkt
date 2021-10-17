#lang racket
(require math/number-theory)  ;check case 2 24
(require racket/trace) 


(define (rev n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (helper 0 n)
)

(define (palindrome? n)
  (= n (rev n))
)


(define (num-palindromes-rec a b)
    (define (helper min max)
        (cond
            [(> min max) 0]
            [(palindrome? min) (+ 1 (helper (+ 1 min) max))]
            [else (helper (+ 1 min) max)]
        )
 
     )

  (cond
    [(> a b)
     ;(trace helper)
     (helper b a)] 
     [else
      ;(trace helper)
      (helper a b)] 
    )
)

(define (num-palindromes-iter a b)

  (define (helper current cnt-palindromes min max)
    (cond
      [(< current min) (error "Current num is lesser than min")]
      [(> current max) cnt-palindromes]
      [(palindrome? current) (helper (+ 1 current) (+ 1 cnt-palindromes) min max)]
      [else (helper (+ 1 current) cnt-palindromes min max)]
      )
)
  (cond
    [ (> a b) (helper b 0 b a)]
    [else (helper a 0 a b)]
    )
)

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)


(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)

