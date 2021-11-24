#lang racket
(require racket/trace)

;counts occurrences of an element in a list
(define (cnt-occurrences-of n xs)
  (define (helper left-over cnt)
    (cond
      [(null? left-over) cnt]
      [(= n (car left-over)) (helper (cdr left-over) (add1 cnt))]
      [else (helper (cdr left-over) cnt)]
   )
  )
  (helper xs 0)
)
;get a list of the digits of a number sorted in descending
(define (num-to-xs n)
  (define (helper left-over res)
    (cond
      [(zero? left-over) (sort res >=)]
      [else (helper (quotient left-over 10) (append (list (remainder left-over 10)) res))]
    )
  )
  (helper n '())
)

(define (get-distribution n)
  (define (helper left-over res squared)
    (cond
      [(null? left-over) (remove-duplicates res)]
      [else (helper (cdr left-over) (append  (list(cons (car left-over) (cnt-occurrences-of  (car left-over) squared))) res) squared)]
    )
  )
    ;(trace helper)

  (if ( < n 0)
      (error "n should be non-negative")
      
   (helper (num-to-xs (* n n)) '() (num-to-xs (* n n) )))
)

(get-distribution 123)

;(get-distribution 123) '((1 . 2) (2 . 1) (5 . 1) (9 . 1))
