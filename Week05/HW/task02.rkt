#lang racket
;NOT FINISHED
(require racket/trace)

;list-ref returns the value of the element at given position

(define (longest-ascending from lst)
  (define (helper current res)
    (if (or  (= (length lst) (+ 1 current)) (< (list-ref lst (+ 1 current)) (list-ref lst current)))
        res
        (helper (+ 1 current) (cons (list-ref lst ( + 1 current)) res))
    )
   )

  (if (>= (+ 1 from) (length lst))
      '()
      (cons (list-ref lst from) (reverse(helper from '())))
  )
)



(define (longest-ascending-sub lst)
  (define (helper max-len max current pos)
    (cond
      [(null? current) max]
      [(<  max-len ( length current)) (helper (length current) current (longest-ascending (+ 1 pos) lst) (+ 1 pos) )   ]
     [else (helper max-len max (longest-ascending (+ 1 pos) lst) (+ 1 pos))]
    )
  )
  (helper 0 '() (longest-ascending 0 lst) 0)
)






(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))