#lang racket
(require racket/trace)

(define (cnt-occurrences x xs)
  (define (helper cnt left-over)
    (cond
      [(null? left-over) cnt]
      [(= x (car left-over)) (helper (add1 cnt) (cdr left-over))]
      [else (helper cnt (cdr left-over))]
    )
  )
  (helper 0 xs)
)

(define (correct-row? row row-num)
  (define (helper pos left-over cnt)
    (cond
      [(or (> cnt row-num) (null? left-over)) #f]
      [(and (= 1 row-num) (> 0 (cnt-occurrences 0 row)))  #f]
      [(and (= cnt (sub1 row-num)) (= 1 (length left-over)) (not (= 0 (car left-over)))) #t]
      [(=(car left-over) 0 ) (helper (add1 pos) (cdr left-over) (add1 cnt))]
      [else (helper (add1 pos) (cdr left-over) cnt)]
    )
  )
  ;(trace helper)
  (helper 1 row 0)
)

(define (triangular mat)
  (define (helper left-over current-row-num)
    (cond
      [(null? left-over) #t]
      [(not (correct-row? (car left-over) current-row-num)) #f]
      [(correct-row? (car left-over) current-row-num) (helper (cdr left-over) (add1 current-row-num))]
      [else #f]
    )
  )
 ; (trace helper)
  (helper mat 1)
)

(equal? (triangular '( (1 2 3)
                       (0 5 6)
                       (0 0 9))) #t)

(equal? (triangular '( (0 2 3)
                       (1 5 6)
                       (1 0 0))) #f)

(equal? (triangular '( (1 2 3)
                       (1 5 6)
                       (0 0 9))) #f)

(equal? (triangular '( (1 2 3 4)
                       (0 5 6 7)
                       (0 0 8 9)
                       (0 0 0 9))) #t)