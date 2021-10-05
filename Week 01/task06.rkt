#lang racket

(define (are-not-equal-one-line? x y)
  (not ( = x y))
  )


(equal? (are-not-equal-one-line? 5 2) #t)
(equal? (are-not-equal-one-line? 5 5) #f)

(define (are-not-equal-guards? x y)
  (cond
    [(= x y) #f]
    [else #t])
)
(equal? (are-not-equal-guards? 5 2) #t)
(equal? (are-not-equal-guards? 5 5) #f)


(define (inside-one-line? start end x)
        (if (> start end)
            (inside-one-line? end start x)
            (<= start x end)
        )
)

(equal? (inside-one-line? 1 5 4) #t) ; start = 1, end = 5, x = 4
(equal? (inside-one-line? 5 1 4) #t)
(equal? (inside-one-line? 10 50 200) #f)
(equal? (inside-one-line? 10 50 1) #f)

(define (inside-boolean-ops? start end x)
  (and (<= (min start end) x) (<= x (max start end)))
 )
(equal? (inside-boolean-ops? 1 5 4) #t)
(equal? (inside-boolean-ops? 5 1 4) #t)
(equal? (inside-boolean-ops? 10 50 200) #f)
(equal? (inside-boolean-ops? 10 50 1) #f)