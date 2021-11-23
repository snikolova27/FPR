#lang racket
(require racket/trace)

(define (equal-lens-sublists? xss )
  (define (helper left-over len)
    (cond
      [(null? left-over) #t]
      [(not (= len (length (car left-over)))) #f]
      [else (helper (cdr left-over) len)]
     )
  )
  (helper xss (length (car xss)))
)

(define (equal-lens? xss yss)
  (= (length xss) (length yss))
)

(define (have-matching-lengths xss yss)
  (cond
    [(null? xss) (error "xss cannot be empty")]
    [(null? yss) (error "yss cannot be empty")]
    [(and (equal-lens? xss yss) (equal-lens-sublists? xss) (equal-lens-sublists? yss) (= (length (car xss)) (length (car yss)))) #t]
    [else #f]
  )
)
 
(equal? (have-matching-lengths '((1 2 3) (4 5 6) (7 8 9)) '((1 4 7) (2 5 8) (3 6 9))) #t)
(equal? (have-matching-lengths '((1 2)) '((1 4 7) (2 5 8))) #f)
(equal? (have-matching-lengths '( (1 2 3) (4 5)) '( (5 4) (8 9))) #f)
;(have-matching-lengths '() '((1 2 3) (1))) ->дава грешката за празен списък


#|
връща списъка с двойките xsi ysi
;щях да я използвам, ако случаят, в който подаваме на
 (have-matching-lengths '((1 2 3) (4 5)) '((5 4) (8 9 10))) и това ни връща true, защото "двойките" щяха да са
((1 2 3) (4 5)) и ((5 4) (8 9 10)) щяха да са с дължина
     3 + 2  = 5       2  +  3  = 5
т.е. дължината на една "двойка" = сбор на дължните на съставните й елементи
|#
 
(define (get-list-of-pairs xss yss)
  (define (helper res left-over-xss left-over-yss)
    (cond
      [(or (null? left-over-xss) (null? left-over-yss)) res]
      [else (helper (append res (list (list (car left-over-xss) (car left-over-yss)) )) (cdr left-over-xss) (cdr left-over-yss))]
    )
  )
  ;(trace helper)
  (helper '() xss yss)
)

;(get-list-of-pairs '( (1 2) (4 5 6)) '( (4 5 6) (7 8)))