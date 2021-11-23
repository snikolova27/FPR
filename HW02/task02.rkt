#lang racket
(require racket/trace)

;за padding отдолу и отгоре
(define (add-padding-top xs x )
  (define (helper result len)
    (cond
      [(zero? len) result]
      [else (helper (cons x result) (sub1 len))]
    )
  )
  (helper '() (+ 2 (length xs)))
 )

;за padding отляво и отдясно на всеки ред
(define (add-padding-left row x)
  (append (list x) row (list x))
)

(define (pad xs)
  (lambda (x)
    (define (helper res left-over current-row final-rows)
      (cond
        [(= final-rows current-row) (reverse res)]
        [(zero? current-row) (helper (append (list (add-padding-top xs x)) res) left-over (add1 current-row) final-rows)]
        [(= final-rows (+ 1 current-row)) (helper (append (list (add-padding-top xs x)) res) left-over (add1 current-row) final-rows)]
        [else (helper (append  (list (add-padding-left (car left-over ) x))  res) (cdr left-over) (add1 current-row) final-rows)]
      )
    )
   ; (trace helper)
    (helper '() xs 0 (+ 2 (length xs)) )
    
  )
)

(equal? ((pad '( (1 2 3)
         (4 5 6)
         (7 8 9) )) 0)
        '( (0 0 0 0 0)
 (0 1 2 3 0)
 (0 4 5 6 0)
 (0 7 8 9 0)
 (0 0 0 0 0) )
  )



(equal? ((pad '( (1 2 3)
                 (4 5 6)
                 (7 8 9)))9)
'((9 9 9 9 9)
 (9 1 2 3 9)
 (9 4 5 6 9)
 (9 7 8 9 9)
 (9 9 9 9 9) ))

((pad '()) 0)
