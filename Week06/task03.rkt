#lang racket

(define (assoc-rec key a-list)
  (cond
    [(null? a-list) (error "List is empty")]
    [(= (length a-list) 1)
     (if ( = key (caar a-list))
         (cdar a-list)
         (error "Element not present")
     )
    ]
    [(= key (caar a-list)) (cdar a-list)]
    [else (assoc-rec key (cdr a-list))]
  )
)

(define (assoc-hop key a-list)
  (cdar(dropf a-list (lambda (x) (not(equal? key (car x)))))
))

(define (assoc-assoc key a-list)
  (cdr (assoc key a-list))
)
; using a recursive process
(equal? (assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using a higher order procedure
(equal? (assoc-hop 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using assoc
(equal? (assoc-assoc 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")