#lang racket
;връща списък от крайни летища с начало на полета from
(define (get-end-from start flights)
  (define (helper res left-over)
    (cond
      [(null? left-over) res]
      [(equal? start (caar left-over)) (helper (append (list (cdar left-over)) res) (cdr left-over))]
      [else (helper res (cdr left-over))]
    )
  )
 (sort (helper '() flights) string<? ) 
)

(define (itinerary flights)
  (lambda (start)
    (define (helper res left-over current-start)
      (cond
        [(not(assoc start flights)) (error "airport not in the list")]
        [(null? left-over) (reverse res)]
        [(null? (get-end-from current-start left-over)) (error "No such itinerary!")]
        [else (helper (append(list (car (get-end-from current-start left-over) )) res) (remove (cons current-start (car (get-end-from current-start left-over))) left-over)  (car (get-end-from current-start left-over))) ]
      )
    )
    (helper (list start) flights start)
  )
)


((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") 
("HKO" . "ORD"))) "YUL")
((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A"))) 
"A");'("A" "B" "C" "A" "C")
((itinerary '(("SFO" . "COM") ("COM" . "YYZ"))) "COM")
;"No such itinerary!"

