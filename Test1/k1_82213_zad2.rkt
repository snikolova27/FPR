#lang racket
(require racket/trace)

;returns an ascendingly sorted list of the lengths of the inner lists
(define (get-list-of-lens xss)
  (define (helper left-over res)
    (if (null? left-over)
        (sort res <=)
        (helper (cdr left-over) (append res (list(length (car left-over))))))
  )
  (helper xss '())
)
;finds the missing element in a list, where an+1 = an + 1
(define (find-missing-el xs)
  (define (helper current next found left-over)
    (cond
      [(and (not found) (null? left-over))  (error "No missing element")]
      ;[(and (= current (last xs)) (not found))]   <- previous end condition
      [(= (add1 current) next) (helper (car left-over) (cadr left-over) found (cdr left-over))]
      [else (add1 current)]
    )
  )
  ;(trace helper)
  (helper (car xs) (cadr xs) #f (cdr xs))
)
;(get-list-of-lens '((1 2) (4 5 1 1) (1) (5 6 7 8 9)))
;(find-missing-el '(1 2 3 5 6))

(define (get-missing-length xss)
  (cond
    [(or (null? xss) (equal? '(()) (member '() xss)) ) (error "Empty list!")]
    [else (find-missing-el (get-list-of-lens xss)) ]
  ) 
)


(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9))) ;-> 3
(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a",
"a") ("a") ("a", "a", "a", "a", "a", "a"))) ; -> 5

;These are errors
;(get-missing-length '())
;(get-missing-length '( (1 2 ) ()))


