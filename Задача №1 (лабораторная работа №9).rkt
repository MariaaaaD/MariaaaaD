#lang racket

(define (count a k)
  (define (iter num lst)
    (cond [(empty? lst) num]
          [else (if (= (car lst) k)
                    (iter (+ num 1) (cdr lst))
                    (iter  num (cdr lst))
                    )
                ]
          )
    )
  (iter 0 a)
  )

(define (counter a)
  (define (iter num lst)
    (cond [(empty? lst) num]
          [else (if (= (count lst (car lst)) 1)
                    (iter (+ num 1) (cdr lst))
                    (iter num (cdr lst))
                    )
                ]
          )
    )
  (iter 0 a)
  )