#lang racket

(define (step n)
  (define (iter pow num)
    (cond [(= n pow) num]
          [(< n pow) #f]
          [else (iter (* 2 pow) (+ num 1))]
          )
    )
  (iter 1 0)
  )