#lang racket

(define (findfib n)
  (define (iter previous next)
    (cond [(or (= n previous) (= n next)) n]
          [(and (> n previous) (< n next)) (if (< (abs (- n previous)) (abs (- n next)))
                                               previous
                                               next
                                               )
                                           ]
          [else (iter next (+ next previous))]
          )
    )
  (iter 1 1)
  )
    