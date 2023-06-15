#lang racket

(define (% a b)
  (remainder a b)
  )

(define (// a b)
  (quotient a b)
  )

(define (^ a b)
  (expt a b)
  )

(define (pair x k)
  (define (iter num first second ost)
    (if (= k num)
        (cons first second)
        (iter (+ num 1) (// first 10) (% x ost) (* ost 10))
        )
    )
  (iter 0 x 0 10)
  )