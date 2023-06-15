#lang racket

(define (lst a)
  (reverse (build-list (length a) add1))
  )

(define (placeur a)
  (foldl (λ (x res) (cons (take-right a x) res)) '() (reverse (lst a)))
  )