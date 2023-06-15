#lang racket

(define (^ a b)
  (expt a b)
  )

(define (find b n)
  (define (iter num lst rez)
    (cond [(= num n) rez]
          [else (iter (+ num 1) (cdr lst) (car lst))]
          )
    )
  (iter 1 (cdr b) (car b))
  )

(define (dates a)
  (map (λ (x) (find x 1)) a)
  )

(define (operations a)
  (map (λ (x) (eval (find x 2))) a)
  )

(define (sums a)
  (map (λ (x) (find x 3)) a)
  )

(define (beautiful-list a)
  (build-list (length a) add1)
  )

(define (sticker a)
  (reverse (cons '(31 + 0) (reverse a)))
  )

(define (adder a n)
  (cons (reverse (cons n (reverse '(1 +)))) a)
  )

(define (combination a n)
  (sticker (adder a n))
  )

(define (banking sum percent top)
 (foldl (λ (x rez) ((find (operations (combination top sum)) x) (* rez (^ (+ 1 (exact->inexact (/ percent 100))) (abs (- (find (dates (adder top sum)) (- x 1)) (find (dates (combination top sum)) x))))) (find (sums (combination top sum)) x))) sum (cdr (beautiful-list (combination top sum))))
  )