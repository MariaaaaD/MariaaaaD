#lang racket

(define (find b n)
  (define (iter num lst rez)
    (cond [(= num n) rez]
          [else (iter (+ num 1) (cdr lst) (car lst))]
          )
    )
  (iter 1 (cdr b) (car b))
  )

(define (beautiful-list b)
  (build-list (length b) add1)
  )

(define (counter a b)
  (foldl (λ (x y rez) ((find (map eval b) x) y rez)) (car a) (beautiful-list b) (cdr a))
  )