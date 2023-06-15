#lang racket

(define (construction a)
  (define (iter rez pair lst)
    (cond [(empty? lst) (cdr (reverse (cons a rez)))]
          [else (iter (cons (reverse pair) rez) (cons (car lst) pair) (cdr lst))]
          )
    )
  (iter '() '() a)
  )