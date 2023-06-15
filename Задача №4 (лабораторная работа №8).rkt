#lang racket

(define (sticker a)
  (define (iter lst rez)
    (cond [(empty? lst) rez]
          [else (iter (cdr lst) (cons (car lst) rez))]
          )
    )
  (iter (cdr a) a)
  )