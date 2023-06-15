#lang racket

(define roots car)
(define lTree cadr)
(define rTree caddr)

(define (level_i n tree)
  (define (iter lsubtree rsubtree rez num)
    (cond [(= num n) rez]
          [else (if (= (- n 1) num)
                    (if (and 