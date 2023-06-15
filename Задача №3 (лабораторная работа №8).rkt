#lang racket

(define (length a)
  (define (iter lst rez)
    (if (empty? lst) rez
        (iter (cdr lst) (+ rez 1))
        )
    )
  (iter a 0)
  )

(define (maximum a)
  (define (iter lst liste)
    (cond [(and (not (empty? liste)) (< (car liste) (car lst))) (car lst)]
          [(empty? liste) (car lst)]
          [else (iter (cdr lst) (cdr liste))]
          )
    )
  (iter a (cdr a))
  )