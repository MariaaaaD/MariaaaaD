#lang racket

(define (Horner lst)
  (define (iter num liste)
    (cond [(empty? liste) num]
          [else (iter (+ (* num 10) (car liste)) (cdr liste))]
          )
    )
  (iter (car lst) (cdr lst))
  )

(define (maker str)
  (Horner (map (λ (x) (if (= x 48) 0 (- x 48))) (map (λ (y) (char->integer y)) (string->list str))))
  )