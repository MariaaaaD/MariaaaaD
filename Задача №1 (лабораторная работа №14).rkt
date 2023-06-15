#lang racket

(define (maker str)
  (reverse (map (λ (y) (if (= y 48) 0 (- y 48))) (map (λ (x) (char->integer x)) (string->list str))))
  )

(define (collector str)
  (foldl (λ (x y z) (+ (* x (expt 10 y)) z)) 0 (maker str) (map (λ (z) (- z 1)) (build-list (length (maker str)) (λ (k) (+ k 1)))))
  )