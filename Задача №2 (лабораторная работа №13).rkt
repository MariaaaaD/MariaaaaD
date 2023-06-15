#lang racket

;генерирует номера элементов
(define (program n k)
  (build-list n (λ (x) (+ (+ x 1) (* k 10))))
  )

;генерирует строки матрицы
(define (builder n k)
  (map (λ (z) (cond [(and (<= k (/ n 2)) (or (<= z (+ (* k 10) k)) (> z (+ (* k 10) (- n k))))) 1] [(and (> k (/ n 2)) (or (<= z (+ (* k 10) (- (+ n 1) k))) (>= z (+ (* 10 k) k)))) 1] [else 0])) (program n k))
  )
  
;собирает все вместе
(define (final n)
  (map (λ (k) (builder n k)) (build-list n (λ (m) (+ m 1))))
  )