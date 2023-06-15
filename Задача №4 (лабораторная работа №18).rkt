#lang racket
 (define lTree car)
(define rTree cadr)

(define (height hafftree)
  (if (not (list? hafftree))
      1
      (+ (max (lTree hafftree)) (max (rTree hafftree)))
      )
  )

(define (make-list hafftree)
  (if (not (list? hafftree))
      (list hafftree)
      (append (make-list (lTree hafftree)) (make-list (rTree hafftree)))
      )
  )

(define (numbers hafftree)
  (map (λ (x) (cdr x)) (make-list hafftree))
  )

(define (frequency hafftree)
  (if (andmap (λ (x) (number? x)) (numbers hafftree))
  (map (λ (x) (foldl (λ (z y) (if (= x z) (+ y 1) y)) 0 (numbers hafftree))) (numbers hafftree))
  (map  (λ (x) (foldl (λ (z y) (if (eq? x z) (+ y 1) y)) 0 (numbers hafftree))) (numbers hafftree))
  )
  )

(define (final hafftree)
  (map (λ (x y) (cons x y)) (numbers hafftree) (frequency hafftree))
  )