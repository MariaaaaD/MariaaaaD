#lang scheme

(define roots car)
(define lTree cadr)
(define rTree caddr)



(define (height tree)
  (if (empty? tree)
      0
      (+ 1 (max (height (lTree tree)) (height (rTree tree))))
      )
  )

(define (leaves tree)
  (cond [(and (empty? (lTree tree)) (empty? (rTree tree))) 1]
        [(or (empty? (lTree tree)) (empty? (rTree tree))) 1]
        [else (+ (leaves (lTree tree)) (leaves (rTree tree)))]
        )
  )

(define (check? tree)
  (cond [(and (empty? (lTree tree)) (empty? (rTree tree))) #t]
        [(or (empty? (lTree tree)) (empty? (rTree tree))) #f]
        [else (and (check? (lTree tree)) (check? (rTree tree)))]
        )
  )
