#lang scheme

(define roots car)
(define lTree cadr)
(define rTree caddr)



(define (check? tree)
  (cond [(and (empty? (lTree tree)) (empty? (rTree tree))) #t]
        [(or (empty? (lTree tree)) (empty? (rTree tree))) #f]
        [else (and (check? (lTree tree)) (check? (rTree tree)))]
        )
  )
