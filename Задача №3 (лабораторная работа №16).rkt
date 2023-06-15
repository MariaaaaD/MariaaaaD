#lang scheme

(define roots car)
(define lTree cadr)
(define rTree caddr)

(define (maximum tree)
  (if (and (empty? (lTree tree)) (empty? (rTree tree)))
      (roots tree)
      (max (maximum (lTree tree)) (maximum (rTree tree)))
      )
  )

(define (counter tree)
  (if (and (empty? (lTree tree)) (empty? (rTree tree)))
      1
      (if (= (maximum (lTree tree)) (maximum (rTree tree)))
          (+ (counter (rTree tree)) (counter (lTree tree)))
          (if (> (counter (lTree tree)) (counter (rTree tree)))
              (counter (lTree tree))
              (counter (rTree tree))
          )
      )
  )
  )

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

