#lang racket

(define (searcher matrix i j)
  (list-ref (list-ref matrix (- i 1)) (- j 1))
  )

(define (delete a n)
  (define (iter rez lst)
    (cond [(empty? lst) (reverse rez)]
          [else (if (= n (car lst))
                    (iter rez (cdr lst))
                    (iter (cons (car lst) rez) (cdr lst))
                    )
                ]
          )
    )
  (iter '() a)
  )

(define (predominant matrix)
  (if (andmap (λ (x) (= (searcher matrix x x) (foldl + 0 (delete (list-ref matrix (- x 1)) (searcher matrix x x))))) (build-list (length matrix) add1))
      #f
      (andmap (λ (x) (>= (searcher matrix x x) (foldl + 0 (delete (list-ref matrix (- x 1)) (searcher matrix x x))))) (build-list (length matrix) add1))
      )
  )
  