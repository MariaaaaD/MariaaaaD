#lang racket

(define (% a b)
  (remainder a b)
)

(define (// a b)
  (quotient a b)
)

(define (check n)
  (define (iter a)
    (cond [(< n 10) #t]
          [(or (> (% n (% n 10)) 0) (and (< n 100) (= (% n 10) 1) (< (% n 10) (// n 10)))) #f]
          [else (if (and (= a (% n 10)) (= (% n a) 0))
                         (check (// n 10))
                         (iter (+ a 1))
                         )
                ]
          )
    )
  (iter 0)
  )