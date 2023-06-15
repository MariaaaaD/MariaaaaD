#lang racket

(define (// a b)
  (quotient a b)
  )

(define (% a b)
  (remainder a b)
  )

(define (!= a b)
  (not (= a b))
  )

(define (fact a)
  (if (= a 1)
      1
      (* a (fact (- a 1)))
      )
  )

(define (factDig n)
  (define (iter d)
    (cond [(= (% n (fact d)) 0) #t]
          [(!= (% n (fact d)) 0) #f]
          [else (iter (+ d 1))]
          )
    )
  (cond [(or (= n 1) (= n 2) (= n 6)) #t]
        [(or (= n 3) (= n 4) (= n 5) (= n 7) (= n 8) (= n 9)) #f]
        [else (iter 4)]
        )
  )

(factDig 5040)