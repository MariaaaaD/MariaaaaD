#lang racket
(define (% a b)
  (remainder a b)
  )

(define (// a b)
  (quotient a b)
  )

(define (!= a b)
  (not (= a b))
  )

(define (fifi n a)
  (% (// n a) a)
  )

(define (^ a b)
  (expt a b)
  )

(define (find n a0)
  (define (iter num ost)
    (cond [(< n 10) n]
          [(= a0 num) (% ost 10)]
          [else (iter (- num 1) (// ost 10))]
          )
    )
  (iter (compte n) n)
  )

(define (compte n)
  (define (iter ost length)
    (cond [(< n 10) 1]
          [(< (// ost 10) 10) length]
          [else (iter (// ost 10) (+ length 1))]
          )
    )
  (iter n 2)
  )

(define (search n)
  (define (iter a b)
    (cond [(or (= (% n b) 0) (= (% n a) 0)) (if (or (= (% (// n a) a) 0) (= (% (// n b) b) 0))
                                                 #f
                                                 #t
                                                 )
                                             ]
          [else (iter (+ a 1) (+ b 1))]
          )
    )
  (iter 2 3)
  )
          
                             