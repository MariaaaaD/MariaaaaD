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

(define (fifi n)
  (% (// n 10) 10)
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

(define (progressive n)
  (define (iter num)
    (cond [(= (% n 10) 0) #f]
      [(= (compte n) num) (if (or (= (% (find n num) (find n (- num 1))) 0) (= (% (find n (- num 1)) (find n num)) 0))
                                  #t
                                  (if (and (> (% (find n num) (find n (- num 1))) 0) (> (% (find n (- num 1)) (find n num)) 0) (!= (abs (- (find n (- num 1)) (find n num))) (abs (- (fifi n) (% n 10)))))
                                      #f
                                      (if (!= (abs (- (find n 1) (find n 2))) (abs (- (fifi n) (% n 10))))
                                          #f
                                          #t
                                      )
                                  )
                                 )
                              ]
          [else (iter (+ num 1))]
          )
    )
  (iter 1)
  )