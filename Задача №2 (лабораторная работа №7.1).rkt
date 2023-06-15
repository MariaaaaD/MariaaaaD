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

(define (compte n)
  (define (iter ost length)
    (cond [(< n 10) 1]
          [(< (// ost 10) 10) length]
          [else (iter (// ost 10) (+ length 1))]
          )
    )
  (iter n 2)
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

(define (check n a0)
  (define (iter num)
    (cond [(= a0 (find n num)) #t]
          [(!= a0 (find n num)) (if (or (= a0 (find n (+ num 1))) (= a0 (find n (- num 1))))
                                    #t
                                    #f
                                    )
                                ]
          [else (iter (+ num 1))]
          )
    )
  (iter 1)
 )



(define (proche n)
  (define (iter num chiffre ost)
    (cond [(= 0 num) (if (and (> (find n ost) (find n (- ost 1))) (< (find n 1) (find n 2)) (< (find n 2) (find n 3)))
                                 n
                                 chiffre
                                 )
                              ]
          [(and (> n 90) (<= n 106)) 89]
          [else (if (check n 0)
                    (iter (- num 1) (+ chiffre (* (+ 1 (find chiffre ost)) (^ 10 (- num 1)))) (+ ost 1))
                    (iter (- num 1) (+ chiffre (* (+ (abs (- (find n num) (find n (- num 1)))) (find chiffre ost)) (^ 10 (- num 1)))) (+ ost 1))
                    )
                ]
          )
    )
   (iter (- (compte n) 1) (* (find n 1) (^ 10 (- (compte n) 1))) 1)
  )