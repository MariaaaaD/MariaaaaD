#lang racket

(define (!= a b)
  (not (= a b))
  )

(define (count a k)
  (define (iter num lst)
    (cond [(empty? lst) num]
          [else (if (= (car lst) k)
                    (iter (+ num 1) (cdr lst))
                    (iter  num (cdr lst))
                    )
                ]
          )
    )
  (iter 0 a)
  )

(define (fib n)
  (define (iter a b p)
    (if (= p n)
        b
        (iter b (+ a b) (+ 1 p))
        )
    )
  (iter 0 1 0)
  )

(define (checkfib n)
  (define (iter num)
    (cond [(= n (fib num)) #t]
          [(< n (fib num)) #f]
          [else (iter (+ num 1))]
          )
    )
  (iter 0)
  )

(define (hunterfib a)
  (define (iter num lst)
    (cond [(empty? lst) num]
          [else (if (checkfib (car lst))
                    (if (= (count lst (car lst)) 1)
                        (iter (+ num 1) (cdr lst))
                        (iter num (cdr lst))
                        )
                    (iter num (cdr lst))
                    )
                ]
          )
    )
  (iter 0 a)
  )