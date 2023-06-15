#lang racket

(define (// a b)
  (quotient a b)
  )

(define (^ a b)
  (expt a b)
  )

(define (length a)
  (define (iter lst rez)
    (if (empty? lst) rez
        (iter (cdr lst) (+ rez 1))
        )
    )
  (iter a 0)
  )

(define (find a k)
  (define const (^ 10 (- k 1)))
  (define (iter num rez lst)
    (cond [(= num (+ (length a) 1)) rez]
          [else (if (and (< (// (car lst)const) 10) (>= (// (car lst) const) 0))
                    (iter (+ num 1) (* rez (car lst)) (cdr lst))
                    (iter (+ num 1) rez (cdr lst))
                    )
                ]
          )
    )
  (if (and (< (// (car a) const) 10) (>= (// (car a) const) 0))
      (iter 1 (car a) a)
      (iter 1 1 a)
      )
  )