#lang racket

(define (!= a b)
  (not (= a b))
  )

(define (^ a b)
  (expt a b)
  )

(define (% a b)
  (remainder a b)
  )

(define (// a b)
  (quotient a b)
  )

(define (length n)
  (define (iter chiffre rez)
    (cond [(= (// chiffre 10) 0) rez]
          [else (iter (// chiffre 10) (+ rez 1))]
          )
    )
  (iter n 1)
  )

;разворот числа
(define (mirroir n)
  (define (iter num chiffre rez sh)
    (cond [(= num (length n)) rez]
          [else (iter (+ num 1) (// chiffre 10) (+ (* (% (// chiffre 10) 10) (^ 10 (- sh 1))) rez) (- sh 1))]
          )
    )
  (iter 1 n (* (% n 10) (^ 10 (- (length n) 1))) (- (length n) 1))
  )

(define (transformation a b)
  (define (iter lst lst1)
    (cond [(empty? lst) #t]
          [(!= (car lst1) (mirroir (car lst))) #f]
          [else (iter (cdr lst) (cdr lst1))]
          )
    )
  (iter a (reverse b))
  )