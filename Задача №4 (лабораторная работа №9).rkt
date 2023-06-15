#lang racket

(define (!= a b)
  (not (= a b))
  )

 (define (% a b)
  (remainder a b)
  )

(define (// a b)
  (quotient a b)
  )

(define (find a k)
  (define (iter num chiffre lst)
    (cond [(= chiffre k) num]
          [else (iter (+ num 1) (car lst) (cdr lst))]
          )
    )
  (iter 0 0 a)
  )


(define (divide a)
  (define (iter lst rez)
    (cond [(empty? lst) (reverse rez)]
          [else (if (= (% (car lst) (find a (car lst))) 0)
                    (iter (cdr lst) (cons (car lst) rez))
                    (iter (cdr lst) rez)
                    )
                ]
          )
    )
  (iter a '())
  )