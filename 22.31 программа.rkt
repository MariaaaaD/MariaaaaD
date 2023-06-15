#lang racket

(define (% a b)
  (remainder a b)
  )

(define (// a b)
  (quotient a b)
  )

(define (length a)
  (define (iter lst rez)
    (if (empty? lst) rez
        (iter (cdr lst) (+ rez 1))
        )
    )
  (iter a 0)
  )

(define (middle a)
  (define (iter num rez lst)
    (cond [(= num (length a)) (/ rez (length a))]
          [else (iter (+ num 1) (+ rez (car lst)) (cdr lst))]
          )
    )
  (iter 1 (car a) a)
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

(define (find a k)
  (define (iter num chiffre lst)
    (cond [(= chiffre k) num]
          [else (iter (+ num 1) (car lst) (cdr lst))]
          )
    )
  (iter 0 (car a) a)
  )

(define (liste a)
  (define (iter num rez lst)
    (cond [(= num (find a )  (reverse rez)]
          [else (if (= (count a (round (middle a))) 1)
                    (iter (cons (find a (round (middle a))) rez) (cdr lst))
                    (iter (cons (cdr lst) (round (middle a) rez)) (cdr lst))
                    )
                ]
          )
    )
  (iter '((round (middle a))) a)
  )