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

(define (division a)
  (define (iter lst prev next rez)
    (cond [(!= (% prev 2) (% next 2)) (cdr (reverse (cons prev rez)))]
          [(empty? lst) a]
          [else (iter (cdr lst) next (car lst) (cons prev rez))]
          )
    )
  (iter (cdr a) (car a) (car a) '())
  )

(define (collector a)
  (define (iter rez liste)
    (cond [(empty? liste) (reverse rez)]
          [else (iter (cons (division liste) rez) (list-tail liste (length (division liste))))]
          )
    )
  (iter '() a)
  )

(define (final a)
  (define (iter num prev next lst rez)
    (cond [(= num (length (collector a))) rez]
          [else (if (= (max (length prev) (length next)) (length prev))
                    (iter (+ num 1) next (car (collector a)) (cdr lst) prev)
                    (iter (+ num 1) next (car (collector a)) (cdr lst) next)
                    )
                ]
          )
    )
  (iter 1 (car (collector a)) (car (cdr (collector a))) (collector a) '())
  )
