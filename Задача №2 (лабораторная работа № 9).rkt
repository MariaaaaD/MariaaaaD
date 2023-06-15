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
  (iter 0 0 a)
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
  (define (iter rez lst)
    (cond [(= k (car lst)) rez]
          [else (iter (+ rez 1) (cdr lst))]
          )
    )
  (iter 1 a)
  )

(define (findmore a k)
  (define (iter lst rez top)
    (cond [(= (count a k) 1) (find a k)]
          [(= (count lst k) 0) (cdr (reverse (cons rez top)))]
          [else (iter (list-tail lst (find lst k)) (+ rez (find lst k)) (cons rez top))]
          )
    )
  (iter (list-tail a (find a k)) (find a k) (cons (find a k) '()))
  )

(define (liste a)
  (cond [(= (count a (round (middle a))) 1) (list (round (middle a)) (find a (round (middle a))))]
          [(> (count a (round (middle a))) 1) (cons (findmore (round (middle a))) (round (middle a)))]
          [else (if (= (count a (+ (round (middle a)) 1)) 1)
                    (list (+ (round (middle a)) 1) (find a (+ (round (middle a)) 1)))
                    (cons (+ (round (middle a)) 1) (findmore a (+ (round (middle a)) 1)))
                    )
                ]
          )
    )
  