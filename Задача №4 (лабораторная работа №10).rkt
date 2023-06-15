#lang racket

(define (% a b)
  (remainder a b)
  )

(define (checkdel a b)
  (define (iter num)
    (cond [(and (= (% a num) 0) (= (% b num) 0)) #t]
          [(and (> (% a (+ num 1)) 0) (> (% b (+ num 1)) 0)) #f]
          [else (iter (+ num 1))]
          )
    )
  (iter 2)
  )

(define (Dieu a b)
  (define (iter lst liste rez)
    (cond [(and (> (length a) (length b)) (empty? liste)) (reverse rez)]
          [(and (< (length a) (length b)) (empty? lst)) (reverse rez)]
          [(and (= (length a) (length b)) (empty? liste)) (reverse rez)]
          [else (if (checkdel (car lst) (car liste))
                    (iter (cdr lst) (cdr liste) rez)
                    (iter (cdr lst) (cdr liste) (cons (cons (car lst) (car liste)) rez))
                    )
                ]
          )
    )
  (iter a b '())
  )
    