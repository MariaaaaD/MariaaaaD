#lang scheme

(define (l_checker a pair)
  (if (= a (car pair))
      #t
      #f
      )
  )

(define (r_checker a pair)
  (if (= a (cdr  pair))
      #t
      #f
      )
  )

(define (l_counter a lst)
  (foldl (λ (x y) (if (l_checker a x) (+ y 1) y)) 0 lst)
  )

(define (r_counter a lst)
  (foldl (λ (x y) (if (r_checker a x) (+ y 1) y)) 0 lst)
  )

(define (checker a lst)
  (ormap (λ (x) (= x a)) lst)
  )

(define (unfold lst)
  (foldl (λ (x y) (if (empty? y) (cons (cdr x) (cons (car x) y)) (cond [(and (checker (car x) y) (checker (cdr x) y)) y]
                                                                       [(and (checker (car x) y) (not (checker (cdr x) y))) (cons (cdr x) y)]
                                                                       [else (cons (car x) y)]
                                                                       )
                      )) '() lst)
  )


(define (maker lst)
  (define (iter num rez liste el)
    (cond [(= num (length lst)) rez]
          [else (iter (+ num 1) (cond [(and (= (r_counter el lst) 1) (= (l_counter el lst) 0)) (list (list el '() '()) rez)]
                                      [(and (= (r_counter el lst) 1) (= (l_counter el lst) 1)) (cons el rez