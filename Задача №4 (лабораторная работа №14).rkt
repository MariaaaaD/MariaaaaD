#lang racket
(define (gap str2)
  (map (λ (x) (string x)) (string->list str2))
  )

(define (maker str1 str2)
  (filter (λ (k) (ormap (λ (y) (string=? (string (string-ref str1 k)) y)) (gap str2))) (build-list (string-length str1) +))
  )

(define (createur str1 str2)
 (map (λ (x y) (substring str1 x y)) (cond [(and (member? 0 (maker str1 str2)) (member? (- (string-length str1) 1) (maker str1 str2))) (map (λ (k) (+ k 1)) (take (maker str1 str2) (- (length (maker str1 str2)) 1)))]
                                         [(member? 0 (maker str1 str2)) (cdr (maker str1 str2))]
                                         [(member? (string-length str1) (maker str1 str2)) (cons 0  (map (λ (k) (+ k 1)) (take (maker str1 str2) (- (length (maker str1 str2)) 1))))]
                                         [else (cons 0 (map (λ (x) (+ x 1)) (maker str1 str2)))]) (cond [(and (member? 0 (maker str1 str2)) (member? (- (string-length str1) 1) (maker str1 str2))) (cdr (maker str1 str2))]
                                                                                                                                         [(member? 0 (maker str1 str2)) (reverse (cons (string-length str1) (reverse (cdr (maker str1 str2)))))]
                                                                                                                                         [(member? (string-length str1) (maker str1 str2)) (maker str1 str2)]
                                                                                                                                         [else (reverse (cons (string-length str1) (reverse (maker str1 str2))))]))
                                                                                                                                         
  )

(define (member? a lst)
  (ormap (λ (x) (= x a)) lst)
  )

(define (final str1 str2)
  (filter (λ (x) (not (string=? x ""))) (createur str1 str2))
  )

(define (counter a lst)
  (foldl (λ (x y) (if (string=? x a) (+ y 1) y)) 0 lst)
  )

(define (check str1 str2)
  (filter (λ (x) (>= (counter x (final str1 str2)) 2)) (final str1 str2))
  )

(define (fin str1 str2)
  (if (empty? (check str1 str2))
      #f
      (check str1 str2)
      )
  )