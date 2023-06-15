#lang racket

;ищет минимальное число в списке
(define (minimiser lst)
  (foldl (λ (x y) (min x y)) (car lst) lst)
  )

;находит минимальные числа в каждой строке
(define (micro matr)
  (map (λ (x) (minimiser x)) matr)
  )

;ищет номер вхождения элемента в списке
(define (chercheur lst a)
  (define (iter num mem liste)
    (cond [(= mem a) num]
          [else (iter (+ num 1) (car liste) (cdr liste))]
          )
    )
  (iter 0 (car lst) (cdr lst))
  )

;ищет номер столбца, где расположено число
(define (column matr)
  (map (λ (x y) (chercheur x y)) matr (micro matr))
  )


;ищет количество вхождений
(define (counter lst a)
  (foldl (λ (x y) (if (= x a) (+ y 1) y)) 0 lst)
  )

;собирает столбцы
(define (builder matr)
  (map (λ (y) (map (λ (x) (list-ref y x)) (maker (column matr)))) matr)
  )

;отсеивает повторяющиеся номера столбцов и те, которые не повторяются
(define (maker lst)
  (filter (λ (x) (>= (counter lst x) 2)) lst)
  )