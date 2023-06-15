#lang racket

;преобразует строки в столбцы
(define (column matr)
  (map (λ (x) (map (λ (y) (list-ref y x)) matr)) (map (λ (k) (- k 1)) (build-list (length (car matr)) (λ (m) (+ m 1)))))
  )

;находит максимальное число в списке
(define (sup lst)
  (apply max lst)
  )

;находит минимальное число в списке
(define (inf lst)
  (apply min lst)
  )

;находит номер вхождения
(define (chercheur a lst)
  (define (iter num el liste)
    (cond [(= el a) num]
          [else (iter (+ num 1) (car liste) (cdr liste))])
    )
  (iter 0 (car lst) (cdr lst))
  )

;проверяет все элементы матрицы на седловое свойство
(define (checker matr)
  (map (λ (x) (map (λ (y) (cond [(and (= y (inf x)) (= y (sup (list-ref (column matr) (chercheur (inf x) x))))) y]
                                [(and (= (sup x) (inf x)) (= (inf (list-ref (chercheur (sup x) x))) (sup (list-ref (chercheur (inf x) (column matr)))))) y]
                                [else #f])
                     ) x)) matr)
  )

;преобразует результат ф-ции checker в список из n элементов
(define (fun matr)
  (foldl (λ (y z) (append (filter (λ (x) (number? x)) y) z)) '() (checker matr))
  )

;преобразует список в число, если седловая точка только одна, и выдает список в обратном случае
(define (final matr)
  (cond [(= (length (fun matr)) 1) (car (fun matr))]
        [(= (length (fun matr)) 0) #f]
        [else (fun matr)]
        )
  )