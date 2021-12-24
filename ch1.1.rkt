#lang racket
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(define x -123123)

;(abs  x)
;(abs2 x)
;(abs3 x)

;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))        
;  (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (choose-min a b c)
  (if (> a b) (if (> b c) c b)
              (if (> a c) c a)
      ))
(define (sumsq-bigger-two x1 x2 x3)
  (cond ((= x1 (choose-min x1 x2 x3)) (sum-of-square x2 x3))
        ((= x2 (choose-min x1 x2 x3)) (sum-of-square x1 x3))
        ((= x3 (choose-min x1 x2 x3)) (sum-of-square x1 x2))
  ))
(sumsq-bigger-two 4 3 2)

;(and (> 10 5) (< 1 3))
;(not (not (not #t)))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x)
;                 x)))

;(define (improve guess x)
;  (average guess (/ x guess)))

;(define (average x y)
;  (/ (+ x y) 2))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

;(define (sqrt x) (sqrt-iter 1.0 x))
;(sqrt 2)


;(define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))

;(define (sqrt-iter2 guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter2 (improve guess x)
;                      x)))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))4

(sqrt 2)