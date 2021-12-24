#lang racket
(define (cube x) (* x x x))

;(define (sum-integers a b)
;  (if (> a b)
;      0
;      (+ a (sum-integers (+ a 1) b))))

;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a) (sum-cubes (+ a 1) b))))

;(define (pi-sum a b)
;  (if (> a b)
;      0
;      (+ (/ 1.0 (* a (+ a 2)))
;         (pi-sum (+ a 4) b))))

;(define (<name> a b)
;  (if (> a b)
;      0
;      (+ (<term> a)
;         (<name> (<next a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum1 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral1 f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;(lambda (<formal-parameters>) <body>)
;(define (plus4 x) (+ x 4))
;(define plus4 (lambda (x) (+ x 4)))
(define square (lambda (x) (* x x)))
;((lambda (x y z) (+ x y (square z))) 1 2 3)
#|
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))
|#
#|
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
;equivalent to
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (-1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
|#
#|           
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      (<var3> <exp3>)
      ...
      (<varn> <expn>))
  <body>)
;equivalent to
((lambda (<var1> ... <varn>)
   <body>)
 <exp1>
 ...
 <expn>)
|#
#|
(define x 5)
(+ (let ((x 3))
      (+ x ( * x 10)))
    x)

(define x 2)
(let ((x 3)
      (y (+ x 2)))
  (* x y))
|#
#|
(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))
|#
;(define (f g) (g 2))


(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average a b) (/ (+ a b) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Value are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))
      
(define (sqrt1 x)
  (fixed-point (lambda (y) (average y(/ x y)))
               1.0))

(define (golden-ratio x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt3 x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))