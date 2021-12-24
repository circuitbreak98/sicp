
#lang racket
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial3 n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   )))
  (fact-iter 1 1))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (+a a b)
  (if (= a 0)
      b
      (inc (+a (dec a) b))))

(define (+z a b)
  (if (= a 0)
      b
      (+z (dec a) (inc b))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
    
  (fib-iter 1 0 n))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
        
(define (triple-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (triple-fib (- n 1))
                 (* (triple-fib (- n 2)) 2)
                 (* (triple-fib (- n 3)) 3)))))

(define (pascal x y)
  (cond ((= x y) 1)
        ((= y 0) 1)
        (else (+ (pascal (- x 1) y)
                 (pascal (- x 1) (- y 1))))))
        
  
(define (expt2 b n)
  (if (= n 0)
      1
      (* b (expt2 b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt2 b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt2 b (/ n 2))))
        (else (* b (fast-expt2 b (- n 1))))))

(define (fast-expt b n)
  (define (even? n) (= (remainder n 2) 0 ))
  (define (square x) (* x x))
  (define (fast-expt-iter base counter product)
    (cond ((= counter 0) product)
          ((even? counter) (fast-expt-iter (square base) (/ counter 2) product))
          (else (fast-expt-iter base (- counter 1) (* product base)))))
  (fast-expt-iter b n 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
        
         