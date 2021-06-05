#lang sicp

(define (square x)
  (* x x))

(define (expmod base exp m)
  (define (f x)
    (if (and (not (= x (- m 1)))
             (not (= x 1))
             (= (remainder (square x) m) 1))
        0
        x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (f (expmod base (/ exp 2) m)))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
