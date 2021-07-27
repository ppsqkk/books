#lang sicp

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((even? start) (search-for-primes (+ start 1) end))
        ((not (> start end)) (timed-prime-test start)
                             (search-for-primes (+ start 2) end))))

; Larger than 1000: 1009, 1013, 1019
; Larger than 10000: 10007, 10009, 10037
; Larger than 100000: 100003, 100019, 100043
; Larger than 1000000: 1000003, 1000033, 1000037
