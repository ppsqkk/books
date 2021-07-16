#lang sicp

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (area x1 x2 y1 y2) (* (- x2 x1) (- y2 y1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (area x1 x2 y1 y2) (monte-carlo trials experiment)))

; (define (square x) (* x x))
; (define (<= x y) (not (> x y)))
; (define trials 100000)
; (estimate-integral (lambda (x y) (<= (+ (square (- x 5)) (square (- y 7)))
;                                      (square 3)))
;                    2.0 8.0 4.0 10.0 trials)
