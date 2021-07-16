#lang sicp

(define (count-pairs x)
  ; A set would be faster here
  (let ((seen '()))
    (define (iter x)
      (if (or (not (pair? x)) (memq x seen))
          0
          (begin (set! seen (cons x seen))
                 (let ((a (iter (car x))))
                   (let ((b (iter (cdr x))))
                     (+ a b 1))))))
    (iter x)))


(define w '(a b c))
(count-pairs w)

(define x
  (let ((p '((a) b)))
    (set-cdr! (car p) (cdr p))
    p))
(count-pairs x)

(define y
  (let ((p (cons 'a 'b)))
    (let ((q (cons p p)))
      (cons q q))))
(count-pairs y)

(define z
  (let ((p '(a b c)))
    (set-cdr! (cddr p) p)
    p))
(count-pairs z)
