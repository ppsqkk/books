#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (unique-tuples n k)
  (cond ((= k 0) (list nil))
        ((= n 0) nil)
        (else (flatmap (lambda (x)
                         (map (lambda (y) (cons x y))
                              (unique-tuples (- x 1) (- k 1))))
                       (enumerate-interval 1 n)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (sum-triples n s)
  (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s))
          (unique-tuples n 3)))
