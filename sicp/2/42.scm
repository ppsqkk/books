#lang sicp

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-position row col) (cons row col))
(define (row position) (car position))
(define (col position) (cdr position))

(define (adjoin-position row col rest)
  (cons (make-position row col) rest))

(define empty-board nil)

; We don't care about col, because with our representation, the proposed
; position is always at the front.
(define (safe? col positions)
  (let ((proposed-position (car positions))
        (test-against-list (cdr positions)))
    (accumulate (lambda (x y) (and x y))
                true
                (map (lambda (test-against)
                       (not (attacks? test-against proposed-position)))
                     test-against-list))))
; Strangely, (accumulate and true some-list) doesn't work, but
; (accumulate (lambda (x y) (and x y)) true some-list) does. The difference
; between the two expressions is shown in Exercise 1.6, but that does not
; explain why the first expression fails. The way the special form and is
; described in the book, there should ultimately be no difference.

(define (attacks? p1 p2)
  (define (horizontally-attacks? p1 p2)
    (= (row p1) (row p2)))
  (define (vertically-attacks? p1 p2)
    (= (col p1) (col p2)))
  (define (diagonally-attacks? p1 p2)
    (= (abs (- (col p1) (col p2)))
       (abs (- (row p1) (row p2)))))
  (or (horizontally-attacks? p1 p2)
      (vertically-attacks? p1 p2)
      (diagonally-attacks? p1 p2)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

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
