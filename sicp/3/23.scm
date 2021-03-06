#lang sicp

(define (front-ptr dequeue) (car dequeue))
(define (rear-ptr dequeue) (cdr dequeue))
(define (set-front-ptr! dequeue item)
  (set-car! dequeue item))
(define (set-rear-ptr! dequeue item)
  (set-cdr! dequeue item))
(define (empty-dequeue? dequeue)
  (or (null? (front-ptr dequeue))
      (null? (rear-ptr dequeue))))
(define (make-dequeue) (cons '() '()))

(define (front-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "FRONT called with an empty dequeue" dequeue)
      (car (front-ptr dequeue))))
(define (rear-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "REAR called with an empty dequeue" dequeue)
      (car (rear-ptr dequeue))))
(define (front-insert-dequeue! dequeue item)
  (let ((new-pair (list item '() '())))
    (cond ((empty-dequeue? dequeue)
           (set-front-ptr! dequeue new-pair)
           (set-rear-ptr! dequeue new-pair)
           dequeue)
          (else
           (set-car! (cddr new-pair) (front-ptr dequeue))
           (set-car! (cdr (front-ptr dequeue)) new-pair)
           (set-front-ptr! dequeue new-pair)
           dequeue))))
(define (rear-insert-dequeue! dequeue item)
  (let ((new-pair (list item '() '())))
    (cond ((empty-dequeue? dequeue)
           (set-front-ptr! dequeue new-pair)
           (set-rear-ptr! dequeue new-pair)
           dequeue)
          (else
           (set-car! (cdr new-pair) (rear-ptr dequeue))
           (set-car! (cddr (rear-ptr dequeue)) new-pair)
           (set-rear-ptr! dequeue new-pair)
           dequeue))))
(define (front-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "FRONT-DELETE! called with an empty dequeue" dequeue))
        (else (set-front-ptr! dequeue (caddr (front-ptr dequeue)))
              (if (not (null? (front-ptr dequeue)))
                  (set-car! (cdr (front-ptr dequeue)) '()))
              dequeue)))
(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "REAR-DELETE! called with an empty dequeue" dequeue))
        (else (set-rear-ptr! dequeue (cadr (rear-ptr dequeue)))
              (if (not (null? (rear-ptr dequeue)))
                  (set-car! (cddr (rear-ptr dequeue)) '()))
              dequeue)))
