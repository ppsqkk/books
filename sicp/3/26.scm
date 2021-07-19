#lang sicp

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (set-entry! tree x) (set-car! tree x))
(define (set-left-branch! tree x) (set-car! (cdr tree) x))
(define (set-right-branch! tree x) (set-car! (cddr tree) x))
(define (empty-tree? tree) (null? tree))

; Only works for numerical keys
(define (assoc key tree)
  (if (empty-tree? tree)
      false
      (let ((cur-key (car (entry tree))))
        (cond ((= key cur-key) (entry tree))
              ((< key cur-key) (assoc key (left-branch tree)))
              (else (assoc key (right-branch tree)))))))
(define (adjoin key value tree)
  (if (empty-tree? tree)
      (make-tree (cons key value) '() '())
      (let ((cur-key (car (entry tree))))
        (cond ((= key cur-key) (set-cdr! (entry tree) value) tree)
              ((< key cur-key)
               (set-left-branch! tree (adjoin key value (left-branch tree))))
              (else
               (set-right-branch! tree (adjoin key value (right-branch tree)))))
        tree)))

(define (make-table) (cons '*table* '()))
(define (lookup-one key table)
  (let ((record (assoc key (cdr table))))
    (if record (cdr record) false)))
(define (insert-one! key value table)
  (set-cdr! table (adjoin key value (cdr table))))
(define (is-table? x) (and (pair? x) (eq? (car x) '*table*)))

(define (lookup key-list table)
  (cond ((null? key-list) false)
        ((null? (cdr key-list)) ; Emulate lookup-one behavior
         (lookup-one (car key-list) table))
        (else
         (let ((x (lookup-one (car key-list) table)))
           ; x must be a table because there is at least 1 more key to process
           (if (and x (is-table? x))
               (lookup (cdr key-list) x)
               false)))))
(define (insert! key-list value table)
  (cond ((null? key-list) false)
        ((null? (cdr key-list)) ; Emulate insert-one! behavior
         (insert-one! (car key-list) value table))
        (else
         (let ((x (lookup-one (car key-list) table)))
           ; If x is a table, don't overwrite it
           (if (and x (is-table? x))
               (insert! (cdr key-list) value x)
               (let ((t (make-table)))
                 (insert! (cdr key-list) value t)
                 (insert-one! (car key-list) t table)))))))

; This test proves that, if x is a table, x is not being overridden. If x were
; being overridden, the following code would still work, but (lookup '(1 2 3) t)
; would point to (*table* (3 . 1)).
(define t (make-table))
(insert! '(1 2 3) 1 t)
(insert! '(1 2 3) (lookup '(1 2) t) t)
(display (lookup '(1 2 3) t)) (newline)
