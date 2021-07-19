#lang sicp

; Subsection 3.3.3 is quite misleading, especially the line "the subtables don't
; need a special header symbol, since the key that identifies the subtable
; serves this purpose." In the strictly two-dimensional case, the decision not
; to include the header symbol does not matter, but in the any-dimensional case,
; as in Exercise 3.25, it essentially becomes impossible to differentiate an
; inner table from a regular list. Besides, the concept is inconsistent: why
; attach a special header symbol to the outermost table, but neglect to attach a
; header symbol to all inner tables as well? There is conceptually no difference
; between an outer table and an inner table, and yet they are represented
; differently in the book. This difference in representation discourages clean,
; recursive solutions in favor of opaque ones such as the book's sample
; implementations of lookup and insert! for two-dimensional tables, which make
; extensive use of cons and friends.

; The following solution builds on top of the existing implementations of lookup
; and insert! for one-dimensional tables given at the beginning of 3.3.3. It is
; short, relatively readable, and most importantly, is independent of any
; specific internal representation. For example, solving Exercise 3.26 only
; requires changing four short procedures which fully encapsulate the table
; representation.

; The main disadvantage of this solution is that we cannot use message passing.
; If we used message passing, a table would be a procedure ("table dispatch").
; Therefore, to implement is-table?, we would need a way to determine whether or
; not something is a table dispatch. No such way has been taught thus far.
; Alternatively, we might use type tags, but they are non-trivial to implement
; and offer no discernible benefit.

; An argument could be made that the implementation of two-dimensional tables in
; the book is intentionally misguiding, and that the reader is supposed to
; discover for themselves a better implementation strategy. Supposing that this
; is the case, it is worrying that most online solutions do not attempt to step
; outside of the framework that the book provides, as the authors of online
; solutions are usually the ones who are most diligent. (Solutions which use a
; list to simulate an arbitrary number of keys are wrong: after (insert! '(1 2)
; 1 t) and (insert! '(1) 1 t) for some table t, (lookup '(1 2) t) should return
; false.)

(define (make-table) (list '*table*))
(define (lookup-one key table)
  (let ((record (assoc key (cdr table))))
    (if record (cdr record) false)))
(define (insert-one! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))
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
