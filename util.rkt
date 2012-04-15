#lang racket

(provide (all-defined-out))

(define flatmap
  (lambda(f seq)
    (if (null? seq)
        '()
        (append (f (car seq))
                (flatmap f (cdr seq))))))
