#lang racket

(provide (all-defined-out))

(define flatmap
  (lambda(f seq)
    (if (null? seq)
        '()
        (append (f (car seq))
                (flatmap f (cdr seq))))))

(define fresh-name
  (let((counter 0))
    (lambda name
      (if(null? name)
         (fresh-name 'var)
         (let((new-name (string->symbol (string-append (symbol->string (car name)) ":" (number->string counter)))))
           (set! counter (+ 1 counter))
           new-name)))))

