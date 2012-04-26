#lang racket
(require "language.rkt")
(require "ex3-helpers.rkt")

(provide free-vars)


(define free-vars
  (lambda(pt)
    (free-vars2 pt '())))

(define free-vars2
  (lambda(pt bindings)
    (cond ((number-pt? pt) ... )
          ((var-pt? pt) ... )
          ((if-pt? pt) ... )
          ((app-pt? pt) ... )
          ((procedure-pt? pt) ... )
          ((let-pt? pt) ... )
          ((letrec-pt? pt) ... )
          
          (else ...)
)))





