#lang racket
(require "ex3-helpers.rkt")
(require "language.rkt")
(provide rename)



(define rename
  (lambda(pt)
    (rename2 pt '())))

(define rename2
  (lambda(pt bindings)
    (cond ((number-pt? pt) ...)
          ((var-pt? pt)  ... )
          ((if-pt? pt)... )
          ((app-pt? pt) ... )
          ((procedure-pt? pt) ...)
          ((let-pt? pt) ...) 
          ((letrec-pt? pt) ...) 
          )))          

 ;;(require racket/trace)
 ;;(trace rename2)
  

