#lang racket
(require "type.rkt")
(provide no-occurrence?)

(define no-occurrence?
  (lambda(tvar ty)
    (cond((number-type? ty) ... )
         ((boolean-type? ty) ... )
         ((void-type? ty) ... )
         ((procedure-type? ty) ... )
         ((tuple-type? ty) ... )
         ((var-type? ty) ...)
         )))
