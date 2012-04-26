#lang racket
(require "type.rkt")
(provide no-occurrence?)

(define no-occurrence?
  (lambda(tvar ty)
    (cond((number-type? ty) #t)
         ((boolean-type? ty) #t)
         ((void-type? ty) #t)
         ((procedure-type? ty)
          (and (no-occurrence? tvar (procedure-type->arg-type ty))
               (no-occurrence? tvar (procedure-type->result-type ty))))
         ((tuple-type? ty) (andmap (lambda(ty)
                                     (no-occurrence? tvar ty))
                                   (tuple-type->types ty)))
         ((var-type? ty) (not (equal? tvar ty)))
         )))
