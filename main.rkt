#lang racket
;; Author: Bar Orion Barak.
(require "type.rkt")
(require "language.rkt")


;; Sample usages          
;; (tuple-type (list (number-type) (var-type)))
;;(type->human-form (procedure-type (tuple-type (list (number-type) (var-type))) (var-type)))
;;(type->human-form (procedure-type (void-type) (var-type)))

(define p
  (lambda(exp)
    (parse exp (lambda(x) x) (lambda() 'failed))))

;;(p 1)
;;(p 'x)
;;(p '(if 1 2 x))
;;(p '(lambda(f g) 1 3 (if f f g)))
;;(p '(let ((foo bar)) foo bar))
;;(p '(letrec ((foo bar)) foo bar))
;;(p '(foo (letrec ((foo bar)) foo bar)))



