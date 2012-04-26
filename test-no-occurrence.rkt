#lang racket
(require "type.rkt")
(require "no-occurrence.rkt")
(require "utils.rkt")


(define tvar (var-type))

(run-tests
 (test (no-occurrence? tvar (var-type)) 
       => #t
       )
 
 (test (no-occurrence? tvar tvar) 
       => #f
       )
 
 (test (no-occurrence? tvar (number-type)) 
       => #t
       )
 (test (no-occurrence? tvar (tuple-type (list (number-type)))) 
       => #t
       )
 (test (no-occurrence? tvar (tuple-type (list tvar))) 
       => #f
       )
 )

