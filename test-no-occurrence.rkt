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
 (test (no-occurrence? tvar (tuple-type (list (void-type)))) 
       => #t
       )
 (test (no-occurrence? tvar (tuple-type (list tvar))) 
       => #f
       )
 
 (test (no-occurrence? tvar (procedure-type (tuple-type (list tvar)) (number-type))) 
       => #f
       )
 
 (test (no-occurrence? tvar (procedure-type (tuple-type (list (var-type))) (var-type))) 
       => #t
       )
 
 (test (no-occurrence? tvar (procedure-type (tuple-type (list (number-type))) (number-type))) 
       => #t
       )
 
 (test (no-occurrence? tvar (procedure-type (tuple-type (list (number-type))) tvar)) 
       => #f
       )
 (test (no-occurrence? tvar (procedure-type (tuple-type (list (number-type))) (boolean-type))) 
       => #t
       )
 )

