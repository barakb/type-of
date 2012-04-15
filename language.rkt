#lang racket
;; Author: Bar Orion Barak.

;; Language Grammer.
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program -> Expression
;; Expression -> 'number'
;; Expression -> 'variable'
;; Expression -> '(' 'if' Expression Expression Expression')'
;; Expression -> Procedure 
;; Procedure -> '(' 'lambda' '(' 'variable'* ')' Expression+ ')'
;; Expression -> '(' 'let' '(' '(' 'variable' Expression ')' ')' Expression+ ')'
;; Expression -> '(' 'letrec' '(' '(' 'variable' Procedure ')' ')' Expression+ ')'
;; Expression -> '(' Expression+ ')'

;; primitive procedures 
;; '*' : [number * ... * number -> number]
;; '+' : [number * ... * number -> number]
;; '-' : [number * ... * number -> number]
;; '<' : [number * ... * number -> boolean]
;; '=' : [number * ... * number -> boolean]
;; 'zero?' [number -> boolean]

;; An if can have only boolean as its test and both branches should have the same type (no union of us)

;; The start symbol for this grammer is Program.

;; Abstract Parse Tree

(require "parse-tree-utils.rkt")

(define primitive-names '(* + - < = zero?))


(provide (except-out (all-defined-out)
                     parse-let-type let-type-exp? parse-seq let-exp? letrec-exp? lambda-exp? if-exp?))


;; Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> 'number'
(define number-pt
  (lambda(value)
    (tag 'number value)))

(define number-pt? (make-pt-pred 'number))

(define number-pt->value
  (lambda(pt)
    (pt-ref pt 0)))

;; Variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> 'variable'
(define var-pt
  (lambda(name)
    (tag 'var name)))

(define var-pt? (make-pt-pred 'var))

(define var-pt->name pt-take-first)

;; If
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> '(' 'if' Expression Expression Expression')'
(define if-pt
  (lambda(test-pt do-if-true-pt do-if-false-pt)
    (tag 'if test-pt do-if-true-pt do-if-false-pt)))

(define if-pt? (make-pt-pred 'if))

(define if-pt->test-pt pt-take-first)
(define if-pt->do-if-true-pt pt-take-second)
(define if-pt->do-if-false-pt pt-take-third)


;; User defined procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> '(' 'lambda' '(' 'variable'* ')' Expression+ ')'
(define procedure-pt
  (lambda(formals list-of-parsed-bodies)
    (tag 'procedure formals list-of-parsed-bodies)))

(define procedure-pt? (make-pt-pred 'procedure))

(define procedure->formals pt-take-first)

(define procedure->bodies pt-take-second)

;; Let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> '(' 'let' '(' '(' 'variable' Expression ')' ')' Expression+ ')'
(define let-pt
  (lambda(var parsed-exp parsed-bodies)
    (tag 'let var parsed-exp parsed-bodies)))

(define let-pt? (make-pt-pred 'let))

(define let-pt->var pt-take-first)

(define let-pt->expression pt-take-second)

(define let-pt->bodies pt-take-third)

;; Letrec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> '(' 'letrec' '(' '(' 'variable' Expression ')' ')' Expression+ ')'
(define letrec-pt
  (lambda(var parsed-exp parsed-bodies)
    (tag 'letrec var parsed-exp parsed-bodies)))

(define letrec-pt? (make-pt-pred 'letrec))

(define letrec-pt->var pt-take-first)

(define letrec-pt->procedure pt-take-second)

(define letrec-pt->bodies pt-take-third)

;; Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression -> '(' Expression+ ')'  
(define app-pt
  (lambda(rator-pt rands-pt)
    (tag 'app rator-pt rands-pt)))

(define app-pt? (make-pt-pred 'app))

(define app-pt->rator-pt pt-take-first)

(define app-pt->rands-pt pt-take-second)

;; Language Grammer.
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program -> Expression
;; Expression -> 'number'
;; Expression -> '(' Expression+ ')'
;; Expression -> '(' 'if' Expression Expression Expression')'
;; Expression -> 'variable'
;; Expression -> '(' 'lambda' '(' 'variable'* ')' Expression+ ')'
;; Expression -> '(' 'let' '(' '(' 'variable' Expression ')' ')' Expression+ ')'
;; Expression -> '(' 'letrec' '(' '(' 'variable' Expression ')' ')' Expression+ ')'

(define parse
  (lambda(exp success fail)
    (cond ((number? exp) (success (number-pt exp)))
          ((symbol? exp) (success (var-pt exp)))
          ((if-exp? exp) (parse (cadr exp)
                                (lambda(parsed-test)
                                  (parse (caddr exp) (lambda(parsed-dit)
                                                       (parse (cadddr exp)
                                                              (lambda(parsed-dif)
                                                                (success (if-pt parsed-test parsed-dit parsed-dif)))
                                                              fail))
                                         fail))
                                fail))
          ((lambda-exp? exp) (parse-seq (cddr exp)
                                        (lambda(bodies-pt)
                                          (success (procedure-pt (cadr exp) bodies-pt)))
                                        fail))
          ((let-exp? exp) (parse-let-type exp let-pt success fail))
          ((letrec-exp? exp) (parse-let-type exp letrec-pt success fail))
          ((and (pair? exp) (not (null? exp)) (not (memq (car exp) '(if lambda let letrec))))
           (parse-seq exp (lambda(parsed-seq)
                            (success (app-pt (car parsed-seq) (cdr parsed-seq))))
                      fail))
          (else (fail)))))

(define parse-let-type
  (lambda(exp pt-constractor success fail)
    (let ((binding (caadr exp))
          (bodies (cddr exp)))
      (parse-seq bodies
                 (lambda(parsed-bodies)
                   (parse (cadr binding) (lambda(parsed-exp)
                                           (if (and (eq? pt-constractor letrec-pt) (not (procedure-pt? parsed-exp)))
                                               (fail)
                                               (success (pt-constractor (car binding) parsed-exp parsed-bodies))))
                          fail))
                 fail))))

(define parse-seq
  (lambda(seq success fail)
    (if (null? seq)
        (success seq)
        (parse (car seq)
               (lambda(first-pt)
                 (parse-seq (cdr seq)
                            (lambda(rest-pts)
                              (success (cons first-pt rest-pts)))
                            fail))
               fail))))


(define let-type-exp?
  (lambda(exp keyward)
    (and (pair? exp) (< 2 (length exp)) (eq? keyward (car exp))
         (andmap (lambda(binding)
                   (and (pair? binding)
                        (= 2 (length binding))))
                 (cadr exp)))))

(define let-exp?
  (lambda(exp)
    (let-type-exp? exp 'let)))

(define letrec-exp?
  (lambda(exp)
    (let-type-exp? exp 'letrec)))

(define lambda-exp?
  (lambda(exp)
    (and (pair? exp) (< 2 (length exp)) (eq? 'lambda (car exp)))))

(define if-exp?
  (lambda(exp)
    (and (pair? exp) (= 4 (length exp)) (eq? 'if (car exp)))))


