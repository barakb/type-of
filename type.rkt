#lang racket

;; Author: Bar Orion Barak.

;; Types Grammer
;;;;;;;;;;;;;;;;;;;;;;
;; Type -> 'number'
;; Type -> 'boolean'
;; Type -> '[' Type+ '->' Type ']'
;; Type -> '[' 'void' '->' Type ']'
;; Type -> 'type-var'

(require "parse-tree-utils.rkt")

(provide (except-out (all-defined-out)
                     tuple-type->humen-form))

;; (provide (all-defined-out))

;; Type -> 'number'
(define number-type 
  (lambda()
    (tag 'number-type)))

(define number-type? (make-pt-pred 'number-type))

;; Type -> 'boolean'
(define boolean-type 
  (lambda()
    (tag 'boolean-type)))
(define boolean-type? (make-pt-pred 'boolean-type))

;; void
(define void-type
  (lambda()
    (tag 'void-type)))
(define void-type? (make-pt-pred 'void-type))

(define tuple-type
  (lambda(types)
    (tag 'tuple-type types)))
(define tuple-type? (make-pt-pred 'tuple-type))
(define tuple-type->types pt-take-first)

;; Type -> '[' Type+ '->' Type ']'
;; Type -> '[' 'void' '->' Type ']'
(define procedure-type 
  (lambda(arg-type result-type)
    (tag 'procedure-type arg-type result-type)))

(define procedure-type->arg-type pt-take-first)

(define procedure-type->result-type pt-take-second)

(define procedure-type? (make-pt-pred 'procedure-type))

(define var-type
  (let((serial 0))
    (lambda()
      (let ((res (tag 'var-type serial)))
        (set! serial (+ 1 serial))
        res))))

(define var-type? (make-pt-pred 'var-type))

(define var-type->serial pt-take-first)


(define type->human-form
  (lambda(type)
    (cond ((number-type? type) 'NUMBER)
          ((boolean-type? type) 'BOOLEAN)
          ((void-type? type) 'VOID)
          ((tuple-type? type) (tuple-type->humen-form type))
          ((var-type? type) (string->symbol (string-append "T" (number->string (var-type->serial type)))))
          ((procedure-type? type) 
           (let ((arg-type (procedure-type->arg-type type)))
             (if(tuple-type? arg-type)
                (append (tuple-type->humen-form  arg-type) '(->) (list (type->human-form (procedure-type->result-type type))))
                (list (type->human-form  arg-type) '-> (type->human-form (procedure-type->result-type type)))))))))


(define tuple-type->humen-form
  (lambda(tuple)
    (letrec ((addbetween (lambda(lst seperator)
                           (cond ((null? lst) lst)
                                 ((= 1 (length lst)) (list (type->human-form (car lst))))
                                 (else
                                  (cons (type->human-form (car lst)) (cons seperator (addbetween (cdr lst) seperator))))))))
      (addbetween (tuple-type->types tuple) '*))))


(define normalize-type
  (lambda(type)
    (normalize type '() (lambda(type env) type))))

(define normalize-types
  (lambda(types env cont)
    (cond((null? types) (cont types env))
         (else (normalize (car types) env (lambda(normalized-first env)
                                            (normalize-types (cdr types) env
                                                             (lambda(normalized-rest env)
                                                               (cont (cons normalized-first normalized-rest) env))))))))) 

(define normalize
  (lambda(type env cont)
    (cond ((number-type? type) (cont type env))
          ((boolean-type? type) (cont type env))
          ((void-type? type) (cont type env))
          ((tuple-type? type) (normalize-types (tuple-type->types type) env 
                                               (lambda(types env)
                                                 (cont (tuple-type types) env))))
          ((var-type? type) (let ((found (assoc type env)))
                              (if found 
                                  (cont (cdr found) env)
                                  (let ((var (tag 'var-type (length env))))
                                    (cont var (cons (cons type var) env))))))
          ((procedure-type? type) 
           (normalize (procedure-type->arg-type type) 
                      env
                      (lambda(arg-type env)
                        (normalize (procedure-type->result-type type)
                                   env
                                   (lambda(result-type env)
                                     (cont (procedure-type arg-type result-type) env)))))))))


;; Sample usages          
;; (type->human-form (procedure-type (tuple-type (list (number-type) (var-type))) (var-type)))
;; (type->human-form (procedure-type (void-type) (var-type)))


;;(require racket/trace)
;;(trace normalize-type)
