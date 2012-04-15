#lang racket
(require "util.rkt")
(require "language.rkt")

(provide (except-out (all-defined-out)
                     fv))
(define free-vars
  (lambda(pt bindings)
    (cond ((number-pt? pt) '())
          ((var-pt? pt) (if (memq (var-pt->name pt) bindings)
                            '()
                            (list (var-pt->name pt))))
          ((if-pt? pt) (append (free-vars (if-pt->test-pt pt) bindings)
                               (free-vars (if-pt->do-if-true-pt pt) bindings)
                               (free-vars (if-pt->do-if-false-pt pt) bindings)))
          ((app-pt? pt) (append (free-vars (app-pt->rator-pt pt) bindings)
                                (flatmap (lambda(pt)
                                           (free-vars pt bindings))
                                         (app-pt->rands-pt pt))))
          ((procedure-pt? pt) (flatmap (lambda(body-pt)
                                         (free-vars body-pt 
                                                    (append (procedure->formals pt) bindings)))
                                       (procedure->bodies pt)))
          ((let-pt? pt) (append (free-vars (let-pt->expression pt) bindings)
                                (flatmap (lambda(body-pt)
                                           (free-vars body-pt 
                                                      (cons (let-pt->var pt) bindings)))
                                         (let-pt->bodies pt))))
          ((letrec-pt? pt) (append (free-vars (letrec-pt->procedure pt) (cons (letrec-pt->var pt) bindings))
                                   (flatmap (lambda(body-pt)
                                              (free-vars body-pt 
                                                         (cons (letrec-pt->var pt) bindings)))
                                            (letrec-pt->bodies pt))))
          
          (else '()))))




(define free-occurrence-check 
  (lambda(exp success fail)
    (parse exp (lambda(pt)
                 (let ((fv (free-vars pt primitive-names)))
                   (if(null? fv)
                      (success pt)
                      (fail fv))))
           (lambda()
             (fail 'parse)))))

(define fv
  (lambda(exp)
     (free-occurrence-check exp (lambda(pt) pt) (lambda(msg) msg))))

;;(fv '(lambda(a b) a b +))
;;(fv '(let ((foo (lambda(bar) foo + 1 bar))) foo goo))
;;(fv '(let ((foo (lambda(bar) foo + 1 bar))) goo))
;;(fv '(letrec ((foo (lambda(bar) foo + 1 bar))) foo))

