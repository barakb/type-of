#lang racket
(require "language.rkt")
(require "ex3-helpers.rkt")

(provide free-vars)


(define free-vars
  (lambda(pt)
    (free-vars2 pt '())))

(define free-vars2
  (lambda(pt bindings)
    (cond ((number-pt? pt) '())
          ((var-pt? pt) (if (memq (var-pt->name pt) bindings)
                            '()
                            (list (var-pt->name pt))))
          ((if-pt? pt) (append (free-vars2 (if-pt->test-pt pt) bindings)
                               (free-vars2 (if-pt->do-if-true-pt pt) bindings)
                               (free-vars2 (if-pt->do-if-false-pt pt) bindings)))
          ((app-pt? pt) (append (free-vars2 (app-pt->rator-pt pt) bindings)
                                (flatmap (lambda(pt)
                                           (free-vars2 pt bindings))
                                         (app-pt->rands-pt pt))))
          ((procedure-pt? pt) (flatmap (lambda(body-pt)
                                         (free-vars2 body-pt 
                                                    (append (procedure->formals pt) bindings)))
                                       (procedure->bodies pt)))
          ((let-pt? pt) (append (free-vars2 (let-pt->expression pt) bindings)
                                (flatmap (lambda(body-pt)
                                           (free-vars2 body-pt 
                                                      (cons (let-pt->var pt) bindings)))
                                         (let-pt->bodies pt))))
          ((letrec-pt? pt) (append (free-vars2 (letrec-pt->procedure pt) (cons (letrec-pt->var pt) bindings))
                                   (flatmap (lambda(body-pt)
                                              (free-vars2 body-pt 
                                                         (cons (letrec-pt->var pt) bindings)))
                                            (letrec-pt->bodies pt))))
          
          (else '()))))





