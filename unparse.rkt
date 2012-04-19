#lang racket
(require "language.rkt")
(require "utils.rkt")
(provide unparse)

(define unparse
  (lambda(pt)
    (cond ((number-pt? pt) (number-pt->value pt)) 
          ((var-pt? pt) (var-pt->name pt))
          ((if-pt? pt) (list 'if (unparse (if-pt->test-pt pt))
                             (unparse (if-pt->do-if-true-pt pt))
                             (unparse (if-pt->do-if-false-pt pt))))
          ((procedure-pt? pt)
           `(lambda ,(procedure->formals pt)
                 ,@(map unparse  (procedure->bodies pt))))
          ((app-pt? pt)
           (cons (unparse (app-pt->rator-pt pt)) 
                 (map unparse (app-pt->rands-pt pt))))
          ((let-pt? pt)
           `(let ((,(let-pt->var pt) ,(unparse (let-pt->expression pt))))
              ,@(map unparse (let-pt->bodies pt))))
          ((letrec-pt? pt)
           `(letrec ((,(letrec-pt->var pt) ,(unparse (letrec-pt->procedure pt))))
              ,@(map unparse (letrec-pt->bodies pt)))))))

(run-tests
 
 (let ((exp '(letrec ((foo (lambda(x) (+ x y))))
               (if foo
                   (let ((x 1))
                     (foo 1))
                   boo))))
   (parse exp
          (lambda(pt)
            (test (equal? (unparse pt) exp)))
          (lambda()
            (test #f))))
 
)
 
 