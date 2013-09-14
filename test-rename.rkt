#lang racket
(require "ex3-helpers.rkt")
(require "language.rkt")
(require (rename-in "rename.rkt" (rename rename:rename)))
(require "utils.rkt")

(define rename
  (lambda(exp)
    (parse exp
           (lambda(pt)
             (rename:rename pt))
           (lambda()
             #f))))


(define rename?
  (lambda(pt1 pt2)
    (rename3? pt1 pt2 '())))

(define rename3?
  (lambda(pt1 pt2 bindings)
    (cond ((and (number-pt? pt1) (number-pt? pt2)) #t)
          ((and (var-pt? pt1) (var-pt? pt2)
                (let ((entry (assoc (var-pt->name pt1) bindings)))
                  (if entry
                      (and (eq? (cdr entry) (var-pt->name pt2)) (not (eq? (var-pt->name pt1) (var-pt->name pt2))))
                      (eq? (var-pt? pt1) (var-pt? pt2))))))
          ((and (if-pt? pt1) (if-pt? pt2)) 
           (and 
            (rename3? (if-pt->test-pt pt1) (if-pt->test-pt pt2) bindings)
            (rename3? (if-pt->do-if-true-pt pt1) (if-pt->do-if-true-pt pt2) bindings)
            (rename3? (if-pt->do-if-false-pt pt1) (if-pt->do-if-false-pt pt2) bindings)))
          ((and (app-pt? pt1) (app-pt? pt2) (= (length (app-pt->rands-pt pt1)) (length (app-pt->rands-pt pt2))))
           (andmap (lambda(pt1 pt2)
                     (rename3? pt1 pt2 bindings))
                   (cons (app-pt->rator-pt pt1) (app-pt->rands-pt pt1))
                   (cons (app-pt->rator-pt pt2) (app-pt->rands-pt pt2))
                   ))
          ((and (procedure-pt? pt1) (procedure-pt? pt1) (= (length (procedure->formals pt1)) (length (procedure->formals pt2))))
           (let ((extendend-bindings (append (map cons (procedure->formals pt1) (procedure->formals pt2))
                                             bindings)))
             (andmap (lambda(pt1 pt2)
                       (rename3? pt1 pt2 extendend-bindings))
                     (procedure->bodies pt1)
                     (procedure->bodies pt2))))
          ((and (let-pt? pt1) (let-pt? pt2) (= (length (let-pt->bodies pt1)) (length (let-pt->bodies pt2))))
           (and (rename3? (let-pt->expression pt1) (let-pt->expression pt2) bindings)
                (andmap (lambda(pt1 pt2)
                          (rename3? pt1 pt2 (cons (cons (let-pt->var pt1) (let-pt->var pt2))
                                                  bindings)))
                        (let-pt->bodies pt1)  (let-pt->bodies pt2))))        
          ((and (letrec-pt? pt1) (letrec-pt? pt2) 
                (= (length (letrec-pt->bodies pt1)) (length (letrec-pt->bodies pt2))))
           (and (rename3? (letrec-pt->procedure pt1) (letrec-pt->procedure pt2)
                          (cons (cons (letrec-pt->var pt1) (letrec-pt->var pt2))
                                bindings))
                (andmap (lambda(pt1 pt2)
                          (rename3? pt1 pt2 (cons (cons (letrec-pt->var pt1) (letrec-pt->var pt2))
                                                  bindings)))
                        (letrec-pt->bodies pt1)  (letrec-pt->bodies pt2))))
          (else #f))))

(define check-rename3?
  (lambda(exp)
    (parse exp
           (lambda(pt)
             (rename? pt (rename:rename pt)))
           (lambda()
             'parse-failed))))



(run-tests
 (test  (check-rename3?  '+))
 (test  (check-rename3?  '(lambda(x) 1 x)))
 (test  (check-rename3?  '((lambda(r)(lambda(y)( + r y))) (+ x y))))
 (test  (check-rename3?  '(let((r (+ r 3)))
                            (x y) (x z))))
 (test  (check-rename3?  '(letrec ((foo (lambda(x y)(foo + x 3 y))))
                   (foo y) (x z))))
 (test  (check-rename3?  '(if (lambda(x y z)(+ x 1)) (lambda(y x z)(+ y z x 1)) (lambda(z t)(+ t z 1)))))
 (test  (check-rename3?  '((lambda(z)(lambda(y)( + z y))) (+ z y))))
 (test  (check-rename3?  '(lambda(x)(lambda(y)(x y)))))
 (test  (check-rename3?  '(lambda(x y)(letrec ((foo (lambda(y)(foo z x y))))
                                        foo))))
 (test  (check-rename3?  '(if test 3 (lambda(z)(lambda(y)(z y))))))
 
 )
