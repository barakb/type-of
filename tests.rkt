#lang racket
(require "language.rkt")
(require "type.rkt")
(require "type-equations.rkt")
(require (rename-in "unifier.rkt" 
                    (unify unify:unify))
                    )
(require "free-vars.rkt")
(require "utils.rkt")



(define unify
  (lambda(exp)
    (parse exp (lambda(pt)
                 (if (null? (free-vars pt (map car init-data)))
                     (type-equations pt init-env init-equations
                                     (lambda(type-var type-equations) 
                                       (format-type-equations type-var type-equations)
                                       (unify:unify type-equations '() (lambda(subs)
                                                                   (type->human-form 
                                                                    (normalize-type (cdr (assoc type-var subs)))))
                                              (lambda()
                                                'subs-fail))))
                     'free-var))
           (lambda() 'parse-fail))))



(run-tests
 
 (test  (unify '(lambda(f g)
                      (lambda(n)
                        (f (g n)))))
        => 
        '((T0 -> T1) * (T2 -> T0) -> (T2 -> T1)))
 
 (test (unify '(letrec ((fact (lambda(n)
                                    (if (zero? n)
                                        1
                                        (* n (fact (- n 1)))))))
                     fact))
       => 
       '(NUMBER -> NUMBER))
 
 (test (unify '(lambda(x) (+ x (x 1))))
       => 
       'subs-fail)
 
 (test (unify '(lambda(f)
                     (lambda(x)
                       (- (f 3) (f x)))))
       => 
       '((NUMBER -> NUMBER) -> (NUMBER -> NUMBER)))
 
 (test (unify '(lambda(f)(f 11)))
       => 
       '((NUMBER -> T0) -> T0))
 
 (test (unify '(lambda(x)
                     (if x 1 (+ x 2))))
       => 
       'subs-fail)
 
 (test (unify '((lambda(x) (x x)) (lambda(x) (x x))))
       => 
       'subs-fail)
 
 
 (test (unify '(lambda(x) (x x)))
       => 
       'subs-fail)
 
 (test (unify '(lambda(x) +))
       => 
       '(T0 -> (NUMBER * NUMBER -> NUMBER)))
 
 (test (unify '(lambda(+) +))
       => 
       '(T0 -> T0))
 
 (test (unify '(lambda(+) (- + 1)))
       => 
       '(NUMBER -> NUMBER))
 
 )