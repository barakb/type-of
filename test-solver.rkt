#lang racket
(require "language.rkt")
(require "type.rkt")
(require "type-equations.rkt")
(require (rename-in "solver.rkt" 
                    (solve solve:solve))
         )
(require "free-vars.rkt")
(require "utils.rkt")



(define solve
  (lambda(exp)
    (parse exp (lambda(pt)
                 (if (null? (free-vars2 pt (map car init-data)))
                     (type-equations pt init-env init-equations
                                     (lambda(type-var type-equations) 
                                       (format-type-equations type-var type-equations)
                                       (solve:solve type-equations '() (lambda(subs)
                                                                         (type->human-form 
                                                                          ;;(cdr (assoc type-var subs))))
                                                                          (normalize-type (cdr (assoc type-var subs)))))
                                                    (lambda()
                                                      'subs-fail))))
                     'free-var))
           (lambda() 'parse-fail))))



(run-tests
 
 (test  (solve '(lambda(f g)
                  (lambda(n)
                    (f (g n)))))
        => 
        '((T0 -> T1) * (T2 -> T0) -> (T2 -> T1)))
 
 (test (solve '(letrec ((fact (lambda(n)
                                (if (zero? n)
                                    1
                                    (* n (fact (- n 1)))))))
                 fact))
       => 
       '(NUMBER -> NUMBER))
 
 (test (solve '(lambda(x) (+ x (x 1))))
       => 
       'subs-fail)
 
 (test (solve '(lambda(f)
                 (lambda(x)
                   (- (f 3) (f x)))))
       => 
       '((NUMBER -> NUMBER) -> (NUMBER -> NUMBER)))
 
 (test (solve '(lambda(f)(f 11)))
       => 
       '((NUMBER -> T0) -> T0))
 
 (test (solve '(lambda(x)
                 (if x 1 (+ x 2))))
       => 
       'subs-fail)
 
 (test (solve '((lambda(x) (x x)) (lambda(x) (x x))))
       => 
       'subs-fail)
 
 
 (test (solve '(lambda(x) (x x)))
       => 
       'subs-fail)
 
 (test (solve '(lambda(x) +))
       => 
       '(T0 -> (NUMBER * NUMBER -> NUMBER)))
 
 (test (solve '(lambda(+) +))
       => 
       '(T0 -> T0))
 
 (test (solve '(lambda(+) (- + 1)))
       => 
       '(NUMBER -> NUMBER))
 
 
 )

