#lang racket
(require "language.rkt")
(require "ex3-helpers.rkt")
(require "utils.rkt")
(require "free-vars.rkt")
(require racket/set)

(define exp-free-vars
  (lambda(exp)
    (parse exp 
           (lambda(pt)
             (list->seteq (free-vars pt)))
           (lambda()
             (seteq 'this 'is 'a 'uniqe 'set 'because 'of (list 'me))))))


(run-tests
 
 (test (set=? (exp-free-vars 'z) 
              (seteq 'z))) 
 
 (test (set=? (exp-free-vars '(lambda(y) y))
              (seteq))) 
 
 (test (set=? (exp-free-vars  '((lambda(x)(lambda(y)(+ x y))) (+ x y)))
              (seteq '+ 'x 'y))) 
 
 (test (set=? (exp-free-vars '(let((x (+ x 3)))
                                (x y) (x z))) 
              (seteq '+ 'x 'y 'z))) 
 
 (test (set=? (exp-free-vars '(letrec ((foo (lambda(x g)(g foo + x 3))))
                                (foo y) 
                                (x z)))
              (seteq 'x 'y 'z '+)))
 
 
 (test (set=? (exp-free-vars '(if (lambda(x)(+ x 1)) 
                                  (lambda(y)(- y 1)) 
                                  (lambda(*)(* z 1))))
              (seteq '+ '- 'z)))
 
 (test (set=? (exp-free-vars  '((lambda(x)
                                  (lambda(y)
                                    (lambda(t)
                                       (t (+ x y)))))
                                (+ 1 2))) 
              (seteq '+))) 
 
 
 )