#lang racket
(require "utils.rkt")
(require "tree.rkt")

(define sexp->tree
  (lambda(sexp)
         (cond((null? sexp) '())
              ((pair? sexp) (make-tree (car sexp) (map sexp->tree (cdr sexp)))))))

(define tree->sexp
  (lambda(tree)
    (cons (tree->data tree) (map tree->sexp (tree->children tree)))))

(run-tests
 (let((sexp '(1 (5 (6) (7)))))
   (test (tree->sexp (sexp->tree sexp))
       => sexp)
   )
 (test (tree-find-if$ (sexp->tree '(1 (4 (0) (f)) (f))) zero? (lambda(x) x) (lambda() #f))
       => 0)
 (test (tree-find-if$ (sexp->tree '(1 (4 (0) (f)) (f))) (lambda(x) #f) (lambda(x) x) (lambda() #f))
       => #f)
 (test (tree->sexp (tree-map (lambda(n) (+ n 1)) (sexp->tree '(1 (4 (0))))))
       => 
       '(2 (5 (1))))
 )