#lang racket
(provide (all-defined-out))

;; ADT type [T1 * List(Tree) -> Tree]


(define make-tree
  (lambda(data children) ... )
 



;; ADT type  [Tree -> T1]


(define tree->data
  (lambda(tree) ... )



;; ADT type  [Tree -> List(Tree)]


(define tree->children
  (lambda(tree) ... )



(define tree-find-if$
  (lambda(tree pred? success fail) ... )
  

(define tree-map
  (lambda(f tree) ...)




