#lang racket
(provide (all-defined-out))

;; ADT type [T1 * List(Tree) -> Tree]
;; Impl type [T1 * List -> [[T1 * List -> T2] -> T2]]
(define make-tree
  (lambda(data children)
    (lambda(selector)
      (selector data children))))


;; Signature tree->data(tree)
;; ADT type  [Tree -> T1]
;; Impl type [[[T1 * List -> T2] -> T2]] -> T2]
(define tree->data
  (lambda(tree)
    (tree (lambda(data children) data))))

;; Signature tree->children(node)
;; ADT type  [Tree -> List(Tree)]
;; Impl type [[[T1 * List -> T2] -> T2]] -> T2]
(define tree->children
  (lambda(tree)
    (tree (lambda(data children) children))))


(define tree-find-if$
  (lambda(tree pred? success fail)
    (if (pred? (tree->data tree))
        (success (tree->data tree))
        (trees-find-if$ (tree->children tree) pred? success fail))))

(define trees-find-if$       
  (lambda(trees pred? success fail)
    (if(null? trees)
       (fail)
       (tree-find-if$ (car trees)
                      pred?
                      success
                      (lambda()
                        (trees-find-if$ (cdr trees)
                                        pred?
                                        success
                                        fail))))))

(define tree-map
  (lambda(f tree)
    (make-tree (f (tree->data tree))
               (map (lambda(tree)
                      (tree-map f tree))
                    (tree->children tree)))))




