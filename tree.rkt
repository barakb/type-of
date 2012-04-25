#lang racket

;; ADT type [unit -> Tree]
;; Impl type [T1 * List -> [[T1 * List -> T2] -> T2]]
(define make-tree
  (lambda(data children)
    (lambda(selector)
      (selector data children))))


;; Signature get-data(tree)
;; ADT type  [Tree -> T1]
(((T0 * T1 -> T0) -> T2) -> T2)
;; Impl type [[[T1 * List -> T2] -> T2]] -> T2]
(define get-data 
  (lambda(tree)
    (tree (lambda(data children) data))))

;; Signature get-children(node)
;; ADT type  [Tree -> T1]
;; Impl type [[[T1 * List -> T2] -> T2]] -> T2]
(define get-children
  (lambda(tree)
    (tree (lambda(data children) children))))


(define tree-find-if
  (lambda(tree pred? success fail)
    (if (pred? (get-data tree))
        (success (get-data tree))
        (trees-find-if (get-children tree) pred? success fail))))

(define trees-find-if       
  (lambda(trees pred? success fail)
    (if(null? trees)
       (fail)
       (tree-find-if (car trees)
                     pred?
                     success
                     (lambda()
                       (trees-find-if (cdr trees)
                                      pred?
                                      success
                                      fail))))))

(define tree-map
  (lambda(f tree)
    (make-tree (f (tree-data tree))
               (map (lambda(tree)
                      (tree-map f tree))
                    (tree-children)))))


