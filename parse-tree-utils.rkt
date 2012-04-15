#lang racket

(provide (except-out (all-defined-out)
                     make-pt-ref))

(define tag
  (lambda(name . rest)
    (list->vector (cons name rest))))

(define tagged?
  (lambda(name pt)
    (and (vector? pt)
         (< 0 (vector-length pt))
         (eq? name (vector-ref pt 0)))))

(define pt->tag
  (lambda(pt)
    (vector-ref pt 0)))

(define pt-ref
  (lambda(pt n)
    (vector-ref pt (+ 1 n))))

(define make-pt-pred
  (lambda(tag)
    (lambda(pt)
      (tagged? tag pt))))

;; module private begin
(define make-pt-ref
  (lambda(n)
    (lambda(pt)
      (pt-ref pt n))))
;; module private end

(define pt-take-first (make-pt-ref 0))  

(define pt-take-second (make-pt-ref 1))

(define pt-take-third (make-pt-ref 2))

(define pt-take-forth (make-pt-ref 3))

