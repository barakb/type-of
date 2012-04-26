#lang racket
(require "language.rkt")
(require "type.rkt")

(require racket/trace)

(provide (except-out (all-defined-out)))



(define type-equations
  (lambda(pt bindings equations c)
    (cond ((number-pt? pt) ...) 
          ((var-pt? pt)  ... )
          ((if-pt? pt) ... )
          ((procedure-pt? pt) ... )
          ((app-pt? pt) ... )
          ((let-pt? pt) ... )
          ((letrec-pt? pt) ...)
          (else '()))))



(define extend-bindings
  (lambda(vars bindings)
    (append (map (lambda(var)
                   (cons var (var-type)))
                 vars)
            bindings)))

(define type-equations-seq 
  (lambda(exps bindings equations c)
    (if (= 1 (length exps))
        (type-equations (car exps) bindings equations 
                        (lambda(tv bindings)
                          (c (list tv) bindings)))
        (type-equations (car exps)
                        bindings 
                        equations 
                        (lambda(tv equations)
                          (type-equations-seq (cdr exps) bindings equations 
                                              (lambda(tv-seq bindings)
                                                (c (cons tv tv-seq) bindings))))))))


(define display-type-equations-of-exp
  (lambda(exp)
    (parse exp (lambda(pt)
                 (type-equations pt '() '() (lambda(type-var type-equations) 
                                              (fprintf (current-output-port)
                                                       "~a:\n  start type-variable: ~a\n  type-equations: ~a\n"
                                                       exp
                                                       (type->human-form  type-var)
                                                       (map (lambda(equation)
                                                              (list (type->human-form (car equation))
                                                                    '=
                                                                    (type->human-form (cdr equation))))
                                                            type-equations))
                                              #t)))
           (lambda() 'parse-fail))))


;;(trace type-equations)
;(display-type-equations-of-exp '(lambda(x y z) 1))
;(display-type-equations-of-exp '(lambda(x y) x y))
;(display-type-equations-of-exp '((lambda(x) x) 1))
;(display-type-equations-of-exp '(letrec ((foo (lambda(f) 
;                                                 (foo f))))
;                                   (foo 1)))
