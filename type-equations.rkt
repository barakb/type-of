#lang racket
(require "language.rkt")
(require "type.rkt")

(require racket/trace)

(provide (except-out (all-defined-out)
                     ))

(define type-equations
  (lambda(pt bindings equations c)
    ;;(display `(bindings ,bindings pt ,pt)) (newline)
    (cond ((number-pt? pt) 
           (let ((tv (var-type)))
             (c tv (cons (cons tv (number-type)) equations))))
          ((var-pt? pt) (c (cdr (assoc (var-pt->name pt) bindings)) equations)) 
          ((if-pt? pt) (type-equations  (if-pt->test-pt pt)
                                        bindings
                                        equations
                                        (lambda(test-tv equations)
                                          (type-equations (if-pt->do-if-true-pt pt)
                                                          bindings
                                                          equations
                                                          (lambda(do-if-true-tv equations)
                                                            (type-equations (if-pt->do-if-false-pt pt)
                                                                            bindings
                                                                            equations
                                                                            (lambda(do-if-false-tv equations)
                                                                              (c do-if-false-tv (cons (cons test-tv (boolean-type))
                                                                                                      (cons (cons do-if-true-tv do-if-false-tv) equations))))))))))
          ((procedure-pt? pt)
           (let ((tv (var-type))
                 (body-bindings  (extend-bindings (procedure->formals pt) bindings)))
             (type-equations-seq (procedure->bodies pt)
                                 body-bindings
                                 equations 
                                 (lambda(bodies-tvs equations)
                                   (c tv (cons (cons tv (procedure-type (if (null? (procedure->formals pt))
                                                                            (void-type)
                                                                            (tuple-type (map (lambda(formal) 
                                                                                               (cdr (assoc formal body-bindings)))
                                                                                             (procedure->formals pt))))
                                                                        (last bodies-tvs))) 
                                               equations))))))
          ((app-pt? pt)
           (type-equations (app-pt->rator-pt pt)
                           bindings 
                           equations
                           (lambda(rator-tv equations)
                             (type-equations-seq (app-pt->rands-pt pt)
                                                 bindings
                                                 equations
                                                 (lambda(tvs equations)
                                                   (let ((proc-result-tv (var-type)))
                                                     (c proc-result-tv (cons (cons rator-tv (procedure-type (if (null? tvs)
                                                                                                                (void-type)
                                                                                                                (tuple-type tvs))
                                                                                                            proc-result-tv))
                                                                             equations))))))))
          
          
          
          
          ((let-pt? pt)
           (type-equations (let-pt->expression pt) 
                           bindings 
                           equations
                           (lambda(exp-tv equations)
                             (type-equations-seq  (let-pt->bodies pt)
                                                  (cons (cons (let-pt->var pt) exp-tv) bindings)
                                                  equations
                                                  (lambda(bodies-tvs equations)
                                                    (c (last bodies-tvs) equations)))))) 
          ((letrec-pt? pt)
           (let((procedure-tv (var-type)))
             (type-equations (letrec-pt->procedure pt) 
                             (cons (cons (letrec-pt->var pt) procedure-tv) bindings) 
                             equations
                             (lambda(proc-tv equations)
                               (type-equations-seq  (letrec-pt->bodies pt)
                                                    (cons (cons (letrec-pt->var pt) proc-tv) bindings)
                                                    (cons (cons procedure-tv proc-tv) equations)
                                                    (lambda(bodies-tvs equations)
                                                      (c (last bodies-tvs) equations))))))) 
          ;          ((letrec-pt? pt) (append (free-vars (letrec-pt->expression pt) (cons (letrec-pt->var pt) bindings))
          ;                                   (flatmap (lambda(body-pt)
          ;                                              (free-vars body-pt 
          ;                                                         (cons (letrec-pt->var pt) bindings)))
          ;                                            (letrec-pt->bodies pt))))
          
          (else '()))))



;;var = foo
;;expression = lambda
;;bodies = (foo 1 2)
;;(letrec ((foo (lambda(a b)
;;                (+ a b))))
;;  (foo 1 2))

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


(define foo
  (lambda(exp)
    (parse exp (lambda(pt)
                 (type-equations pt '() '() (lambda(type-var type-equations) 
                                              (fprintf (current-output-port)
                                                       "start type-variable: ~a \ntype-equations: ~a\n"
                                                       type-var
                                                       type-equations)
                                              #t)))
           (lambda() 'parse-fail))))


;;(trace type-equations)
;;(foo '(lambda(x y z) 1))
;;(foo '(lambda(x y) x y))
;;(foo '((lambda(x) x) 1))
;;(foo '(letrec ((foo (lambda(f) 
;;                      (foo f))))
;;        (foo 1)))
