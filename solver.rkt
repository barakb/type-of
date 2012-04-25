#lang racket

(require "language.rkt")
(require "type.rkt")
(require "type-equations.rkt")


;; (require racket/trace)

(provide (except-out (all-defined-out)
                     ))

(define apply-one-subst
  (lambda(ty0 tvar ty1)
    (cond((number-type? ty0) ty0)
         ((boolean-type? ty0) ty0)
         ((void-type? ty0) ty0)
         ((procedure-type? ty0)
          (procedure-type (apply-one-subst (procedure-type->arg-type ty0) tvar ty1)
                          (apply-one-subst (procedure-type->result-type ty0) tvar ty1)))
         ((tuple-type? ty0) 
          (tuple-type (map (lambda(ty0)
                             (apply-one-subst ty0 tvar ty1))
                           (tuple-type->types ty0))))
         ((var-type? ty0) (if (equal? ty0 tvar)
                               ty1
                               ty0))
         )))

(define apply-subst-to-type
  (lambda(ty subst)
    (cond((number-type? ty) ty)
         ((boolean-type? ty) ty)
         ((void-type? ty) ty)
         ((procedure-type? ty)
          (procedure-type (apply-subst-to-type (procedure-type->arg-type ty) subst)
                          (apply-subst-to-type (procedure-type->result-type ty) subst)))
         ((tuple-type? ty) (tuple-type (map (lambda(ty)
                                              (apply-subst-to-type ty subst))
                                            (tuple-type->types ty))))
         ((var-type? ty)
          (let((tmp (assoc ty subst)))
            (if tmp 
                (cdr tmp)
                ty)))
         )))

(define extends-subst
  (lambda(subst tvar ty)
    (cons 
     (cons tvar ty)
     (map (lambda(p)
            (let ((oldlhs (car p))
                  (oldrhs (cdr p)))
              (cons 
               oldlhs
               (apply-one-subst oldrhs tvar ty))))
          subst))))

(define no-occurrence?
  (lambda(tvar ty)
    (cond((number-type? ty) #t)
         ((boolean-type? ty) #t)
         ((void-type? ty) #t)
         ((procedure-type? ty)
          (and (no-occurrence? tvar (procedure-type->arg-type ty))
               (no-occurrence? tvar (procedure-type->result-type ty))))
         ((tuple-type? ty) (andmap (lambda(ty)
                                     (no-occurrence? tvar ty))
                                   (tuple-type->types ty)))
         ((var-type? ty) (not (equal? tvar ty)))
         )))

(define solve-once 
  (lambda(ty1 ty2 subst success fail)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond ((equal? ty1 ty2) (success subst))
            ((var-type? ty1)
             (if(no-occurrence? ty1 ty2)
                (success (extends-subst subst ty1 ty2))
                (fail)))
            ((var-type? ty2)
             (if(no-occurrence? ty2 ty1)
                (success (extends-subst subst ty2 ty1))
                (fail)))
            ((and (tuple-type? ty1) (tuple-type? ty2))
             (solve-list (tuple-type->types ty1) (tuple-type->types ty2) subst success fail))
            ((and (procedure-type? ty1) (procedure-type? ty2))
             (solve-once (procedure-type->arg-type ty1) (procedure-type->arg-type ty2) 
                         subst
                         (lambda(subst)
                           (solve-once (procedure-type->result-type ty1) (procedure-type->result-type ty2)
                                       subst
                                       success
                                       fail))
                         fail))
            (else (fail))))))

(define solve-list
  (lambda(tys1 tys2 subst success fail)           
    (cond ((and (null? tys1) (null? tys2)) (success subst))
          ((and (not (null? tys1)) (not (null? tys2)))
           (solve-once (car tys1) (car tys2) subst 
                       (lambda(subst)
                         (solve-list (cdr tys1) (cdr tys2)
                                     subst 
                                     success
                                     fail))
                       fail))
          (else (fail)))))

(define solve 
  (lambda(equations subst success fail)
    (cond ((null? equations) (success subst))
          (else (solve-once (car (car equations))
                            (cdr (car equations))
                            subst
                            (lambda(subst)
                              (solve (cdr equations) subst success fail))
                            fail)))))

(define init-data
  (list
   (list '+ (var-type) (procedure-type (tuple-type (list (number-type) (number-type))) (number-type)))
   (list '* (var-type) (procedure-type (tuple-type (list (number-type) (number-type))) (number-type)))
   (list '- (var-type) (procedure-type (tuple-type (list (number-type) (number-type))) (number-type)))
   (list '= (var-type) (procedure-type (tuple-type (list (number-type) (number-type))) (boolean-type)))
   (list 'zero? (var-type) (procedure-type (tuple-type (list (number-type))) (boolean-type)))
   ))

(define init-env
  (map (lambda(d)
         (cons (car d)
               (cadr d)))
       init-data))

(define init-equations
  (map (lambda(d)
         (cons (cadr d)
               (caddr d)))
       init-data))

(define format-type-equations
  (lambda(type-var type-equations)
    (fprintf (current-output-port)
             "start type-variable: ~a \n"
             (type->human-form type-var))
    (map (lambda(equation)
           (fprintf (current-output-port)
                    "~a = ~a \n"
                    (type->human-form (car equation))
                    (type->human-form (cdr equation))
                    ))
         type-equations)
    type-equations))


(define u
  (lambda(exp)
    (parse exp (lambda(pt)
                 (type-equations pt init-env init-equations
                                 (lambda(type-var type-equations) 
                                   (format-type-equations type-var type-equations)
                                   (solve type-equations '() (lambda(subs)
                                                               (type->human-form (cdr (assoc type-var subs))))
                                          (lambda()
                                            'subs-fail)))))
           
           (lambda() 'parse-fail))))

;;(trace apply-one-subst)

;(u '(lambda(f g)
;      (lambda(n)
;        (f (g n)))))

;;(u '(letrec ((fact (lambda(n)
;;               (if (zero? n)
;;                   1
;;                   (* n (fact (- n 1)))))))
;;      fact))

;; (u '(lambda(x) (+ x (x 1))))
;;(u '(lambda(f)
;;      (lambda(x)
;;        (- (f 3) (f x)))))

;;(u '(lambda(f)(f 11)))

;; x can not be both boolean and number.
;;(u '(lambda(x)
;;      (if x 1 (+ x 2))))

;; (u '(lambda(f) (zero? (f f))))
;; (u '((lambda(x) (x x)) (lambda(x) (x x))))
;;(u '(lambda(x) (x x)))
;;(u '+)
;;(u '(lambda(x) +))
;;(u '(lambda(+) +))
;;(u '(lambda(+) (- + 1)))