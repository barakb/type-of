#lang racket
(require "language.rkt")
(provide (rename-out (rename1 rename)))

(define fresh-name
  (let((counter 0))
    (lambda name
      (if(null? name)
         (fresh-name 'var)
         (let((new-name (string->symbol (string-append (symbol->string (car name)) ":" (number->string counter)))))
           (set! counter (+ 1 counter))
           new-name)))))

       
(define rename1
  (lambda(pt)
    (rename2 pt '())))

(define rename2
  (lambda(pt bindings)
    (cond ((number-pt? pt) pt)
          ((var-pt? pt) (let ((entry (assoc (var-pt->name pt) bindings)))
                          (var-pt (if entry (cdr entry) (var-pt->name pt)))))
          ((if-pt? pt) (if-pt (rename2 (if-pt->test-pt pt) bindings)
                              (rename2 (if-pt->do-if-true-pt pt) bindings)
                              (rename2 (if-pt->do-if-false-pt pt) bindings)))
          ((app-pt? pt) (app-pt (rename2 (app-pt->rator-pt pt) bindings)
                                (map (lambda(pt)
                                       (rename2 pt bindings))
                                     (app-pt->rands-pt pt))))
          ((procedure-pt? pt) (let ((new-names (map fresh-name (procedure->formals pt))))
                                (procedure-pt new-names
                                              (map (lambda(body-pt)
                                                     (rename2 body-pt 
                                                             (append (map cons (procedure->formals pt) new-names) bindings)))
                                                   (procedure->bodies pt)))))
          ((let-pt? pt) 
           (let ((new-name (fresh-name (let-pt->var pt))))
             (let-pt new-name (rename2 (let-pt->expression pt) bindings)
                     (map (lambda(body-pt)
                            (rename2 body-pt
                                    (cons (cons (let-pt->var pt) new-name) bindings)))
                          (let-pt->bodies pt)))))
          ((letrec-pt? pt) 
           (let ((new-name (fresh-name (letrec-pt->var pt))))
             (letrec-pt new-name (rename2 (letrec-pt->procedure pt) (cons (cons (letrec-pt->var pt) new-name) bindings))
                        (map (lambda(body-pt)
                               (rename2 body-pt
                                       (cons (cons (letrec-pt->var pt) new-name) bindings)))
                             (letrec-pt->bodies pt)))))
          )))          

 ;;(require racket/trace)
 ;;(trace rename2)
  

