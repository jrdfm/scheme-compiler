#lang racket
(require "ast.rkt")
(provide lambdas lambdas-ds lambdas-e lambdas-es lambdas-fun)


;; Prog -> [Listof Lam]
;; List all of the lambda expressions in p
(define (lambdas p)
  (match p
    [(Prog ds e)
     (append (lambdas-ds ds) (lambdas-e e))]))

;; Defns -> [Listof Lam]
;; List all of the lambda expressions in ds
(define (lambdas-ds ds)
  (match ds
    ['() '()]
    [(cons (Defn f fun) ds)
     (append (lambdas-fun fun)
             (lambdas-ds ds))]))

;; I think it works
(define (lambdas-fun fn)
 (match fn
  [(FunPlain xs e)  (lambdas-e e)]
  [(FunRest xs x e) (lambdas-e e)]
  [(FunCase cs)     (lambdas-cs cs)]))


(define (lambdas-cs cs)
 (match cs
  ['() '()]
  [(cons fn t) (append (lambdas-fun fn)
                       (lambdas-cs t))]))

(define (lambdas-es es)
  (match es
    ['() '()]
    [(cons h t)
     (append (lambdas-e h)
             (lambdas-es t))]))



;; Expr -> [Listof Lam]
;; List all of the lambda expressions in e
(define (lambdas-e e)
  (match e
    [(Prim1 p e)        (lambdas-e e)]
    [(Prim2 p e1 e2)    (append (lambdas-e e1) (lambdas-e e2))]
    [(Prim3 p e1 e2 e3) (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(PrimN p es)       (lambdas-es es)]
    [(If e1 e2 e3)      (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(Begin e1 e2)      (append (lambdas-e e1) (lambdas-e e2))]
    [(or (Let xs es e) (Let* xs es e))    
                        (append (lambdas-es es) (lambdas-e e))]
    [(App e1 es)        (append (lambdas-e e1) (append-map lambdas-e es))]
    [(Lam f xs e1)      (cons e (lambdas-e e1))]
    [(Match e ps es)    (append (lambdas-e e) (append-map lambdas-e es))]
    [(Values es)        (lambdas-es es)]
    [(LetValues xs e e0) (append (lambdas-e e) (lambdas-e e0))]
    [_                  '()]))
