#lang racket
(require "ast.rkt")
(provide fv)

;; Expr -> [Listof Id]
;; List all of the free variables in e
(define (fv e)
  (remove-duplicates (fv* e)))

(define (fv* e)  
  (match e
    [(Var x)            (list x)]
    [(Prim1 p e)        (fv* e)]
    [(Prim2 p e1 e2)    (append (fv* e1) (fv* e2))]
    [(Prim3 p e1 e2 e3) (append (fv* e1) (fv* e2) (fv* e3))]
    [(PrimN p es)       (append-map fv* es)]
    [(If e1 e2 e3)      (append (fv* e1) (fv* e2) (fv* e3))]
    [(Begin e1 e2)      (append (fv* e1) (fv* e2))]

    [(Let x e1 e2)      (append (fv* e1) (remq* (list x) (fv* e2)))]

    [(or (Let xs es e) 
         (Let* xs es e)) (append (append-map fv* es) (remq* xs (fv* e)))]

    [(Values es)         (append-map fv* es)]
    [(LetValues xs e e0) (append (fv* e) (remq* xs (fv* e0)))]

    [(App e1 es)        (append (fv* e1) (append-map fv* es))]
    [(Apply f es e)     (append (append-map fv* es) (fv* e))]
    [(Lam f xs e)       (remq* xs (fv* e))]
    [(Match e ps es)    (append (fv* e) (append-map fv-clause* ps es))]

    [(Defn f fun)      (match fun
                        [(FunPlain xs e)  (remq* xs (fv* e))]
                        [(FunRest xs x e) (remq* (cons x xs) (fv* e))]
                        [(FunCase cs) (fv-cs cs)])]
                        

    [_                  '()]))



(define (fv-cs cs)
 (match cs
  ['() '()]
  [(cons (FunPlain xs e) t) (append  (remq* xs (fv* e)) (fv-cs t))]
  [(cons (FunRest xs x e) t) (append (remq* (cons x xs) (fv* e)) (fv-cs t))]))

;; Pat Expr -> [Listof Id]
(define (fv-clause* p e)
  (remq* (bv-pat* p) (fv* e)))

;; Pat -> [Listof Id]
(define (bv-pat* p)
  (match p
    [(PVar x) (list x)]
    [(PCons p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PAnd p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PBox p) (bv-pat* p)]
    [_ '()]))
