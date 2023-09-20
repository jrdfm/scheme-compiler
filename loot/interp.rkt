#lang racket
(provide interp interp-env)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)
;; | (Value ... -> Answer)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))

;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (interp-var x r ds)]
    [(Str s)  (string-copy s)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (match (interp-env e3 r ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]

    [(PrimN p es)
     (match (interp-env* es r ds)
       ['err 'err]
       [v (interp-primN p v)])]


    [(If p e1 e2)
     (match (interp-env p r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [_    (interp-env e2 r ds)])]
    [(Let xs es e)
     (match (interp-env* es r ds)
       ['err 'err]
       [v (interp-env e (append (reverse (map list xs v)) r) ds)])]
    [(Let* xs es e) (interp-let* xs es e r ds)]
    [(Cond cs e)
     (interp-cond cs e r ds)]
    [(Case e cs el)
     (interp-case e cs el r ds)]

    [(Lam _ xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r) ds)
           'err))]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (if (procedure? f)
               (apply f vs)
               'err)])])]
    [(Apply f es e)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (interp-env e r ds)
          ['err 'err]
          [ws
           (if (list? ws)
               (match (interp-var f r ds)
                 ['err 'err]
                 [f (if (procedure? f) (apply f (append vs ws)) 'err)])
               'err)])])]
    [(Match e ps es)
     (match (interp-env e r ds)
       ['err 'err]
       [v
        (interp-match v ps es r ds)])]
    [(Values es) (apply values (interp-env* es r ds))]
    [(LetValues xs e e0)
     (let ((i (call-with-values (λ () (interp-env e r ds)) list)))
       (interp-env e0 (append (map list xs i) r) ds))]))


(define (interp-cond cs e r ds)
  (match cs
    ['() (interp-env e r ds)]
    [(list (Clause ep ea) t ...)
     (match (interp-env ep r ds)
       ['err 'err]
       [v (if v
              (interp-env ea r ds)
              (interp-cond t e r ds))])]))

;; General Let
(define (interp-case e cs el r ds)
  (match cs
    ['() (interp-env el r ds)]
    [(list (Clause l ea) t ...)
     (if (member (interp-env e r ds) l)
         (interp-env ea r ds)
         (interp-case e t el r ds))]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (interp-env e r ds)])]))

;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(PWild) r]
    [(PVar x) (ext r x v)]
    [(PLit l) (and (eqv? l v) r)]
    [(PBox p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(PCons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(PAnd p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f fun)
             (interp-fn f fun r ds)]
            [#f 'err])]
    [v v]))

(define (interp-fn f fn r ds)
  (match fn
    [(FunPlain xs e) (interp-env (Lam f xs e) '() ds)]
    [(FunRest xs x e)
     (λ vs
       (if (< (length vs) (length xs))
           'err
           (interp-env e
                       (zip (cons x xs)
                            (cons (drop vs (length xs))
                                  (take vs (length xs)))) ds)))]
    [(FunCase cs) (λ vs (match (select-case-lambda cs (length vs))
                          ['err 'err]
                          [fn (apply (interp-fn f fn r ds) vs)]))]))

;; [Listof FunCaseClause] Nat -> Fun | 'err
(define (select-case-lambda cs n)
  (match cs
    ['() 'err]
    [(cons (and (FunPlain xs e) f) cs)
     (if (= (length xs) n)
         f
         (select-case-lambda cs n))]
    [(cons (and (FunRest xs x e) f) cs)
     (if (<= (length xs) n)
         f
         (select-case-lambda cs n))]))

;; Back referencing Let
(define (interp-let* xs es e r ds)
  (define ls (map list xs es))
  (match (interp* ls r ds)
    ['err 'err]
    [r* (interp-env e r* ds)]))

;; interp*-env's cousin, returns extended environment
;; [Listof [Listof Id Expr]] Env -> Env
(define (interp* es r ds)
  (match es
    ['() r]
    [(cons (cons x (cons e '())) es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp* es (ext r x v) ds)])]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (match (interp-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Defns Symbol -> [Maybe Defn]
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ ) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
