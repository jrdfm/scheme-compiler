#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "lambdas.rkt" "fv.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r9 'r9) ;
(define r8  'r8)  ; scratch
(define r11  'r11); # result arity
(define r10 'r10) ; # fun args

;; type CEnv = [Listof Id]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-defines-values ds)
           (compile-e e (reverse (define-ids ds)) #t)
           (Add rsp (* 8 (length ds))) ;; pop function definitions
           (Ret)
           (compile-defines ds)
           (compile-defines (lambdas p))
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f fun) ds)
     (cons f (define-ids ds))]))

;; [Listof Lam] -> Asm
(define (compile-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-define l)
          (compile-defines ls))]))

(define (compile-define l)
  (let ((fvs (fv l)))
    (match l
      [(Lam f xs e)
       (let ((env  (append (reverse fvs) (reverse xs) (list #f))))
         (seq (Label (symbol->label f))
              (Cmp r10 (length xs))       ;; check arity
              (Jne 'raise_error_align)
              (Mov rax (Offset rsp (* 8 (length xs))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)
              (compile-e e env #t)
              (Add rsp (* 8 (length env))) ; pop env
              (Ret)))]
      [(Defn f (FunPlain xs e)) (compile-define (Lam f xs e))]
      [(Defn f (FunRest xs x e))
       (let ((env (append (reverse fvs) (cons x (reverse xs)) (list #f)))
             (loop (gensym 'loop))
             (eq (gensym 'eq)))
         (seq  (Label (symbol->label f))
               (Sub r10 (length xs))
               (Cmp r10 0)
               (Jl 'raise_error_align)
               (Mov rax val-empty)
               (Je eq)       ;; no extra args
               (Label loop)  ;; loop to make a list
               (Sub r10 1)
               (Mov (Offset rbx 0) rax)
               (Pop rax)
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-cons)
               (Add rbx 16)
               (Cmp r10 0)
               (Jg loop)
               (Label eq)
               (Push rax) ;; push list
               (Mov rax (Offset rsp (* 8 (add1 (length xs))))) ;; get fun addr from stack below xs & x to fetch fvs 
               (Xor rax type-proc)
               (copy-env-to-stack fvs 8)
               (compile-e e env #f)
               (Add rsp (* 8 (length env))) ;; pop args + list
               (Ret)))]
      [(Defn f (FunCase cs))
       (seq (Label (symbol->label f))
            (compile-fun-case cs)
            (Ret))])))

(define (compile-fun-case cs)
  (match cs
    ['() (seq (Jmp 'raise_error_align))]
    [(cons (FunPlain xs e) t)
     (let ((l (gensym 'l))
           (en (gensym 'en) ))
       (seq (Cmp r10 (length xs))
            (Jne en)        ;; check arity
            (compile-define (Defn l (FunPlain xs e)))
            (Label en)
            (compile-fun-case t )))]
    [(cons (FunRest xs x e) t)
     (let ((l (gensym 'l))
           (en (gensym 'en)))
       (seq (Cmp r10 (length xs))
            (Jl en)         ;; less is not more
            (compile-define (Defn l (FunRest xs x e)))
            (Label en)
            (compile-fun-case t )))]))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))

;; Expr CEnv Bool -> Asm
(define (compile-e e c t?)
  (match e
    [(Int i)             (compile-value i)]
    [(Bool b)            (compile-value b)]
    [(Char c)            (compile-value c)]
    [(Eof)               (compile-value eof)]
    [(Empty)             (compile-value '())]
    [(Var x)             (compile-variable x c)]
    [(Str s)             (compile-string s)]
    [(Prim0 p)           (compile-prim0 p c)]
    [(Prim1 p e)         (compile-prim1 p e c)]
    [(Prim2 p e1 e2)     (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3)  (compile-prim3 p e1 e2 e3 c)]
    [(PrimN p es)        (compile-primn p es c)]
    [(If e1 e2 e3)       (compile-if e1 e2 e3 c t?)]
    [(Begin e1 e2)       (compile-begin e1 e2 c t?)]
    [(Let xs es e)       (compile-let xs es e c)]
    [(Let* xs es e)      (compile-let* xs es e c)]
    [(Cond cs e)         (compile-cond cs e c)]
    [(Case e cs el)      (compile-case e cs el c)]
    [(App e es)          (compile-app e es c t?)]
    [(Apply f es e)      (compile-apply f es e c)]
    [(Lam f xs e)        (compile-lam f xs e c)]
    [(Match e ps es)     (compile-match e ps es c t?)]
    [(Values es)         (compile-values es c)]
    [(LetValues xs e e0) (compile-let-values xs e e0 c)]))


(define (compile-values es c)
  (let ((i (length es)))
    (if (= 1 i)   ;; if (values e) -> compile e
        (seq (compile-e (car es) c #f)
             (assert-res-arity 1)
             (Mov r11 1))
        (seq
         (compile-es es c)
         (Mov r11 i)
         (push-to-heap i)))))

;; pops n values off the stack and push to heap
;; tag pointer values
(define (push-to-heap n)
  (if (= n 0)
      (seq
       (Mov rax val-void))
      (let ((loop (gensym 'loop)))
        (seq
         (Mov r8 n) ;;length
         (Mov rax rbx) ;; get heap pointer & tag
         (Or rax type-val)
         (Mov (Offset rbx 0) r8) ;; move to heap
         (Add rbx 8) ;; inc heap pointer
         (Label loop)
         (Pop r9) ; pop stack
         (Mov (Offset rbx 0) r9)
         (Add rbx 8)
         (Sub r8 1)
         (Cmp r8 0)
         (Jne loop)))))

(define (compile-let-values xs e e0 c)
  (let ((nv (gensym 'nv))
        (f (gensym 'f) )
        (i (length xs)))
    (seq (compile-e e c #f) ;; check if val and push from heap to stack
         (Cmp r11 i) ;; check res arity
         (Jne 'raise_error_align)
         (Mov r9 rax)
         (And r9 ptr-mask)
         (Cmp r9 type-val)
         (Jne nv)
         (Xor rax type-val)
         (push-to-stack 0 i) ;; push values to stack
         (Jmp f)
         (Label nv)
         (Push rax)
         (Label f)
         (compile-e e0 (append xs c) #f);; either 1 via rax or val through push stack
         (Add rsp (* 8 i)))))

;; Integer Integer -> Asm
(define (push-to-stack i len)
  (cond [(>= i len) (seq)]
        [else
         (seq (Mov r8 (Offset rax (* 8 (add1 i))))
              (Push r8)
              (push-to-stack (add1 i) len))]))


;; Id [Listof Expr] Expr CEnv -> Asm
(define (compile-apply f es e c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c))
         (compile-e e (append (make-list (add1 (length es)) #f) (cons #f c)) #f)   ;; rax = list addr
         (Mov r10 (length es))
         (push-list)
         (Jmp (symbol->label f))
         (Label r))))

(define (push-list)
  (let ((loop (gensym 'loop))
        (end (gensym 'end)))
    (seq (Label loop)
         (Cmp rax val-empty)
         (Je end)
         (assert-cons rax)
         (Xor rax type-cons)
         (Mov r8 (Offset rax 8))
         (Push r8)
         (Add r10 1)
         (Mov rax (Offset rax 0))
         (Jmp loop)
         (Label end))))

;; [Listof Id] [Listof Expr] -> Asm
;; General Let
(define (compile-let xs es e c)
  (seq (compile-es es c)
       (compile-e e (append (reverse xs) c) #f) ; #t , no assert res
       (Add rsp (* 8 (length xs)))))


;; [Listof Id] [Listof Expr] -> Asm
;; Back-Referencing Let
(define (compile-let* xs es e c)
  (define ls (map list es xs))
  (seq (compile* ls e c)
       (Add rsp (* 8 (length ls)))))

;; Clause Expr CEnv -> Asm
(define (compile-cond cs e c)
  (match cs
    ['() (seq (compile-e e c #f))]
    [(list (Clause p b) t ...)
     (let ((l1 (gensym 'cond))
           (l2 (gensym 'cond)))
       (seq (compile-e p c #f)
            (Cmp 'rax val-false)
            (Je l1)
            (compile-e b c #f)
            (Jmp l2)
            (Label l1)
            (compile-cond t e c)
            (Label l2)))]))

;; Expr Clause Expr CEnv -> Asm
(define (compile-case e cs el c)
  (match cs
    ['() (seq (compile-e el c #f))]
    [(list (Clause p b) t ...)
     (let ((l1 (gensym 'case))
           (l2 (gensym 'case)))
       (seq (compile-e e c #f)
            (mem p)
            (Pop 'r10)
            (Cmp 'r10 val-true)
            (Jne l1)
            (compile-e b c #f)
            (Jmp l2)
            (Label l1)
            (compile-case e t el c)
            (Label l2)))]))

;; [Listof val] -> Asm
;; generate Asm to check if list contains val in rax
(define (mem l)
  (match l
    ['() (seq (Mov 'r10 val-false)
              (Push 'r10))]
    [(list h t ...) (let ((l1 (gensym 'mem))
                          (l2 (gensym 'mem)))
                      (seq (Cmp 'rax (imm->bits h))
                           (Jne l1)
                           (Mov 'r10 val-true)
                           (Push 'r10)
                           (Jmp l2)
                           (Label l1)
                           (mem t)
                           (Label l2)))]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))
       (Mov r11 1)))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i))
         (Mov r11 1))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str)
             (Mov r11 1))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Mov r11 1)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq (Mov r11 1))]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c #f)
       (assert-res-arity 1)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c #f)
       (assert-res-arity 1)
       (Push rax)
       (compile-e e2 (cons #f c) #f)
       (assert-res-arity 1)
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c #f)
       (assert-res-arity 1)
       (Push rax)
       (compile-e e2 (cons #f c) #f)
       (assert-res-arity 1)
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)) #f)
       (assert-res-arity 1)
       (compile-op3 p)))

;; OpN Expr Expr CEnv -> Asm
(define (compile-primn p es c)
  (match es
    ['() (seq (Mov rax 0))]
    [(cons h t) (seq (compile-e h c #f)
                     (assert-res-arity 1)
                     (Push rax)
                     (compile-tail p t (cons #f c)))]))

(define (compile-tail p es c)
  (match es
    ['() (seq (Pop rax))]
    [(cons h t) (seq (compile-e h c #f)
                     (compile-op2 p)
                     (Push rax)
                     (compile-tail p t c))]))

;; Expr Expr Expr CEnv Bool -> Asm
(define (compile-if e1 e2 e3 c t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c #f)
         (assert-res-arity 1)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c t?)
         (Label l2))))

;; Expr Expr CEnv Bool -> Asm
(define (compile-begin e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (compile-e e2 c t?)))

;; Id [Listof Expr] CEnv Bool -> Asm
(define (compile-app f es c t?)
  ;(compile-app-nontail f es c)
  (if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

;; Expr [Listof Expr] CEnv -> Asm
(define (compile-app-tail e es c)
  (seq (compile-es (cons e es) c)
       (Mov r10 (length es))
       (move-args (add1 (length es)) (length c))
       (Add rsp (* 8 (length c)))
       (Mov rax (Offset rsp (* 8 (length es))))
       (assert-proc rax)
       (Xor rax type-proc)
       (Mov rax (Offset rax 0))
       (Jmp rax)))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Expr [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c)
  (let ((r (gensym 'ret))
        (i (* 8 (length es))))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c))
         (Mov r10 (length es)) ;; pass arity
         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov rax (Offset rax 0)) ; fetch the code label
         (Jmp rax)
         (Label r))))

;; Defns -> Asm
;; Compile the closures for ds and push them on the stack
(define (compile-defines-values ds)
  (seq (alloc-defines ds 0)
       (init-defines ds (reverse (define-ids ds)) 8)
       (add-rbx-defines ds 0)))

;; Defns Int -> Asm
;; Allocate closures for ds at given offset, but don't write environment yet
(define (alloc-defines ds off)
  (match ds
    ['() (seq)]
    [(cons (Defn f fun) ds)
     (let ((fvs (fv (Defn f fun))))
       (seq (Lea rax (symbol->label f))
            (Mov (Offset rbx off) rax)
            (Mov rax rbx)
            (Add rax off)
            (Or rax type-proc)
            (Push rax)
            (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns CEnv Int -> Asm
;; Initialize the environment for each closure for ds at given offset
(define (init-defines ds c off)
  (match ds
    ['() (seq)]
    [(cons (Defn f fun) ds)
     (let ((fvs (fv (Defn f fun) )))
       (seq (free-vars-to-heap fvs c off)
            (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns Int -> Asm
;; Compute adjustment to rbx for allocation of all ds
(define (add-rbx-defines ds n)
  (match ds
    ['() (seq (Add rbx (* n 8)))]
    [(cons (Defn f fun) ds)
     (add-rbx-defines ds (+ n (add1 (length (fv (Defn f fun))))))]))

;; Id [Listof Id] Expr CEnv -> Asm
(define (compile-lam f xs e c)
  (let ((fvs (fv (Lam f xs e))))
    (seq (Lea rax (symbol->label f))
         (Mov (Offset rbx 0) rax)
         (free-vars-to-heap fvs c 8)
         (Mov rax rbx) ; return value
         (Or rax type-proc)
         (Mov r11 1)
         (Add rbx (* 8 (add1 (length fvs)))))))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov r8 (Offset rsp (lookup x c)))
          (Mov (Offset rbx off) r8)
          (free-vars-to-heap fvs c (+ off 8)))]))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c #f)
          (assert-res-arity 1) ;; assert each expr returns just 1 res
          (Push rax)
          (compile-es es (cons #f c)))]))

;; [Listof Expr] CEnv -> Asm
;; Another cool function
;; kinda like compile-es but with variable binding
(define (compile* ls e c)
  (match ls
    ['() (seq (compile-e e c  #f))] ;; try #t,no assert res
    [(cons (cons ex (cons x '())) es)
     (seq (compile-e ex c #f)
          (assert-res-arity 1)
          (Push rax)
          (compile* es e (cons x c)))]))

;; Asserts result arity
(define (assert-res-arity ex)
  (seq  (Cmp r11 ex)
        (Jne 'raise_error_align)))

;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
(define (compile-match e ps es c t?)
  (let ((done (gensym)))
    (seq (compile-e e c #f)
         (assert-res-arity 1) ;; res can't be > 1
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) done t?) ;; add (Mov r11 1) ? No can return mul val
         (Jmp 'raise_error_align)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched

;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c done t?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t?)
          (compile-match-clauses ps es c done t?))]))

;; Pat Expr CEnv Symbol Bool -> Asm
(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) t?)
            (Add rsp (* 8 (length cm)))
            (Jmp done)
            f
            (Label next))])))

;; Pat CEnv Symbol -> (list Asm Asm CEnv)
(define (compile-pattern p cm next)
  (match p
    [(PWild)
     (list (seq) (seq) cm)]
    [(PVar x)
     (list (seq (Push rax))
           (seq)
           (cons x cm))]
    [(PLit l)
     (let ((fail (gensym)))
       (list (seq (Cmp rax (imm->bits l))
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next))
             cm))]
    [(PAnd p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (list
            (seq (Push rax)
                 i1
                 (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                 i2)
            (seq f1 f2)
            cm2)])])]
    [(PBox p)
     (match (compile-pattern p cm next)
       [(list i1 f1 cm1)
        (let ((fail (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-box)
                (Jne fail)
                (Xor rax type-box)
                (Mov rax (Offset rax 0))
                i1)
           (seq f1
                (Label fail)
                (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                (Jmp next))
           cm1))])]
    [(PCons p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (let ((fail (gensym)))
             (list
              (seq (Mov r8 rax)
                   (And r8 ptr-mask)
                   (Cmp r8 type-cons)
                   (Jne fail)
                   (Xor rax type-cons)
                   (Mov r8 (Offset rax 0))
                   (Push r8)                ; push cdr
                   (Mov rax (Offset rax 8)) ; mov rax car
                   i1
                   (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              (seq f1
                   f2
                   (Label fail)
                   (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                   (Jmp next))
              cm2))])])]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
          (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
