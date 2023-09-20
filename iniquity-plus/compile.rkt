#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

(define r9 'r9) ;
(define r8  'r8)  ; scratch
(define r11  'r11); # result arity
(define r10 'r10) ; # fun args

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           (Or rsp 8)
           (Jmp 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  (match fun
    [(FunPlain xs e)
     (seq (Label (symbol->label f))
          (Cmp r10 (length xs))       ;; check arity
          (Jne 'raise_error_align)
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret))]
    [(FunRest xs x e)
     (let ((loop (gensym 'loop))
           (eq (gensym 'eq)))
       (seq (Label (symbol->label f))
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
            (Push rax)     ;; push list
            (compile-e e (cons x (reverse xs)))
            (Add rsp (* 8 (+ 1 (length xs)))) ;; pop args + list
            (Ret)))]
    [(FunCase cs)
     (seq (Label (symbol->label f))
          (compile-fun-case cs)
          (Ret))]))

(define (compile-fun-case cs)
  (match cs
    ['() (seq (Jmp 'raise_error_align))]
    [(cons (FunPlain xs e) t)
     (let ((l (gensym 'l))
           (en (gensym 'en) ))
       (seq (Label l)
            (Cmp r10 (length xs))
            (Jne en)        ;; check arity
            (compile-fun l (FunPlain xs e))
            (Label en)
            (compile-fun-case t )))]
    [(cons (FunRest xs x e) t)
     (let ((l (gensym 'l))
           (en (gensym 'en)))
       (seq (Label l)
            (Cmp r10 (length xs))
            (Jl en)         ;; less is not more
            (compile-fun l (FunRest xs x e))
            (Label en)
            (compile-fun-case t )))]	))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)               (compile-value i)]
    [(Bool b)              (compile-value b)]
    [(Char c)              (compile-value c)]
    [(Eof)                 (compile-value eof)]
    [(Empty)               (compile-value '())]
    [(Var x)               (compile-variable x c)]
    [(Str s)               (compile-string s)]
    [(Prim0 p)             (compile-prim0 p c)]
    [(Prim1 p e)           (compile-prim1 p e c)]
    [(Prim2 p e1 e2)       (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3)    (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)         (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)         (compile-begin e1 e2 c)]
    [(Let x e1 e2)         (compile-let x e1 e2 c)]
    [(App f es)            (compile-app f es c)]
    [(Apply f es e)        (compile-apply f es e c)]
    [(Values es)           (compile-values es c)]
    [(LetValues xs e e0)   (compile-let-values xs e e0 c)]))



(define (compile-values es c)
  (let ((i (length es)))
    (if (= 1 i)   ;; if (values e) -> compile e
        (seq (compile-e (car es) c)
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
    (seq (compile-e e c) ;; check if val and push from heap to stack
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
         (compile-e e0 (append xs c));; either 1 via rax or val through push stack
         (Add rsp (* 8 i)))))

;; Integer Integer -> Asm
(define (push-to-stack i len)
  (cond [(>= i len) (seq)]
        [else
         (seq (Mov r8 (Offset rax (* 8 (add1 i))))
              (Push r8)
              (push-to-stack (add1 i) len))]))

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
  (seq (compile-e e c)
       (assert-res-arity 1)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (assert-res-arity 1)
       (Push rax)
       (compile-e e2 (cons #f c))
       (assert-res-arity 1)
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (assert-res-arity 1)
       (Push rax)
       (compile-e e2 (cons #f c))
       (assert-res-arity 1)
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (assert-res-arity 1)
       (compile-op3 p)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (assert-res-arity 1)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (assert-res-arity 1)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (Mov r10 (length es))
         (Jmp (symbol->label f))
         (Label r))))

;; Id [Listof Expr] Expr CEnv -> Asm
(define (compile-apply f es e c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (compile-e e (append (make-list (length es) #f) (cons #f c)))   ;; rax = list addr
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

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (assert-res-arity 1) ;; assert each expr returns just 1 res
          (Push rax)
          (compile-es es (cons #f c)))]))

(define (assert-res-arity ex)
  (seq  (Cmp r11 ex)
        (Jne 'raise_error_align)))


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
