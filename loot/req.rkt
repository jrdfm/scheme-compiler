#lang racket
(provide (all-defined-out))
(require "interp.rkt" "ast.rkt" "interp-prims.rkt" "parse.rkt" "unload-bits-asm.rkt"
         "types.rkt" "compile.rkt" a86/printer a86/ast a86 "lambdas.rkt")

(current-objs '("runtime.o"))

(provide (all-from-out "interp.rkt" "ast.rkt" "interp-prims.rkt" "parse.rkt" "unload-bits-asm.rkt"
         "types.rkt" "compile.rkt" a86/printer a86/ast a86 "lambdas.rkt"))

(define (sho e)

(unload/free (asm-interp (compile (parse e)))))

(define (show e)

(asm-display (compile (parse e))))

(define (p . p) (parse p))