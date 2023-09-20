#lang racket
(require "ast.rkt")
(provide interp-prim1 interp-prim2 interp-prim3 interp-primN)

;; Op1 Value -> Answer
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'char? v)                      (char? v)]
    [(list 'char->integer (? char?))      (char->integer v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'eof-object? v)                (eof-object? v)]
    [(list 'write-byte (? byte?))         (write-byte v)]
    [(list 'box v)                        (box v)]
    [(list 'unbox (? box?))               (unbox v)]
    [(list 'car (? pair?))                (car v)]
    [(list 'cdr (? pair?))                (cdr v)]
    [(list 'empty? v)                     (empty? v)]
    [(list 'cons? v)                      (cons? v)]
    [(list 'box? v)                       (box? v)]
    [(list 'vector? v)                    (vector? v)]
    [(list 'vector-length (? vector?))    (vector-length v)]
    [(list 'string? v)                    (string? v)]
    [(list 'string-length (? string?))    (string-length v)]
    [(list '- (? integer?))               (- v)]
    [(list 'abs (? integer?))             (abs v)]
    [(list 'integer? v)                   (integer? v)]
    [(list 'boolean? v)                   (boolean? v)]
    [(list 'not v)                        (not v)]
    [_               'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (? integer?) (? integer?))  (+ v1 v2)]
    [(list '- (? integer?) (? integer?))  (- v1 v2)]
    [(list '< (? integer?) (? integer?))  (< v1 v2)]
    [(list '= (? integer?) (? integer?))  (= v1 v2)]    
    [(list 'cons v1 v2)                   (cons v1 v2)]
    [(list 'eq? v1 v2)                    (eq? v1 v2)]    
    [(list 'make-vector (? integer?) _)
     (if (<= 0 v1)
         (make-vector v1 v2)
         'err)]
    [(list 'vector-ref (? vector?) (? integer?))
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-ref v1 v2)
         'err)]
    [(list 'make-string (? integer?) (? char?))
     (if (<= 0 v1)
         (make-string v1 v2)
         'err)]
    [(list 'string-ref (? string?) (? integer?))
     (if (<= 0 v2 (sub1 (string-length v1)))
         (string-ref v1 v2)
         'err)]
    [(list 'arithmetic-shift (? integer?) (? integer?)) 
        (arithmetic-shift v1 v2)]
    [_ 'err]))

;; Op3 Value Value Value -> Answer
(define (interp-prim3 p v1 v2 v3)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (? vector?) (? integer?) _)
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-set! v1 v2 v3)
         'err)]
    [_ 'err]))

;; OpN [Listof Value] -> Answer
(define (interp-primN op vs)
  (match vs
    ['() 0]
    [else (match (equal? vs (takef vs integer?))
            [#f 'err]
            [#t (n* (match op ['+ +] ['- -]) 
                    (cdr vs) (car vs))])]))

;; OpN [Listof Value] initial -> ans
(define (n* op ls a)  
(match ls
  ['() a]
  [(cons h t) (n* op t (op a h))]))     


;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
