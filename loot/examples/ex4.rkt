#lang racket

(define (f x)
        (values x (+ x 1) (+ x 2)))
(let-values ([(x y z) (f 5)])
        (cons x (cons y (cons z '()))))