#lang racket

(define (f x)
        (values x (+ x 1) (+ x 2)))
(values 1 2 '() (cons 1 (cons 2 '())))