#lang racket

(define (f a)
(cond [(= 0 a) '()]
      [else (begin (writeln a) (f (sub1 a)))]))