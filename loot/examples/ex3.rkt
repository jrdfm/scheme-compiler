#lang racket

(let ((f (λ (x) (values x (+ x 1) (+ x 2))))) (f 1))

