#lang racket

(let ((f (let ((in (read-byte))) 
               (let ((y 42))
               (if (= in 97)
                   (λ (x) (+ x in))
                   (λ (x) (+ y in)))))))
(f 1))


