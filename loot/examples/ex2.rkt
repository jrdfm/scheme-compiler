#lang racket

(let ((f (let ((in (read-byte))) 
               (let ((y 42))
               (let ((g (λ (x) (+ x in))  ))
               (let ((h (λ (x) (+ y in))))
                     
               (if (= in 97)
                   (λ (x) (+ x in))
                   (λ (x) (+ y in)))))))))               
(f 1))