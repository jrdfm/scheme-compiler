#lang racket
(define (append . xss)
                            (if (empty? xss)
                                '()
                                (if (empty? (car xss))
                                    (apply append (cdr xss))
                                    (cons (car (car xss))
                                          (apply append (cdr ( car xss)) (cdr xss))))))


(define (list . xs) xs)
(define (flatten xs)
(apply append xs))
(flatten (list (append) (append (list 1 2 3) (list 4 5) (list 6)) (list 7)))