#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run)

    ;; Iniquity tests
  (check-equal? (run
                 '(define (f x) x)
                 '(f 5))
                5)
  (check-equal? (run
                 '(define (tri x)
                    (if (zero? x)
                        0
                        (+ x (tri (sub1 x)))))
                 '(tri 9))
                45)

  (check-equal? (run
                 '(define (even? x)
                    (if (zero? x)
                        #t
                        (odd? (sub1 x))))
                 '(define (odd? x)
                    (if (zero? x)
                        #f
                        (even? (sub1 x))))
                 '(even? 101))
                #f)

  (check-equal? (run
                 '(define (map-add1 xs)
                    (if (empty? xs)
                        '()
                        (cons (add1 (car xs))
                              (map-add1 (cdr xs)))))
                 '(map-add1 (cons 1 (cons 2 (cons 3 '())))))
                '(2 3 4))
  
  ;; Iniquity+
  (check-equal? (run '(define (f x) x)
                     '(f))
                'err)
  (check-equal? (run '(define (f) 1)
                     '(f 2))
                'err)
  (check-equal? (run '(define (f x) x)
                     '(let ((y 2))
                        (f 1 y)))
                'err)
  (check-equal? (run '(define (f . xs)
                        (if (empty? xs)
                            #t
                            (f)))
                     '(f 1 2 3))
                #t)
  (check-equal? (run '(define (list . xs) xs)
                     '(list (list) (list 1 2 3) (list #t) (list 3 4 5)))
                '(() (1 2 3) (#t) (3 4 5)))
  (check-equal? (run '(define (f x y . z) (cons x (cons y z)))
                     '(cons (f 1 2) (cons (f 8 9 10) '())))
                '((1 2) (8 9 10)))
  (check-equal? (run '(define (f x . xs) x)
                     '(f 1))
                1)
  (check-equal? (run '(define (f x . xs) xs)
                     '(f 1))
                '())
  (check-equal? (run '(define (f x . xs) xs)
                     '(f))
                'err)
  (check-equal? (run '(define (f x . xs) xs)
                     '(let ((x 3))
                        (f 1 x)))
                '(3))
  (check-equal? (run '(define f
                        (case-lambda))
                     '(f))
                'err)
  (check-equal? (run '(define f
                        (case-lambda))
                     '(add1 8))
                9)
  (check-equal? (run '(define f
                        (case-lambda
                          [(x) x]))
                     '(f 1))
                1)
  (check-equal? (run '(define f
                        (case-lambda
                          [x #t]
                          [(x) x]))
                     '(f 1))
                #t)
  (check-equal? (run '(define f
                        (case-lambda
                          [(x y) #f]
                          [(x) x]))
                     '(cons (f 1) (cons (f 1 2) '())))
                '(1 #f))
  (check-equal? (run '(define f
                        (case-lambda
                          [x #f]
                          [y #t]))
                     '(cons (f 1) (cons (f 1 2) '())))
                '(#f #f))
  (check-equal? (run '(define f
                        (case-lambda
                          [(x y . z) z]
                          [(x) (+ x x)]
                          [z 2]))
                     '(cons (f 1 2)
                            (cons (f 1)
                                  (cons (f 1 2 3)
                                        '()))))
                '(() 2 (3)))

  (check-equal? (run '(define (f) 1)
                     '(apply f '()))
                1)
  (check-equal? (run '(define (f . xs) 1)
                     '(apply f '()))
                1)
  (check-equal? (run '(define (f . xs) xs)
                     '(apply f '()))
                '())
  (check-equal? (run '(define (f . xs) xs)
                     '(apply f (cons 1 (cons 2 (cons 3 '())))))
                '(1 2 3))
  (check-equal? (run '(define (f . xs) xs)
                     '(apply f 1 2 (cons 3 '())))
                '(1 2 3))
  (check-equal? (run '(define (append . xss)
                        (if (empty? xss)
                            '()
                            (if (empty? (car xss))
                                (apply append (cdr xss))
                                (cons (car (car xss))
                                      (apply append (cdr (car xss)) (cdr xss))))))
                     '(define (list . xs) xs)
                     '(define (flatten xs)
                        (apply append xs))
                     '(flatten (list (append) (append (list 1 2 3) (list 4 5) (list 6)) (list 7))))
                '(1 2 3 4 5 6 7))
  )

(define (test-runner-io run)
 
  ;; Iniquity examples
  (check-equal? (run ""
                     '(define (print-alphabet i)
                        (if (zero? i)
                            (void)
                            (begin (write-byte (- 123 i))
                                   (print-alphabet (sub1 i)))))
                     '(print-alphabet 26))
                (cons (void) "abcdefghijklmnopqrstuvwxyz"))

  (check-equal? (run ""
                     '(define (f x)
                        (write-byte x))
                     '(f 97))
                (cons (void) "a"))
  (check-equal? (run ""
                     '(define (f x y)
                        (write-byte x))
                     '(f 97 98))
                (cons (void) "a"))
  (check-equal? (run ""
                     '(define (f x)
                        (let ((y x))
                          (write-byte y)))
                     '(f 97))
                (cons (void) "a"))
  (check-equal? (run ""
                     '(define (f x y)
                        (let ((y x))
                          (write-byte y)))
                     '(f 97 98))
                (cons (void) "a"))
  (check-equal? (run ""
                     '(define (f x)
                        (write-byte x))
                     '(let ((z 97))
                        (f z)))
                (cons (void) "a"))
  (check-equal? (run ""
                     '(define (f x y)
                        (write-byte x))
                     '(let ((z 97))
                        (f z 98)))
                (cons (void) "a")))
