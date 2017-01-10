#lang racket
(define reverse-with-count
  (λ (xs ys) 
    (cond ((null? (cdr xs))
           `(,@(build-list (car ys) (const (car xs)))))
          (else `(,@(reverse-with-count (cdr xs) (cdr ys)) ,@(build-list (car ys) (const (car xs))))))))