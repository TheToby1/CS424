#lang racket
(define deep-fetch
  (λ (f xs) 
    (cond 
      ((null? (cdr xs)) (cond
                          ((list? (car xs)) (deep-fetch (car xs)))
                          (else (if (f (car xs)) `(,(car xs)) '()))))
      ((list? (car xs)) `(,@(deep-fetch f (car xs)) ,@(deep-fetch f (cdr xs))))
      (else `(,@(if (f (car xs)) `(,(car xs)) '()) ,@(deep-fetch f (cdr xs)))))))