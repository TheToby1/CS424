#lang racket
(define tr
  (λ (x)
    (cond ((null? (cdar x)) 
           (list (map car x)))
          (else 
           `(,@(list (map car x)) ,@(tr (map cdr x)))))))