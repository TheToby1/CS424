#lang racket
(define add-numbers
  (λ (xs) 
    (cond
      ((not(list? xs)) (if (number? xs) xs 0))
      ((list? (car xs)) (+ (add-numbers (car xs)) (add-numbers (cdr xs))))
      ((null? (cdr xs)) (if (number? (car xs)) (car xs) 0))
      (else (+ (if (number? (car xs)) (car xs) 0) (add-numbers (cdr xs)))))))