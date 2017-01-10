#lang racket
(define after-filter
  (λ (filt xs) 
    (cond ((null? (cddr xs)) (if (filt (car xs)) `(,(cadr xs)) '()))
          (else `(,@(if (filt (car xs)) `(,(cadr xs)) '()) ,@(after-filter filt (cdr xs)))))))