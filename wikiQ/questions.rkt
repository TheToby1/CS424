#lang racket
;;Q1
(define mylast
  (λ (xs)
    (cond ((null? (cdr xs)) (car xs))
          (else (mylast (cdr xs))))))

;;Q2
(define mybutlast
  (λ (xs)
    (cond ((null? (cddr xs)) (car xs))
          (else (mybutlast (cdr xs))))))

;;Q3
(define elementat
  (λ (xs x)
    (cond ((equal? x 1) (car xs))
          (else (elementat (cdr xs) (- x 1))))))

;;Q4
(define mylength
  (λ (xs)
    (cond ((null? (cdr xs)) 1)
          (else (+ 1 (mylength (cdr xs)))))))

;;Q5
(define myreverse
  (λ (xs)
    (cond ((null? (cdr xs)) `(,(car xs)))
          (else `(,@(myreverse (cdr xs)) ,(car xs))))))

;;Q6
(define palindrome
  (λ (xs)
    (equal? xs (myreverse xs))))

;;Q7
(define myflatten
  (λ (xs)
    `(,@(cond ((list? (car xs)) ((myflatten (car xs))))
              (else `(,(car xs))))
      ,@(if (null? (cdr xs)) '() (myflatten (cdr xs))))))





