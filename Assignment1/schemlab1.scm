#lang scheme
;;; Add macros to Scheme interpreter.

;;; Option One:

(define eval_ADD-MACROS-ONE-UGLY
  (λ (e env)
    (cond ((symbol? e) (lookup-var e env))
          ((not (list? e)) e) ; non-list non-symbol is self-evaluatory
          ;; special forms go here:
          ((equal? (car e) 'λ)		; (λ VARS BODY)
           (let ((vars (cadr e)) (body (caddr e)))
             (list '%closure vars body env)))
          ((equal? (car e) 'if)
           (eval_ (if (eval_ (cadr e) env) (caddr e) (cadddr e)) env))
          ((equal? (car e) 'quote) (cadr e))
          ;; Add More Macros Here:
          ((equal? (car e) 'let) error)
          ((equal? (car e) 'cond) error)
          ((equal? (car e) 'and) error)
          ((equal? (car e) 'or) error)
          ;; otherwise must be regular function call
          (else (let ((eeoe (map (λ (e0) (eval_ e0 env)) e)))
                  (apply_ (car eeoe) (cdr eeoe)))))))

;;; Ick: not modular.

;;; Option Two:

(define eval_ (λ (e env) (eval-core (de-sugar e) env)))

(define eval-core
  (λ (e env)
    (cond ((symbol? e) (lookup-var e env))
          ((not (list? e)) e) ; non-list non-symbol is self-evaluatory
          ;; special forms go here:
          ((equal? (car e) 'λ)		; (λ VARS BODY)
           (let ((vars (cadr e)) (body (caddr e)))
             (list '%closure vars body env)))
          ((equal? (car e) 'quote) (cadr e))
          ;; No macros here, assume already expanded away.
          ;; otherwise must be regular function call
          (else (let ((eeoe (map (λ (e0) (eval-core e0 env)) e)))
                  (apply_ (car eeoe) (cdr eeoe)))))))

(define de-sugar
  (λ (e)
    (cond ((not (pair? e)) e)
          (else (let ((f (car e)))
                  (cond ((equal? f 'λ)
                         (list 'λ (cadr e) (de-sugar (caddr e))))
                        ((equal? f 'quasiquote)
                         ((cadadr (assoc f macro-alist)) e))
                        ;; macros:
                        ((assoc f macro-alist)
                         ;; Instead of this:
                         ;;  ((cadr (assoc f macro-alist)) e)
                         ;; can write this:
                         => (λ (x) (de-sugar ((cadr x) e))))
                        ;; regular function call:
                        (else (map de-sugar e))))))))

(define macro-alist
  `((and ,(λ (e)
            ;; (and A B) -> (if A B #f)
            ;; (and A B C ...) -> (if A (and B C ...) #f)
            ;; (and A) -> A
            ;; (and) -> #t
            (let ((forms (cdr e)))
              (cond ((null? forms) '#t)
                    ((null? (cdr forms)) (car forms))
                    (else `(if ,(car forms) (and ,@(cdr forms)) #f))))))
    (or ,(λ (e)
           ;;(or A B) -> (if A A B)
           ;;(or A B C...) -> (if A A (or B C ...))
           ;;(or A) -> A
           ;;(or) -> #t
           (let((forms (cdr e)))
             ;;scheme returns #f for or
             (cond((null? forms) '#f)
                  ((null? (cdr forms)) (car forms))
                  (else `(if ,(car forms) ,(car forms) (or ,@(cdr forms))))))))
    (let ,(λ (e)
            ;;Turns (let ((var1 val1) (var2 val2)) (func)) -> ((λ (var1 var2) (func)) val1 val2)
            (let ((forms (cdr e)))
              `((λ ,(map car (car forms)) ,@(cdr forms)) ,@(map cadr (car forms))))))
    (cond ,(λ (e)
             ;;(cond (A) (B)) -> (if A A B)
             ;;(cond ((procA) A) ((procB) B)) -> (if procA A (cond ((procB) B)))
             ;;(cond ((procA) A) ((procB) B) (else C)) -> (if procA A (cond ((procB) B (else C)))
             ;;(cond (A)) -> A
             (let((forms (cdr e)))
               ;;scheme returns nothing here
               ;;not really possible here due to simple implementation
               ;;return #t instead
               (cond((null? forms) '#t)
                    ((null? (cdar forms)) (if (caar forms) (caar forms) #t))
                    ((equal? (caar forms) 'else) (cadar forms))
                    (else `(if ,(caar forms) ,(cadar forms) (cond ,@(cdr forms))))))))
    (if ,(λ (e) (let ((guard (cadr e))
                      (then-part (caddr e))
                      (else-part (cadddr e)))
                  ;; Sugar to write this more conveniently:
                  ;; (list (list '%if
                  ;; 	       guard
                  ;; 	       (list 'λ '() then-part)
                  ;; 	       (list 'λ '() else-part)))
                  ;; use (quasiquote e), aka: `e
                  ;; (quasiquote ((%if (unquote guard)
                  ;;                   (λ () (unquote then-part))
                  ;;                   (λ () (unquote else-part)))))
                  ;; which itself can be abbreviated:
                  `((%if ,guard (λ () ,then-part) (λ () ,else-part))))))
    (quasiquote ,,(λ (e)
                    `(quote ,(de-nest (quasi (cdr e))))))))

(define quasi
  (λ (forms)
    (cond((or (null? forms) (not(list? forms))) forms)
         ((equal? (car forms) 'unquote) `,(eval_ `,(cadr forms)'()))
         ((equal? (car forms) 'unquote-splicing) `,(de-nest (map eval `,(cdr forms))))
         (else `,(de-nest `(,@(map quasi (list(car forms))) ,@(quasi (cdr forms))))))))

(define de-nest
  (λ (e)
    (if (and
         (null? (cdr e))
         (list? (car e))) (car e) e))) 

;;; Copied from prev lecture:

(define apply_
  (λ (f args)
    (cond ((procedure? f) (apply f args))
          ((equal? (car f) '%closure)
           (let ((vars (cadr f))
                 (body (caddr f))
                 (env (cadddr f)))
             (eval_ body (append (map list vars args) env))))
          (else (error "error: call to non-procedure" f)))))

(define lookup-var
  (λ (s env)
    (cadr (or (assoc s env)
              (assoc s global-variable-alist)
              (error "error: unbound variable" s)))))

(define global-variable-alist
  (list (list 'pi pi)
        (list 'e (exp 1))
        (list '+ +) (list '* *) (list '- -)
        (list 'sin sin)
        (list '%if (λ (g a b) (if g a b)))
        (list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'list list)
        ))

(define mark-tail-calls
  ;;() - > ()
  ;;(a) -> (a)
  ;;(a b) -> (tail-call a b)
  ;;(a (b c)) -> (tail-call a (ntc b c))
  ;;((a b) (c d)) -> (tc (ntc a b) (ntc c d))
  ;;(if (a b) (c d) (e f)) -> (if (ntc a b) (tc c d) (tc e f))
  (λ (e) (tail-calls e)))

(define tail-calls
  (λ (e)
    (cond ((or (null? e) (not (pair? e))) e)
          ((pair? (car e)) `(tail-call ,@(map non-tail-calls e)))
          ((equal? (car e) 'if) (if (equal? (length e) 4) `(if ,(non-tail-calls (cadr e)) ,@(map tail-calls (cddr e))) (error "Invalid if statement")))
          (else `(tail-call ,(car e) ,@(map non-tail-calls (cdr e)))))))

(define non-tail-calls
  (λ (e)
    (cond ((or (null? e) (not (pair? e))) e)
          ((pair? (car e)) `(non-tail-call ,@(map non-tail-calls e)))
          ((equal? (car e) 'if) `(if ,@(map non-tail-calls (cdr e))))
          (else `(non-tail-call ,(car e) ,@(map non-tail-calls (cdr e)))))))