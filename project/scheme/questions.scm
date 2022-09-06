(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
    (define (helper s first second)
        (if (null? s) (list first second)
            (helper (cdr s) (append first (list (caar s))) (append second (list (car (cdar s)))))
        )
    )
    (helper pairs nil nil)
)


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
    (define (helper s i ans)
        (cond
            ((null? s) ans)
            (else (helper (cdr s) (+ i 1) (append ans (list (list i (car s))))))
        )
    )
    (helper s 0 nil)     
)
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
    (define (helper comp a b ans) 
        (cond
            ((null? a) (append ans b))
            ((null? b) (append ans a))
            ((comp (car a) (car b)) (helper comp (cdr a) b (append ans (list (car a)))))
            (else (helper comp a (cdr b) (append ans (list (car b)))))
        )
    )
    (helper comp list1 list2 nil)
)
  ; END PROBLEM 16


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
            (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 17
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
            (define tmp (zip values))
            (append (cons (cons 'lambda (cons (car tmp) (map let-to-lambda body))) nil) (map let-to-lambda (cadr tmp)))
           ; END PROBLEM 17
           ))
        (else
         ; BEGIN PROBLEM 17
            (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM 17
         )))

;;Difficult!!! (Use zip)