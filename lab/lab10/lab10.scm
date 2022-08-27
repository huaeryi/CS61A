(define (over-or-under num1 num2) 
  'YOUR-CODE-HERE
  (cond
     ((< num1 num2) -1)
     ((= num1 num2) 0)
     ((> num1 num2) 1)
  )
)

; ;; Tests
(over-or-under 1 2)

; expect -1
(over-or-under 2 1)

; expect 1
(over-or-under 1 1)

; expect 0
(define (make-adder num) 
  'YOUR-CODE-HERE
  (lambda (y) (+ num y))
)

; ;; Tests
(define adder (make-adder 5))

(adder 8)

; expect 13
(define (composed f g) 
  'YOUR-CODE-HERE
  (lambda (y) (f (g y)))
)

(define lst 
  '((1) 2 (3 4) 5)
)

(define (remove item lst) 
    (if (null? lst) nil
        (if (= item (car lst))
            (remove item (cdr lst))
            (cons (car lst) (remove item (cdr lst)))
        )
    )
)

; ;; Tests
(remove 3 nil)

; expect ()
(remove 3 '(1 3 5))

; expect (1 5)
(remove 5 '(5 3 5 5 1 4 5 4))

; expect (3 1 4 4)
