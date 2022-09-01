(define (tail-replicate x n)
    (define (help x n now)
        (if (= n 0) now
            (help x (- n 1) (cons x now))    
        )
    )
    (help x n nil)
)


(define-macro (def func args body)
    `(define (,func ,@args)
        ,body
    )
)

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let ((y (repeatedly-cube (- n 1) x)))
        (* y y y))))
