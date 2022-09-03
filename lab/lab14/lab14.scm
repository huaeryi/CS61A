(define (split-at lst n)
    (define (help head tail n)
        (cond
            ((null? tail) (cons head tail))
            ((= n 0) (cons head tail))
            (else (help (append head (list(car tail))) (cdr tail) (- n 1)))
        )
    )
    (help nil lst n)
)

(define (compose-all funcs) 
    (lambda (res)
        (cond
            ((null? funcs) res)
            (else ((compose-all (cdr funcs)) ((car funcs) res)))
        )
    )
)


