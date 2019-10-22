#lang racket
(define (fib x vec)
    (if (= (vector-ref vec x) -1)
        (if (= x 0) 
            0 
            (if (= x 1) 
                1
                (vector-set! vec x (+ (fib ((- x 1) vec)) (fib ((- x 2) vec))))
            )
        )
        (vector-ref vec x)
    )
)

fib(1 (make-vector 100 -1))