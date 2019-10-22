#lang racket

(define (soma x y)
    (if (zero? y) 
        x 
        (soma(add1 x) (sub1 y))
    )
)

(define (subtracao x y)
    (if (zero? y) 
        x 
        (subtracao(sub1 x) (sub1 y))
    )
)

(define (mult x y)
    (if (zero? y) 
        x 
        (soma (x (mult(x (sub1 y)))))
    )
)


(soma 40 100)
(subtracao 20 40)
(mult 2 3)