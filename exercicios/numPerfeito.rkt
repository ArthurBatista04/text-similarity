#lang racket
(define (perfeito x y)
    (if (= (remainder x (sub1 y)) 0) 
        #t 
        (perfeito(x (- y 1)))
    )
)

(perfeito 4 4)