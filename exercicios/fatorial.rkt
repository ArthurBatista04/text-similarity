#lang racket
(define (fat x)
    (if (= x 1) 
        1 
        (* x (fat(- x 1)))
    )
)

(fat 5)
