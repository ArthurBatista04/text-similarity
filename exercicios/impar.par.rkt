#lang racket
(define (par? x)
    (if (= (remainder x 2) 0) 
        #t
        (if (impar? x)
            #f
            #t
        )
    )
)
(define (impar? x)
    (if (= (remainder x 2) 0) 
        (if (par? x)
            #f
            #t
        )
        #t
    )
)

(impar? 2)
(par? 10)
(impar? 0)
(par? 0)