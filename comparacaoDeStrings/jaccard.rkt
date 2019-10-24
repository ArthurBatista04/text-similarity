#lang racket
(require memoize)
; referencia https://en.wikipedia.org/wiki/Jaccard_index

(define/memo (jaccard text1 text2) ;retorna um valor entre 0-1 com 1 indicando identicos e 0 totalmente diferentes
    (if (and (= (countSetOfWords text1) 0) (= (countSetOfWords text2) 0))  ;; se a quantidade de palavras em ambos os textos forem zero retorna 1
        1.0
        (exact->inexact(/ (countIntersectionOfWords (makeSetOfWords text1) (makeSetOfWords text2)) 
                    (- (+ (countSetOfWords text1) (countSetOfWords text2)) (countIntersectionOfWords (makeSetOfWords text1) (makeSetOfWords text2)))))
    )
    
)

(define (makeSetOfWords text) ;; transforma uma string em um conjunto de palavras
    (list->set (string-split text))
)

(define (countIntersectionOfWords set1 set2) ;; conta a quantidade de palavras em comuns entre dois texto
    (set-count (set-intersect set1 set2))
)

(define (countSetOfWords text) ;; conta a quantidade de palavras em um texto
    (set-count (list->set (string-split text)))
)

(provide jaccard)