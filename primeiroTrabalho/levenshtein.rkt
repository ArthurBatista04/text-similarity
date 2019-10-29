#lang racket
(require memoize)
; referencia https://en.wikipedia.org/wiki/Levenshtein_distance

(define (levenshtein text1 text2) ;retorna um valor entre 0-1 com 1 indicando identicos e 0 totalmente diferentes
    (if (and (= (string-length text1) 0) (= (string-length text2) 0)) ;; verifica se as strings são ambas vazias e retorna 1 se forem
        1.0
        (if (>= (string-length text1) (string-length text2)) ;a string que possui maior comprimento
            (- 1 ( / (calculateLevenshtein text1 text2 (string-length text1) (string-length text2)) (string-length text1)))     
            (- 1 ( / (calculateLevenshtein text1 text2 (string-length text1) (string-length text2)) (string-length text2)))     
        )

    )
    
   
)

(define/memo (calculateLevenshtein text1 text2 sizeOfText1 sizeOfText2)
    (if (= sizeOfText1 0)  ; caso base (se as strings forem vazias)
        0.0
        (if (= sizeOfText2 0) ; caso base (se as strings forem vazias)
            0.0
            (if (string=? (~a (string-ref text1 (- sizeOfText1 1 ))) (~a (string-ref text2 (- sizeOfText2 1 ))) ) ; verifica se as ultimas posicoes das string sao iguais
                
                (min ; retorna o min entre a exclusão do ultimo char da instancia
                    (+ (calculateLevenshtein text1 text2 (- sizeOfText1 1) sizeOfText2) 1.0) 
                    (+ (calculateLevenshtein text1 text2 sizeOfText1 (- sizeOfText2 1)) 1.0)
                    (+ (calculateLevenshtein text1 text2 (- sizeOfText1 1) (- sizeOfText2 1)) 0.0)
                )
                (min 
                    (+ (calculateLevenshtein text1 text2 (- sizeOfText1 1) sizeOfText2) 1.0) 
                    (+ (calculateLevenshtein text1 text2 sizeOfText1 (- sizeOfText2 1)) 1.0)
                    (+ (calculateLevenshtein text1 text2 (- sizeOfText1 1) (- sizeOfText2 1)) 1.0)
                )
            )
        )
    )
)



(provide levenshtein calculateLevenshtein)