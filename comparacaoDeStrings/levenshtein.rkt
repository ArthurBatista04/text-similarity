#lang racket
(require memoize)
; referencia https://en.wikipedia.org/wiki/Levenshtein_distance

(define (levenshtein text1 text2) ;retorna um valor entre 0-1 com 1 indicando identicos e 0 totalmente diferentes
    (if (>= (string-length text1) (string-length text2)) ;a string que possui maior comprimento
         (- 1 ( / (calculteLevenshtein text1 text2 (string-length text1) (string-length text2)) (string-length text1)))     
         (- 1 ( / (calculteLevenshtein text1 text2 (string-length text1) (string-length text2)) (string-length text2)))     
    )
   
)

(define/memo (calculteLevenshtein text1 text2 sizeOfText1 sizeOfText2)
    (if (= sizeOfText1 0)  ; caso base (se as strings forem vazias)
        0.0
        (if (= sizeOfText2 0) ; caso base (se as strings forem vazias)
            0.0
            (if (string=? (~a (string-ref text1 (- sizeOfText1 1 ))) (~a (string-ref text2 (- sizeOfText2 1 ))) ) ; verifica se as ultimas posicoes das string sao iguais
                
                (min ; retorna o min entre a exclusão do ultimo char da instancia
                    (+ (calculteLevenshtein text1 text2 (- sizeOfText1 1) sizeOfText2) 1.0) 
                    (+ (calculteLevenshtein text1 text2 sizeOfText1 (- sizeOfText2 1)) 1.0)
                    (+ (calculteLevenshtein text1 text2 (- sizeOfText1 1) (- sizeOfText2 1)) 0.0)
                )
                (min 
                    (+ (calculteLevenshtein text1 text2 (- sizeOfText1 1) sizeOfText2) 1.0) 
                    (+ (calculteLevenshtein text1 text2 sizeOfText1 (- sizeOfText2 1)) 1.0)
                    (+ (calculteLevenshtein text1 text2 (- sizeOfText1 1) (- sizeOfText2 1)) 1.0)
                )
            )
        )
    )
)

(levenshtein "arthur" "douglas")
;;(levenshtein "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In eu arcu urna. Etiam ullamcorper lorem turpis, id vulputate sapien ultrices in. Morbi eget neque porta, sagittis dolor eget, tincidunt diam. Vestibulum auctor purus ac mattis aliquam. Mauris finibus id sapien vitae tempor. Phasellus ac est ut nisl lobortis faucibus. In ullamcorper congue turpis, vel placerat ligula pellentesque at. Quisque mollis magna eget finibus bibendum. Vivamus sed sapien id justo volutpat gravida. Proin euismod vestibulum mattis. " "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In eu arcu urna. Etiam ullamcorper lorem turpis, id vulputate sapien ultrices in. Morbi eget neque porta, sagittis dolor eget, tincidunt diam. Vestibulum auctor purus ac mattis aliquam. Mauris finibus id sapien vitae tempor. Phasellus ac est ut nisl lobortis faucibus. In ullamcorper congue turpis, vel placerat ligula pellentesque at. Quisque mollis magna eget finibus bibendum. Vivamus sed sapien id justo volutpat gravida. Proin euismod vestibulum mattis. ")