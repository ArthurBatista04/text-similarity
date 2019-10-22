#lang racket
(require memoize)
; referencia https://en.wikipedia.org/wiki/Jaccard_index

(define/memo (jaccard text1 text2) ;retorna um valor entre 0-1 com 1 indicando identicos e 0 totalmente diferentes
   (exact->inexact(/ (countIntersectionOfWords (makeSetOfWords text1) (makeSetOfWords text2)) 
                    (- (+ (countSetOfWords text1) (countSetOfWords text2)) (countIntersectionOfWords (makeSetOfWords text1) (makeSetOfWords text2)))))
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




(jaccard "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In eu arcu urna. Etiam ullamcorper lorem turpis, id vulputate sapien ultrices in. Morbi eget neque porta, sagittis dolor eget, tincidunt diam. Vestibulum auctor purus ac mattis aliquam. Mauris finibus id sapien vitae tempor. Phasellus ac est ut nisl lobortis faucibus. In ullamcorper congue turpis, vel placerat ligula pellentesque at. Quisque mollis magna eget finibus bibendum. Vivamus sed sapien id justo volutpat gravida. Proin euismod vestibulum mattis. " "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In eu arcu urna. Etiam ullamcorper lorem turpis, id vulputate sapien ultrices in. Morbi eget neque porta, sagittis dolor eget, tincidunt diam. Vestibulum auctor purus ac mattis aliquam. Mauris finibus id sapien vitae tempor. Phasellus ac est ut nisl lobortis faucibus. In ullamcorper congue turpis, vel placerat ligula pellentesque at. Quisque mollis magna eget finibus bibendum. Vivamus sed sapien id justo volutpat gravida. Proin euismod vestibulum mattis. ")



