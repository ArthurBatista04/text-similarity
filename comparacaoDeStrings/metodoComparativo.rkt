#lang racket
(require rackunit)
(require rackunit/text-ui)
(require math/statistics)
(require "jaccard.rkt")
(require "levenshtein.rkt")
(require memoize)
;Para a comparacao dos textos foi utilizado o site https://copyleaks.com/compare a fim de estipular uma porcentagem de plagio 
;Todos os textos plagiados foram retirados dos sites https://www.bowdoin.edu/dean-of-students/judicial-board/academic-honesty-and-plagiarism/examples.html e https://www.utc.edu/library/help/tutorials/plagiarism/examples-of-plagiarism.php
                                                                    ; exemplos de plagio direto 
; o autor A copiou um trechos "palavra por palavra" do autor B, como também incluiu uma passagem literal do autor B e não indicou que é uma citação direta. Nivel de plagio 80% ou 0.8
(define texto1AutorA "Long ago, when there was no written history, these islands were the home of millions of happy birds; the resort of a hundred times more millions of fishes, sea lions, and other creatures. Here lived innumerable creatures predestined from the creation of the world to lay up a store of wealth for the British farmer, and a store of quite another sort for an immaculate Republican government.")
(define texto1AutorB "In ages which have no record these islands were the home of millions of happy birds, the resort of a hundred times more millions of fishes, of sea lions, and other creatures whose names are not so common; the marine residence, in fact, of innumerable creatures predestined from the creation of the world to lay up a store of wealth for the British farmer, and a store of quite another sort for an immaculate Republican government.")
;a versão plagiada (autor A) usou exatamente a mesma redação do original (autor B) sem utilizar aspas ou indicá-lo ao longo do texto. Nivel de plágio 68% ou 0.68
(define texto2AutorA "Descartes is famous for being the first modern philosopher. He promoted a new concept of matter which allowed for the accounting of physical phenomena by way of mechanical explanations and an important connection between geometry and algebra, which allowed for the solving of geometrical problems by way of algebraic equations. However, he is best known for having written Meditations On First Philosophy, published in 1641, in which he provides a philosophical groundwork for the possibility of the sciences. ")
(define texto2AutorB "Descartes has been heralded as the first modern philosopher. He is famous for having made an important connection between geometry and algebra, which allowed for the solving of geometrical problems by way of algebraic equations. He is also famous for having promoted a new conception of matter, which allowed for the accounting of physical phenomena by way of mechanical explanations. However, he is most famous for having written a relatively short work, Meditationes de Prima Philosophia (Meditations On First Philosophy), published in 1641, in which he provides a philosophical groundwork for the possibility of the sciences.")
                                                                    ; exemplos de plagio em mosaico (paráfrase)
;o autor A emprestou, com pequenas variações, uma frase não citada do autor B, como também emprestou uma palavra-chave não reconhecida do autor B e não indicou a omissão de palavras com reticências. Nivel de plagio 31% ou 0.31
(define texto3AutorA "Only two years later, all these friendly Sioux were suddenly plunged into new conditions, including starvation, martial law on all their reservations, and constant urging by their friends and relations to join in warfare against the treacherous government that had kept faith with neither friend nor foe.")
(define texto3AutorB "In ages which have no record these islands were the home of millions of Contrast the condition into which all these friendly Indians are suddenly plunged now, with their condition only two years previous: martial law now in force on all their reservations; themselves in danger of starvation, and constantly exposed to the influence of emissaries from their friends and relations, urging them to join in fighting this treacherous government that had kept faith with nobody--neither with friend nor with foe.")
;o autor A mudou a redação, mas os conceitos são exatamente os mesmos do texto do autor B. Nivel de plagio 8% ou 0.08
(define texto4AutorA "There is no such thing as a 'good' oil spill. If the time and place are just right, even a small oil spill can cause damage to sensitive ecosystems. Further, spills can cause harm days, months, years, or even decades after they occur. Because of this, spills are usually broken into short-term (acute) and long-term (chronic) effects. Both of these types of harm must be addressed in ecosystem recovery: a controversial tactic that is often implemented immediately following an oil spill.")
(define texto4AutorB "No oil spill is entirely benign. Depending on timing and location, even a relatively minor spill can cause significant harm to individual organisms and entire populations. Oil spills can cause impacts over a range of time scales, from days to years, or even decades for certain spills. Impacts are typically divided into acute (short-term) and chronic (long-term) effects. Both types are part of a complicated and often controversial equation that is addressed after an oil spill: ecosystem recovery. ")

(define resultadoEsperadoList (list 0.8 0.68 0.31 0.08))

;; output do algoritimo de levenshtein
(define relacaoLevenshteinTexto1 (levenshtein texto1AutorA texto1AutorB))
(define relacaoLevenshteinTexto2 (levenshtein texto2AutorA texto2AutorB))
(define relacaoLevenshteinTexto3 (levenshtein texto3AutorA texto3AutorB))
(define relacaoLevenshteinTexto4 (levenshtein texto4AutorA texto4AutorB))
(define levenshteinList (list relacaoLevenshteinTexto1 relacaoLevenshteinTexto2 relacaoLevenshteinTexto3 relacaoLevenshteinTexto4))
;; output do algoritimo de jaccard
(define relacaoJaccardTexto1 (jaccard texto1AutorA texto1AutorB))
(define relacaoJaccardTexto2 (jaccard texto2AutorA texto2AutorB))
(define relacaoJaccardTexto3 (jaccard texto3AutorA texto3AutorB))
(define relacaoJaccardTexto4 (jaccard texto4AutorA texto4AutorB))
(define jaccardList (list relacaoJaccardTexto1 relacaoJaccardTexto2 relacaoJaccardTexto3 relacaoJaccardTexto4))

;; metodo comparativo
(define (correlacao resultadoEsperado resultadoDoAlgoritimo)
   (calculateCorrelacao resultadoEsperado resultadoDoAlgoritimo (average resultadoEsperado) (average resultadoDoAlgoritimo))
)
;; calculo do metodo comparativo 
(define (calculateCorrelacao resultadoEsperado resultadoDoAlgoritimo mediaEsperada mediaDoAlgoritmo) 
    (/ (calculateDiferenceBoth resultadoEsperado resultadoDoAlgoritimo mediaEsperada mediaDoAlgoritmo)  
       ( sqrt(* (calculateSquaredDiference resultadoEsperado mediaEsperada) (calculateSquaredDiference resultadoDoAlgoritimo mediaDoAlgoritmo)))
    ) 
)
;; calcula a diferenca entre os elementos dos resultados e suas medias multiplicado-os termo a termo
(define (calculateDiferenceBoth resultadoEsperado resultadoDoAlgoritimo mediaEsperada mediaDoAlgoritmo)
    (if (or (empty? resultadoEsperado) (empty? resultadoDoAlgoritimo))
        0.0
       (+ (* (- (first resultadoEsperado) mediaEsperada ) (- (first resultadoDoAlgoritimo) mediaDoAlgoritmo )) (calculateDiferenceBoth (rest resultadoEsperado) (rest resultadoDoAlgoritimo) mediaEsperada mediaDoAlgoritmo))
    )   
)
;;  calcula a diferenca de todos os elementos de resultado menos a media e eleva ao quadrado cada resultado
(define (calculateSquaredDiference resultado media)
    (if (empty? resultado)
        0.0
        (+ (expt (- (first resultado) media ) 2) (calculateSquaredDiference (rest resultado) media)) 
    )   
)

;; calcula a media de uma lista
(define (average l)
  (/ (foldr (lambda (x y) (+ x y)) 0 l) 
     (length l))
)

(define correlacaoDeLevenshtein (correlacao jaccardList resultadoEsperadoList))
(define correlacaoDeJaccard (correlacao levenshteinList resultadoEsperadoList))


(if (>= correlacaoDeJaccard correlacaoDeLevenshtein)
    (printf "Jaccard possui correlação maior com o resultado esperado, ~a, em comparação à ~a de Levenshtein " correlacaoDeJaccard correlacaoDeLevenshtein)
    (printf "Levenshtein possui correlação maior com o resultado esperado, ~a, em comparação à ~a de Jaccard " correlacaoDeLevenshtein correlacaoDeJaccard)
)
