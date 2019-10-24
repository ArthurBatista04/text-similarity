#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "jaccard.rkt")
(require "levenshtein.rkt")
(require memoize)

; esses são os pares que serão comparados
(define textoIgualA "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam ultrices orci eget nisl venenatis cursus. Mauris fringilla dui quis congue dignissim.")
(define textoIgualB "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam ultrices orci eget nisl venenatis cursus. Mauris fringilla dui quis congue dignissim.")

(define textoDiferenteA "abcdefghijk")
(define textoDiferenteB "lmnopqrstuv")


(define levenshtein-testes
    (test-suite
        "teste de similaridade utilizando levenshtein"
        (check-equal? (levenshtein textoIgualA textoIgualB) 1.0) ;; caso sejam iguais
        (check-equal? (levenshtein textoDiferenteA textoDiferenteB) 0.0) ;; caso sejam totalmente diferentes
        (check-equal? (levenshtein " " " ") 1.0) ;;testes com strings vazias
    )
)

(define jaccard-testes
    (test-suite
        "teste de similaridade utilizando jaccard"
        (check-equal? (jaccard textoIgualA textoIgualB) 1.0) ;; caso sejam iguais
        (check-equal? (jaccard textoDiferenteA textoDiferenteB) 0.0) ;; caso sejam totalmente diferentes
        (check-equal? (jaccard " " " ") 1.0) ;; testes com strings vazias
    )
)

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes levenshtein-testes jaccard-testes)