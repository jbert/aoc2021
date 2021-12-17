#lang racket
(require "graph.rkt")
(require rackunit "day12.rkt")

(define (pls-e? g e s)
  (check-equal? (list->set g) (list->set e) s))

(define g2 (graph-make '("a-b")))
(pls-e? (find-paths g2 'a 'b)
              '(((a . b)))
              "Simplest path list")

(define g3 (graph-make '("a-c" "c-b")))
(pls-e? (find-paths g3 'a 'b)
              '(((a . c) (c . b)))
              "Two step")

(define g4 (graph-make '("a-c" "a-d" "d-b" "c-b")))
(pls-e? (find-paths g4 'a 'b)
              '(((a . c) (c . b))
                ((a . d) (d . b)))
              "Two path")

(define g5 (graph-make '("a-c" "c-d" "d-b")))
(pls-e? (find-paths g5 'a 'b)
              '(((a . c) (c . d) (d . b)))
              "Three step")

(define g6 (graph-make '("a-c" "a-d" "c-d" "d-b" "c-b")))
(pls-e? (find-paths g6 'a 'b)
              '(((a . c) (c . d) (d . b))
                ((a . c) (c . b))
                ((a . d) (d . b))
                ((a . d) (d . c) (c . b)))
              "Cross (four path)")

;(define g6 (graph-make ("a-c" "a-d" "c-d"