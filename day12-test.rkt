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

(define banned-tcs '(
                     (()() "empty")
                     ((a)() "visit one")
                     ((a  b)() "one visit doesnt' ban any")
                     ((a a)(a) "visit one twice bans it")
                     ((A A)() "visit big twice bans nothing")
                     ((a a b)(a b) "visit a twice bans b after 1")
                     ((A A b)() "visit big twice doesn't bans b after 1")
                     ((A A b b)(b) "visit big twice bans b after 2")
                     ((A A b b a)(b a) "visit big twice bans b after 2 bans a after 1")
                     ((start)(start) "visit start once")
                     ))

(define (banned-tc-run tc)
  (let* ([pts (first tc)]
         [expected (second tc)]
         [s (third tc)]
         [b (hash)]
         [got (banned-set (foldl banned-record-visit b pts))])
    (check-equal? got (list->set expected) s)))

(for ([tc banned-tcs])
  (banned-tc-run tc))
