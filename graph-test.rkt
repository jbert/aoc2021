#lang racket
(require rackunit "graph.rkt")

(define g2 (graph-make '("a-b")))

(check-equal? (list->set (graph-points g2)) (set 'a 'b) "g2 points")
(check-true (graph-has-edge? g2 '(a . b)) "Have edge")
(check-true (graph-has-edge? g2 '(b . a)) "Have reversed edge")
(check-equal? (list->set (graph-adjacent g2 'a)) (set 'b))
(check-equal? (list->set (graph-adjacent g2 'b)) (set 'a))

(define g3 (graph-make '("a-b" "b-c")))
(check-equal? (list->set (graph-points g3)) (set 'a 'b 'c) "g3 points")
(check-true (graph-has-edge? g3 '(a . b)) "g3: Have edge")
(check-true (graph-has-edge? g3 '(b . c)) "g3: Next edge")
(check-equal? (list->set (graph-adjacent g3 'a)) (set 'b))
(check-equal? (list->set (graph-adjacent g3 'b)) (set 'a 'c))
(check-equal? (list->set (graph-adjacent g3 'c)) (set 'b))

(define g3- (graph-remove-edge g3 '(a . b)))
(check-equal? (list->set (graph-points g3-)) (set 'b 'c) "g3- points")
(check-false (graph-has-edge? g3- '(a . b)) "g3-: Don't have edge")
(check-false (graph-has-edge? g3- '(b . a)) "g3-: Don't have revedge")
(check-true (graph-has-edge? g3- '(b . c)) "g3-: Next edge")
