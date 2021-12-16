#lang racket
(require "aoc.rkt")
(require "graph.rkt")

(aoc-set-day 12)
(aoc-set-test #t)

(define lines (aoc-get-lines))

(define g (graph-make lines))
(printf "Points: ~a\n" (graph-points g))
(printf "Edges: ~a\n" (graph-edges g)) 

(define (big? p)
  (equal? (string-upcase p)
          p))

(define (find-paths-has-one-step g from to)
  (let* ([one-step (cons from to)]
         [new-g (graph-remove-edge g one-step)]
         [path-list (find-paths new-g from to)]
         [pl-with-step (cons (list one-step) path-list)])
    pl-with-step))

(define (find-paths-fan-out from to)
  (for/list ([step (graph-from g from)])
    (let* ([next-g (graph-remove-edge g step)]
           [next-from (cdr step)])
      (let* ([path-list (find-paths next-g next-from to)]
             [pl-with-prefix (map (lambda (path) (cons step path))
                                  path-list)])
        pl-with-prefix))))

(define (find-paths g from to)
  (printf "\nJB ~a -> ~a\n" from to)
  (cond
    [(graph-has-edge? g (cons from to)) (find-paths-has-one-step g from to)]
    [(not (and (graph-has-point? g from)
               (graph-has-point? g to))) '()]
    [else (find-paths-fan-out from to)]))
  
(find-paths g 'start 'end)