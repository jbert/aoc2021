#lang racket
(require "aoc.rkt")
(require "graph.rkt")
(provide find-paths
         find-paths-fan-out
         banned-record-visit
         banned-set
;         find-paths-has-one-step
         ) ; For testing

(aoc-set-day 12)
(aoc-set-test #t)

;(define lines (aoc-get-lines))
(define lines (list
               "dc-end"
               "HN-start"
               "start-kj"
               "dc-start"
               "dc-HN"
               "LN-dc"
               "HN-end"
               "kj-sa"
               "kj-HN"
               "kj-dc"
               ))

(define the-graph (graph-make lines))
(printf "Points: ~a\n" (graph-points the-graph))
(printf "Edges: ~a\n" (graph-edges the-graph)) 

(define (big? p)
  (let ([s (symbol->string p)])
    (equal? (string-upcase s)
            s)))

#;(define (find-paths-has-one-step g from to)
  (let* ([one-step (cons from to)]
         [new-g (graph-remove-edge g one-step)]
         [path-list (find-paths new-g from to)]
         [pl-with-step (cons (list one-step) path-list)])
    pl-with-step))

(define (find-paths-fan-out g from to banned)
  (if (set-member? banned to)
      '()
      (let* ([adj (list->set (graph-adjacent g from))]
             [visitable (set-subtract adj banned)]
             [direct-paths (if (graph-has-edge? g (cons from to))
                               (list (list (cons from to)))
                               '())])
;        (printf "\nF: ~a B: ~a V: ~a\n" from banned visitable)
        (append*
         direct-paths
         (for/list ([next-from (set->list visitable)])
           (let* ([step (cons from next-from)]
                  [next-banned (if (big? from)
                                   banned
                                   (set-add banned from))]
                  [path-list (find-paths-fan-out g next-from to next-banned)]
                  [pl-with-prefix (map (lambda (path) (cons step path))
                                       path-list)])
;             (printf "\nNB: ~a\n" next-banned)
;             (printf "next-from: ~a\n" next-from)
;             (printf "pl ~a\n" path-list)
;             (printf "PLWP: ~a\n" pl-with-prefix)
             pl-with-prefix))))))

(define (find-paths g from to)
  (find-paths-fan-out g from to (set)))
  
(printf "There are ~a paths\n" (length (find-paths the-graph 'start 'end)))


(define (find-paths-fan-out-p2 g from to banned)
  (if (banned-is-banned? banned to)
      '()
      (let* ([adj (list->set (graph-adjacent g from))]
             [visitable (set-subtract adj (banned-set banned))]
             [direct-paths (if (graph-has-edge? g (cons from to))
                               (list (list from to))
                               '())])
;        (printf "\nF: ~a B: ~a V: ~a\n" from banned visitable)
        (append*
         direct-paths
         (for/list ([next-from (set->list visitable)])
           (let* (;[step (cons from next-from)]
                  [next-banned (banned-record-visit from banned)]
                  [path-list (find-paths-fan-out-p2 g next-from to next-banned)]
                  [pl-with-prefix (map (lambda (path) (cons from path))
                                       path-list)])
;             (printf "\nNB: ~a\n" next-banned)
;             (printf "next-from: ~a\n" next-from)
;             (printf "pl ~a\n" path-list)
;             (printf "PLWP: ~a\n" pl-with-prefix)
             pl-with-prefix))))))

(define (banned-record-visit point b)
  (let* ([current (hash-ref b point 0)]
        [next (add1 current)]
        [h (hash-set b point next)])
    (if (or (big? point)
            (< next 2))
        h
        (hash-set h 'done-two-visit #t))))

(define (banned-set b)
  (list->set (filter (lambda (pt) (and (not (equal? pt 'done-two-visit))
                                       (banned-is-banned? b pt)))
                     (hash-keys b))))

(define (banned-is-banned? b pt)
  (let ([c (hash-ref b pt 0)]
        [done-two-visit (hash-ref b 'done-two-visit #f)])
    (cond
      [(big? pt) #f]
      [(and (or (equal? pt 'start)
                (equal? pt 'end))
            (>= c 1))]
      [else (>= c (if done-two-visit 1 2))])))

(define (find-paths-p2 g from to)
  (find-paths-fan-out-p2 g from to (hash)))

(printf "P2: There are ~a paths\n" (length (find-paths-p2 the-graph 'start 'end)))
(find-paths-p2 the-graph 'start 'end)