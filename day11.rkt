#lang racket
(require "aoc.rkt")

(aoc-set-day 11)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define the-grid (aoc-get-numgrid))

(define (grid-apply-flash flash g)
  (let* ([neighbours (grid-all-neighbour-points g flash)])
         (foldl grid-inc g neighbours)))

(define (new-flashes g seen)
;  (printf "NF seen ~a\n" seen)
  (grid-filter (lambda (p v) (and (not (set-member? seen p))
                                  (>= v 10)))
               g))

(define (remove-flashed g)
  (grid-map g (lambda (v) (if (> v 9)
                            0
                            v))))

(define (tick g)
  (define (helper g flashed-this-turn)
    (let* ([flashes (new-flashes g flashed-this-turn)])
;      (printf "Flashes: ~a\n" (sort flashes p<=?))
;      (pretty-print g)
;      (when (> (length flashes) 40)
;        (error "Jb"))
      (if (empty? flashes)
          (cons (length (set->list flashed-this-turn))
                (remove-flashed g))
          (let ([new-flashed-this-turn (set-union flashed-this-turn
                                                  (list->set flashes))])
            (helper (foldl grid-apply-flash g flashes) new-flashed-this-turn)))))
  ; Add 1 to everything, then iterate
  (helper (grid-map g add1) (set)))

(define (tick-n-times g n)
  (define (helper g n num-flashes) 
    (if (= n 0)
        (cons num-flashes g)
        (let* ([result (tick g)]
               [new-num-flashes (car result)]
               [new-g (cdr result)])
          (helper new-g (sub1 n) (+ num-flashes new-num-flashes)))))
  (helper g n 0))

(printf "After 100 steps, ~a flashes\n" (car (tick-n-times the-grid 100)))

(define (all-flash? g)
  (let ([non-zero-cells (grid-filter (lambda (p v) (not (= v 0))) g)])
        (= (length non-zero-cells) 0)))

(define (find-flash g)
  (define (helper g num-tick)
    (if (all-flash? g)
        num-tick
        (helper (cdr (tick g)) (add1 num-tick))))
  (helper g 0))

(printf "Flash after ~a ticks\n" (find-flash the-grid))