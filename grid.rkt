#lang racket
(provide
 (struct-out grid)
 grid-make
 grid-new
 grid-inc
 grid-count
 grid-neighbour-points
 grid-neighbour-get
 grid-within?
 grid-get
 grid-for-each)

(require "aoc.rkt")
(require "pts.rkt")


 
(struct grid (width height cells) #:transparent)

(define (grid-make w h)
  (grid w
        h
        (for/list ([i (in-range h)])
          (make-list w 0))))

(define (grid-new cells)
  (let* ([h (length cells)]
         [w (length (first cells))])
    (grid w h cells)))

(define (grid-inc p g)
  (let ([new-cells (for/list ([row (grid-cells g)]
                              [j (in-range (grid-height g))])
                     (if (= j (p-y p))
                         (count-inc row (p-x p))
                         row))])
    (struct-copy grid g [cells new-cells])))

(define (grid-get g p)
  (when (not (grid-within? g p))
    (error (format "~a not within grid" p)))
  (let ([row (list-nth (grid-cells g) (p-y p))])
    (list-nth row (p-x p))))

(define (grid-count g f)
  (apply + (for/list ([row (grid-cells g)])
             (count f row))))

(define (grid-within? g p)
  (let ([x (p-x p)]
        [y (p-y p)])
    (and (>= x 0)
         (>= y 0)
         (< x (grid-width g))
         (< y (grid-height g)))))

(define (grid-neighbour-points g p)
  (let ([neighbours (p-neighbours p)])
    (filter (lambda (p) (grid-within? g p))
            neighbours)))

(define (grid-neighbour-get g p)
  (map (lambda (x) (grid-get g x))
       (grid-neighbour-points g p)))

(define (grid-for-each g f)
  (for/list ([row (grid-cells g)]
             [y (in-range 0 (add1 (grid-height g)))])
    (for/list ([v row]
               [x (in-range 0 (add1 (grid-width g)))])
      (f v (p x y)))))
 

; --------------
