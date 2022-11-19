#lang racket
(require "aoc.rkt")
(require "pts.rkt")
(aoc-set-day 17)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define (parse-range r)
  (let* ([bits (string-split r "..")])
    (map string->number bits)))

(define (line->rect l)
  (let* ([ranges (string-split (string-trim l "target area: x=") ", y=")]
         [xr (parse-range (first ranges))]
         [yr (parse-range (second ranges))]
         [bl (p (first xr) (first yr))]
         [tr (p (second xr) (second yr))])
    (rect bl tr)))
         
    
(define target-rect (line->rect (first lines)))
target-rect

(define x-pos-range (rect-xrange target-rect))

; for an initial x velocity N, we travel 1+2+3+...+N = (N-1) * N * 2
; Just iterate to find
(define (find-xmin N)
  (define (helper n)
    (if (>= (/ (* n (sub1 n)) 2) N)
        n
        (helper (add1 n))))
  (sub1 (helper 0)))

(define xmin (find-xmin (car x-pos-range)))
; For an inital velocity of N+1, we will never hit N
(define xmax (cdr x-pos-range))

(struct moving (pos vel) #:transparent)

(define (moving-tick m)
  (let* ([pos (moving-pos m)]
         [vel (moving-vel m)]
         [new-x-vel (- (p-x vel) (sgn (p-x vel)))]
         [new-y-vel (sub1 (p-y vel))]
         [new-pos (p-add pos vel)])
;    (printf "~v : ~v\n" new-pos (p new-x-vel new-y-vel))
    (moving new-pos (p new-x-vel new-y-vel))))
    
(define (simulate target m)
;  (printf "Simulate: ~v\n" m)
  (define (helper target m y-max)
;    (printf "M: ~v\n" m)
    (let* ([pos (moving-pos m)]
           [vel (moving-vel m)]
           [new-y-max (max y-max (p-y pos))])
      (if (rect-within? target pos)
          (begin
;            (printf "Hit! ~v\n" pos)
            (cons #t new-y-max))
          (if (or (> (p-x pos) (rect-xmax target))
                  (< (p-y pos) (rect-ymin target)))
              (begin
;                (printf "Miss! ~v\n" pos)
                (cons #f new-y-max))
              (helper target (moving-tick m) new-y-max)))))
  (helper target m -10000000))


;(simulate target-rect (moving (p 0 0) (p xmin ymin)))

(define (find-successes target ylo yhi)
  (define (helper y results)
    (if (= yhi y)
        results
        (let* ([start-vels (for/list ([x (in-range xmin (add1 xmax))])
                             (p x y))]
               [successes (map cdr (filter (lambda (pair) (car pair))
                                           (map (lambda (v) (simulate target (moving (p 0 0) v)))
                                                start-vels)))]
               [num-successes (length successes)])
          (helper (add1 y)
                  (if (> num-successes 0)
                      (cons (list y num-successes successes) results)
                      results)))))
  (helper ylo '()))


(define successes (find-successes target-rect
                                 (rect-ymin target-rect)
                                 (abs (* 10 (rect-ymax target-rect)))))

(let ([highest (first successes)])
  (printf "Part1: ~v\n" (first (third highest))))

(printf "Part2: ~v\n" (apply + (map second successes)))

