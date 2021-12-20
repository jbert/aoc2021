#lang racket
(provide diag-low-sum-growing
         diag-low-sum-shrinking
         )
(require "aoc.rkt")

(aoc-set-day 15)
(aoc-set-test #f)

(define lines (aoc-get-lines))
(define rows (map (lambda (l) (map string->number
                                   (map string (string->list l))))
                    lines))

(define (diag-low-sum-growing a b)
  ;(printf "DLSG ~v ~v\n" a b)
  (define (helper x y)
    (if (< (length x) 2)
        (list (+ (first x) (first y)))
        (let ([p1 (first x)]
              [p2 (second x)])
          (cons (+ (min p1 p2)
                   (first y))
                (helper (rest x) (rest y))))))
  (cons (+ (first a) (first b)) (helper a (rest b))))

(define (pair-min a)
  (map (lambda (x y) (min x y))
       (take a (sub1 (length a)))
       (rest a)))

(define (list-sum a b)
  (if (empty? a)
      '()
      (cons (+ (first a) (first b))
            (list-sum (rest a) (rest b)))))

(define (diag-low-sum-shrinking a b)
  (list-sum (pair-min a) b))

(define (get-growing rs)
  (define (helper diags old-rows todo)
    (if (empty? todo)
        diags
        (let* ([rows (cons (first todo) old-rows)]
               [next-todo (rest todo)]
               [next-diag (map first rows)]
               [next-diags (cons next-diag diags)]
               [next-rows (map rest rows)])
          (helper next-diags next-rows next-todo))))
  (reverse (helper '() '() rs)))

(define (get-shrinking rs)
  (rest (reverse (get-growing (reverse (map reverse rs))))))

(define (rows->diags rs)
    (let* ([diags-g (get-growing rs)]
           [diags-s (get-shrinking rs)]
           [low-sum-g (foldl (lambda (a b) (diag-low-sum-growing b a))
                             (first diags-g)
                             (rest diags-g))]
           [low-sum-s (foldl (lambda (a b) (diag-low-sum-shrinking b a))
                             low-sum-g
                             diags-s)])
      (printf "low-sum-g ~v\n" low-sum-g)
      (printf "low-sum-s ~v\n" (first low-sum-s))
      (printf "diags-g ~v\n" diags-g)
      (printf "diags-s ~v\n" diags-s)
      (- (first low-sum-s) (first (first rs)))))

(printf "Part 1: ~v\n" (rows->diags rows))