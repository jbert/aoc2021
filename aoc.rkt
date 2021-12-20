#lang racket
(provide
 aoc-get-lines
 aoc-get-nums
 aoc-set-day
 aoc-set-test

 hash-key-add

 count-inc
 count-inc-foldl
 count-add

 list-nth
 
 half-cartesian-product
 )


(define day 0)
(define suffix "")

(define (aoc-set-day newday)
  (set! day newday))

(define (aoc-set-test p)
  (set! suffix (if p "-test" "")))

(define work-dir "/home/john/dev/jbert/aoc2021")

(define (hash-key-add h k v)
  (let ([current (hash-ref h k 0)])
    (hash-set h k (+ current v))))

(define (aoc-get-lines)
  (let ([data-filename (format "~a/data/day~a~a.txt" work-dir day suffix)])
    (file->lines data-filename)))

(define (aoc-get-nums)
  (map string->number (aoc-get-lines)))


;;; Add x to the nth position
(define (count-add counts n x)
  (if (= n 0)
      (cons (+ x (first counts))
            (rest counts))
      (cons (first counts)
            (count-add (rest counts) (- n 1) x))))


;;; increment the n'th position of the list
(define (count-inc counts n)
  (count-add counts n 1))

;;; foldl-friendly count-inc
(define (count-inc-foldl n counts)
  (count-inc counts n))


(define (list-nth l n)
  (if (empty? l)
      l
      (if (= n 0)
          (first l)
          (list-nth (rest l) (sub1 n)))))

(define (half-cartesian-product l)
  (define (helper a l)
    (map (lambda (b) (cons a b)) l))
  (if (empty? l)
      l
      (append
       (helper (first l) (rest l))
       (half-cartesian-product (rest l)))))