#lang racket
(provide
 aoc-get-lines
 aoc-get-nums
 aoc-set-day
 aoc-set-test

 hash-key-add

 (struct-out p)
 p-parse
 p-neighbours
 
 (struct-out ls)
 ls-parse
 ls-points
 ls-horiz?
 ls-vert?
 
 (struct-out grid)
 grid-make
 grid-new
 grid-inc
 grid-count
 grid-neighbour-points
 grid-neighbour-get
 grid-within?
 grid-get
 grid-for-each
 
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

; ----------
(struct p (x y) #:transparent)

(define (p-parse s)
  (let* ([bits (string-split s ",")]
         [nums (map string->number bits)])
    (p (first nums) (second nums))))

(define (p-add a b)
  (p (+ (p-x a) (p-x b))
     (+ (p-y a) (p-y b))))

(define (p-sub a b)
  (p (- (p-x a) (p-x b))
     (- (p-y a) (p-y b))))

(define p-north (p 0 -1))
(define p-east (p 1 0))
(define p-south (p 0 1))
(define p-west (p -1 0))

(define p-news (list p-north p-east p-west p-south))

(define (p-neighbours p)
  (map (lambda (x) (p-add p x))
         p-news))

; ----------
(struct ls (from to) #:transparent)

(define (ls-parse l)
  (let* ([bits (string-split l " ")]
         [from (p-parse (first bits))]
         [to (p-parse (third bits))])
    (ls from to)))

(define (ls-horiz? ls)
  (= (p-y (ls-from ls))
     (p-y (ls-to ls))))

(define (ls-vert? ls)
  (= (p-x (ls-from ls))
     (p-x (ls-to ls))))

(define (ls-diag? ls)
  (let* ([dp (p-sub (ls-to ls) (ls-from ls))])
    (= (abs (p-x dp))
       (abs (p-y dp)))))

(define (ls-horiz-points ls)
  (let* ([y (p-y (ls-from ls))]
        [xs (list (p-x (ls-from ls)) (p-x (ls-to ls)))]
        [from-x (apply min xs)]
        [to-x (apply max xs)])
    (for/list ([x (in-range from-x (add1 to-x))])
      (p x y))))

(define (ls-vert-points ls)
  (let* ([x (p-x (ls-from ls))]
        [ys (list (p-y (ls-from ls)) (p-y (ls-to ls)))]
        [from-y (apply min ys)]
        [to-y (apply max ys)])
    (for/list ([y (in-range from-y (add1 to-y))])
      (p x y))))

(define (ls-diag-points ls)
  (define (helper f t)
    (let* ([ylo (p-y f)]
           [yhi (p-y t)]
           [step (sgn (- yhi ylo))])
      (for/list ([x (in-range (p-x f) (add1 (p-x t)))]
                 [y (in-range ylo (+ yhi step) step)])
            (p x y))))
  (let ([f (ls-from ls)]
        [t (ls-to ls)])
    (if (< (p-x f) (p-x t))
        (helper f t)
        (helper t f))))
    


  
(define (ls-points ls)
  (if (ls-horiz? ls)
      (ls-horiz-points ls)
      (if (ls-vert? ls)
          (ls-vert-points ls)
          (if (ls-diag? ls)
              (ls-diag-points ls)
              (error (format "ls [~a] not horiz, vert or diag" ls))))))


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