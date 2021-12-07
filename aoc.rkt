#lang racket
(provide
 aoc-get-lines
 aoc-get-nums
 aoc-set-day
 aoc-set-test

 (struct-out p)
 p-parse
 
 (struct-out ls)
 ls-parse
 ls-points
 ls-horiz?
 ls-vert?
 
 (struct-out grid)
 grid-make
 grid-inc
 grid-count
 
 count-inc
 half-cartesian-product
 )


(define day 0)
(define suffix "")

(define (aoc-set-day newday)
  (set! day newday))

(define (aoc-set-test p)
  (set! suffix (if p "-test" "")))

(define work-dir "/home/john/dev/jbert/aoc2021")

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

;;; increment the n'th position of the list
(define (count-inc counts n)
  (if (= n 0)
      (cons (add1 (first counts))
            (rest counts))
      (cons (first counts)
            (count-inc (rest counts) (- n 1)))))

(struct grid (width height cells) #:transparent)

(define (grid-make w h)
  (grid w
        h
        (for/list ([i (in-range h)])
          (make-list w 0))))

(define (grid-inc p g)
  (let ([new-cells (for/list ([row (grid-cells g)]
                              [j (in-range (grid-height g))])
                     (if (= j (p-y p))
                         (count-inc row (p-x p))
                         row))])
    (struct-copy grid g [cells new-cells])))

(define (grid-count g f)
  (apply + (for/list ([row (grid-cells g)])
             (count f row))))
    

; --------------


(define (half-cartesian-product l)
  (printf "l is ~a\n" l)
  (define (helper a l)
    (map (lambda (b) (cons a b)) l))
  (if (empty? l)
      l
      (append
       (helper (first l) (rest l))
       (half-cartesian-product (rest l)))))