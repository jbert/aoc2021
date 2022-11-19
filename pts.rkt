#lang racket
(provide
 (struct-out p)
 p-parse
 p-neighbours
 p-add
 p-sub

 (struct-out p3)
 p3-add
 p3-sub
 p3->list
 list->p3
 string->p3
 p3-sqlen
 
 (struct-out ls)
 ls-parse
 ls-points
 ls-horiz?
 ls-vert?

 (struct-out rect)
 rect-xrange
 rect-xmax
 rect-ymin
 rect-ymax
 rect-within?
 )

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

; -------------------

(struct rect (bl tr) #:transparent)

(define (rect-xrange r)
  (cons (p-x (rect-bl r))
        (p-x (rect-tr r))))

(define (rect-within? r pt)
  (let ([bl (rect-bl r)]
        [tr (rect-tr r)])
  (and (<= (p-x bl) (p-x pt) (p-x tr))
       (<= (p-y bl) (p-y pt) (p-y tr)))))

(define (rect-xmax r)
  (p-x (rect-tr r)))

(define (rect-ymin r)
  (p-y (rect-bl r)))

(define (rect-ymax r)
  (p-y (rect-tr r)))

; ----------

(struct p3 (x y z) #:transparent)

(define (p3->list p)
  (list (p3-x p) (p3-y p) (p3-z p)))

(define (list->p3 l)
  (p3 (first l) (second l) (third l)))

(define (string->p3 s)
  (let ([bits (string-split s ",")])
    (list->p3 (map string->number bits))))
         

(define (p3-add a b)
  (list->p3 (map + (p3->list a) (p3->list b))))

(define (p3-sub a b)
  (list->p3 (map - (p3->list a) (p3->list b))))

(define (p3-sqlen a)
  (apply + (map (lambda (x) (* x x)) (p3->list a))))