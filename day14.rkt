#lang racket
(require "aoc.rkt")

(aoc-set-day 14)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define polymer-template (string->list (first lines)))
(define (parse-rule l)
  (string-split l " -> "))

(define rules (map parse-rule (cddr lines)))

; We store each template pair as a count
(define (template->polymer t)
  (foldl (lambda (k h) (hash-key-add h k 1))
         (hash)
         (map list->string (map list
                                (take t (sub1 (length t)))
                                (rest t)))))

;(template->polymer polymer-template)

(struct update (sub adds) #:transparent)

(define (polymer-apply-update u orig-poly)
  (let* ([sub (update-sub u)]
         [k (first sub)]
         [v (second sub)]
         [hr (hash-key-add orig-poly k (- v))])
    (foldl (lambda (add h)
             (let* ([k (car add)]
                    [v (cdr add)])
               (hash-key-add h k v)))
           hr
           (update-adds u))))
  
;(define (polymer-apply-updates us p)
;  (printf "US: ~v\n" us)
;  (printf "P: ~v\n" p)
;  (foldl polymer-apply-update
;         p
;         us))

;(define u (update "NC" (list (cons "AB" 5) (cons "BC" 6))))

(define the-template (template->polymer polymer-template))
;(polymer-apply-updates (list u) the-template)

(define (key-to-chars k)
  (let* ([cs (string->list k)]
         [l (first cs)]
         [r (second cs)])
    (cons l r)))
  
(define (rule->update rule p)
  (let* ([k (car rule)]
         [v (hash-ref p k 0)]
         [insert (first (string->list (second rule)))]
         [cs (string->list k)]
         [l (first cs)]
         [r (second cs)]
         [addl (list->string (list l insert))]
         [addr (list->string (list insert r))])
    (update (list k v)
            (list (cons addl v) (cons addr v)))))
    
(define (polymer-apply-rules rs p)
  (let* ([updates (map (lambda (r) (rule->update r p))
                       rs)])
;    (printf "Updates: ~v\n" updates)
    (foldl polymer-apply-update
           p
           updates)))

(define (run n p rs)
  (if (= n 0)
      p
      (run (sub1 n)
           (polymer-apply-rules rs p)
           rs)))
  
;(polymer-apply-rules rules
;                     (polymer-apply-rules rules the-template))
(define after-10 (run 10 the-template rules))

(define (polymer-length p)
  (add1 (apply + (hash-values p))))

(printf "After 10, length ~v\n" (polymer-length after-10))
(define (polymer-element-counts p)
  (let* ([start-char (first polymer-template)]
         [end-char (last polymer-template)]
         [sums (apply append (for/list ([(k v) p])
                               (let* ([cs (key-to-chars k)]
                                      [l (car cs)]
                                      [r (cdr cs)])
                                 (list (cons l v) (cons r v)))))]
         [hsums (foldl (lambda (s h)
                         (let* ([k (car s)]
                                [v (cdr s)]
                                [current (hash-ref h k 0)])
                           (hash-set h k (+ current v))))
                       (hash)
                       sums)])
    (for/hash ([(k v) hsums])
      (values k (ceiling (/ v 2))))))

(polymer-element-counts after-10)

(let* ([vs (hash-values (polymer-element-counts after-10)
)]
       [most (apply max vs)]
       [least (apply min vs)])
  (printf "Part 1: ~v\n" (- most least)))