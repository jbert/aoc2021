#lang racket
(require "aoc.rkt")

(aoc-set-day 4)
(aoc-set-test #f)

(define lines (aoc-get-lines))
(define the-number-order (map string->number (string-split (first lines) ",")))
(define board-size 5)
(define (skip-rest l n)
  (if (= n 0)
      l
      (skip-rest (rest l) (- n 1))))
(define (read-board-lines lines)
  (if (= (length lines) 0)
      '()
      (cons (take (rest lines) board-size)
            (read-board-lines (skip-rest lines (add1 board-size))))))

(struct board (rows num->pos rowcount colcount d1count d2count called-nums) #:transparent)

(define (on-d1 p)
  (= (car p) (cdr p)))

(define (on-d2 p)
  (= board-size (+ (car p) (cdr p))))

(define (count-list l)
  (define (helper l i)
    (if (empty? l)
        l
        (cons (cons i (first l))
              (helper (rest l) (add1 i)))))
  (helper l 0))

(define (build-row-pos rows)
  (define (tag-col j tagged-num)
    (let* ([i (car tagged-num)]
           [num (cdr tagged-num)])
      (cons (cons i j) num)))
  (define (tag-row row)
    (let* ([j (first row)]
           [tagged-cols (rest row)])
           (map (lambda (tagged-num) (tag-col j tagged-num))
                tagged-cols)))
  (let ([numbered-rows (count-list (map count-list rows))])
    (map tag-row numbered-rows)))
         

(define (parse-board board-lines)
  (let* ([rows (map (lambda (l) (map string->number (string-split l " " #:repeat? #t)))
                    board-lines)]
         [tagged-rows (apply append (build-row-pos rows))]
         [num->pos (apply hash (apply append (map (lambda (cell) (list (cdr cell) (car cell)))
                                                  tagged-rows)))])
    (board rows
           num->pos
           (make-list board-size 0)
           (make-list board-size 0)
           0
           0
           '())))
    

;(define p (hash-ref (board-num->pos (first boards)) 45 #f))

(define (board-find b num)
  (hash-ref (board-num->pos b) num #f))

(define (add-count counts n)
  (if (= n 0)
      (cons (add1 (first counts))
            (rest counts))
      (cons (first counts)
            (add-count (rest counts) (- n 1)))))

(define (board-process b num)
  (if (board-won b)
      b
      (let ([p (board-find b num)])
        (if (equal? p #f)
            b
            (let ([j (cdr p)]
                  [i (car p)]
                  [d1inc (if (on-d1 p) 1 0)]
                  [d2inc (if (on-d2 p) 1 0)])
              (struct-copy board b
                           [rowcount (add-count (board-rowcount b) i)]
                           [colcount (add-count (board-colcount b) j)]
                           [d1count (+ (board-d1count b) d1inc)]
                           [d2count (+ (board-d1count b) d2inc)]
                           [called-nums (cons num (board-called-nums b))]))))))

(define (board-won b)
  (define (row-has-winner r)
    (not (empty? (filter (lambda (c) (= c board-size)) r))))
  (or (row-has-winner (board-rowcount b))
      (row-has-winner (board-colcount b))
      (= (board-d1count b) board-size)
      (= (board-d2count b) board-size)))




(define (call-number bs num)
  (map (lambda (b) (board-process b num)) bs))

(define (call-numbers bs nums)
  (let ([winners (filter board-won bs)])
    (if (empty? winners)
        (call-numbers (call-number bs (first nums)) (rest nums))
        winners)))


(define the-boards (map parse-board (read-board-lines (rest lines))))
(define winning-board
  (first (call-numbers the-boards the-number-order)))

(define (board-last-called b)
  (first (board-called-nums b)))

(define (board-not-called b)
  (let* ([board-nums (list->set (hash-keys (board-num->pos b)))]
         [called-nums (list->set (board-called-nums b))]
         [uncalled-nums (set-subtract board-nums called-nums)])
        (set->list uncalled-nums)))

(define (board-score b)
  (* (board-last-called b)
     (apply + (board-not-called b))))
(printf "Winning board ~a\n" winning-board)
(printf "Board score ~a \n"  (board-score winning-board))

; ---- part2 ----
(define (call-numbers-p2 bs nums last-num)
  (let* ([num-boards (length bs)]
         [winners (filter board-won bs)]
         [current-num (first nums)])
    (if (< (length winners) num-boards)
        (call-numbers-p2 (call-number bs current-num) (rest nums) current-num)
        (filter (lambda (b)
                  (= (board-last-called b) last-num))
                winners))))

(define winning-board-p2
  (first (call-numbers-p2 the-boards the-number-order 0)))

(printf "Winning board P2 ~a\n" winning-board-p2)
(printf "Board score ~a \n"  (board-score winning-board-p2))
