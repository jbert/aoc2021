#lang racket
(require "aoc.rkt")

(aoc-set-day 2)
(aoc-set-test #f)
(define lines (aoc-get-lines))

; horiz, depth as a cons pair
(define pos (cons 0 0))

(define commands (map string-split lines))

(define (do-command command pos)
  (let ([h (car pos)]
        [d (cdr pos)]
        [action (car command)]
        [amount (string->number (car (cdr command)))])
    (case action
      [("forward") (cons (+ h amount) d)]
      [("down") (cons h (+ d amount))]
      [("up") (cons h (- d amount))]
      [else (error (format "Unrecognised action: ~a" action))])))

(define last-pos (foldl do-command pos commands))

(printf "Last pos ~a: number ~a\n" last-pos (* (car last-pos) (cdr last-pos)))

; Rich pos (horiz, depth, aim as a l
(define rpos (list 0 0 0))

(define (do-rcommand command rpos)
  (let ([h (first rpos)]
        [d (second rpos)]
        [aim (third rpos)]
        [action (first command)]
        [amount (string->number (second command))])
    (case action
      [("forward") (list (+ h amount) (+ d (* aim amount)) aim)]
      [("down") (list h d (+ aim amount))]
      [("up") (list h d (- aim amount))]
      [else (error (format "Unrecognised action: ~a" action))])))


(define last-rpos (foldl do-rcommand rpos commands))

(printf "Last pos ~a: number ~a\n" last-rpos (* (first last-rpos) (second last-rpos)))
