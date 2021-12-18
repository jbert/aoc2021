#lang racket
(require rackunit "day13.rkt")
(define tcs '(
              ((6 2) (#f 5) (4 2) "vert fold")
              ((2 6) (#t 5) (2 4) "horiz fold")
              ))
  

(define (tc-run tc)
  (match tc
      [(list (list x y) (list h? v) (list xe ye) msg)
       (let ([got (paper-fold-apply (paper-fold h? v) (p x y))]
             [e (p xe ye)])
         (check-equal? got e msg))]))
    
(for ([tc tcs])
  (printf "TC: ~a\n" tc)
  (tc-run tc))
