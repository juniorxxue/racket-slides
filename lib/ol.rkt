#lang at-exp curly-fn slideshow

(require (except-in pict pin-over pin-under cellophane file-icon)
         racket/contract
         racket/match
         
         (only-in slideshow [current-font-size current-text-size])
         slideshow/code
         slideshow/text
         pict/conditional
         threading
         racket/draw
         (prefix-in slideshow: slideshow/base)
         "slideshow.rkt"
         "color.rkt"
         "pict.rkt"
         "theme.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (ol #:make-num [make-num (λ (n) (t (~a n ".")))]
            #:sep [sep (em 3/4)]
            #:spacing [spacing (current-para-spacing)]
            #:stage [stage #f]
            . elems)
  (define num-picts (for/list ([i (in-range (length elems))])
                      (c:plain (make-num (add1 i)))))
  (define max-num-width (apply max (map pict-width num-picts)))
  (~>> (for/list ([elem (in-list elems)]
                  [num (in-list num-picts)])
         (htl-append sep (indent #:by (- max-num-width (pict-width num)) num) elem))
       (apply paras #:spacing spacing #:stage stage)))

(define (circled n #:inline? [inline? #f])
  (define s (current-font-size))
  (define w (* s 3/50))
  (define offset (match n
                   [1 -0.04]
                   [2 -0.02]
                   [3 -0.06]
                   [4  0.02]))
  (define p (~> (with-font "Concourse C4" (t (~a n)))
                (inset 0 0 (* s offset) 0)))
  (define p* (scale p 0.75))
  (define g (ghost p))
  (define d (- (pict-height p*) (* s 1/25)))
  (~> ((if inline? ctl-superimpose cc-superimpose) g p*)
      (refocus p*)
      (cc-superimpose (circle d #:border-width w))
      (refocus g)
      (inset (/ (- (pict-height p*)
                   (pict-width p*))
                2)
             0)))