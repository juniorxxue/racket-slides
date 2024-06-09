#lang slideshow

(require (except-in pict pin-over pin-under cellophane file-icon)
         racket/contract
         racket/match
         slideshow/base
         slideshow/code
         slideshow/text
         pict/conditional
         threading
         racket/draw
         (prefix-in slideshow: slideshow/base)
         "color.rkt"
         "pict.rkt"
         "util.rkt")

(provide pin-balloon)

(define shape-bg-color (make-color #xf7 #xf7 #xf7))
(define shape-border-color (make-color #xd8 #xd8 #xd8))

(define (balloon w h
                 #:corner-radius [r 25]
                 #:spike-size [s 25]
                 #:spike-position [s-posn 0]
                 #:color [c shape-bg-color]
                 #:border-color [bc shape-border-color]
                 #:border-width [bw 2])
  (define 2s (* 2 s))
  (define s-offset (* s-posn (- w 2s)))
  (define tl-r (min r s-offset))
  (define tr-r (min r (- (- w 2s) s-offset)))
  
  (define path (new dc-path%))
  ; draw the left side of the top
  (send path arc 0 0 tl-r tl-r (turns 1/2) (turns 1/4) #f)
  (send path line-to s-offset 0)

  ; draw the spike
  (send path line-to (+ s-offset s) (- s))
  (send path line-to (+ s-offset 2s) 0)

  ; draw the right side of the top
  (send path line-to (- w tr-r) 0)
  (send path arc (- w tr-r) 0 tr-r tr-r (turns 1/4) 0 #f)
  (send path line-to w (- h r))

  ; draw the rest
  (send path arc (- w r) (- h r) r r 0 (turns -1/4) #f)
  (send path line-to r h)
  (send path arc 0 (- h r) r r (turns 3/4) (turns 1/2) #f)
  (send path close)

  (~> (dc (λ (dc dx dy) (send dc draw-path path dx (+ dy s))) w (+ h s))
      (inset 0 (- s) 0 0)
      (adjust-pen #:color bc #:width bw)
      (maybe-colorize c)))

(define (wrap-balloon p
                      #:spike-size [s 25]
                      #:spike-position [s-posn 0]
                      #:padding [padding 25])
  (cc-superimpose (balloon (+ (pict-width p) (* padding 2))
                           (+ (pict-height p) (* padding 2))
                           #:spike-size s #:spike-position s-posn)
                  p))

(define (pin-balloon base path find p
                     #:spike-size [s 25]
                     #:spike-position [s-posn 0]
                     #:show? [show? #t])
  (define balloon-p (~> (wrap-balloon p #:spike-size s #:spike-position s-posn)
                        (show show?)))
  (define s-offset (* s-posn (- (pict-width balloon-p) (* 2 s))))
  (pin-over base path (adjust-find find (- (+ s-offset s)) s) balloon-p))