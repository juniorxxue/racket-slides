#lang slideshow

(require slideshow/code
         racket/draw
         "color.rkt"
         "pict.rkt"
         "slideshow.rkt"
         (only-in slideshow [current-font-size current-text-size]))

(provide (all-defined-out))

(current-main-font "Concourse T3")
(current-code-font "Fira Code")
(current-text-size 40)
(get-current-code-font-size
 (thunk (round (* (current-text-size) 9/10))))

(define transparent (make-color 0 0 0 0))
(define background-color (make-color #xf9 #xf9 #xf9))
(define code-background-color (make-color #xFA #xE9 #xE6))
(define code-border-color (make-color #xE8 #xCC #xC8))
(define text-plain-color (make-color #x40 #x40 #x40))
(define text-secondary-color (make-color #x60 #x60 #x60))
(define text-tertiary-color (make-color #xa0 #xa0 #xa0))
(define tertiary-color (make-color #xc0 #xc0 #xc0))
(define text-secondary-highlight-color (make-color #xb5 #xd0 #xff))
(define text-tertiary-highlight-color (make-color #xc7 #xff #xcd))
(define text-error-color (make-color #x8a #x16 #x16))
(define interaction-output-color (make-color #x96 #x00 #x96))
(define interaction-result-color (->color% (hsv 23/36 0.77 0.74)))
(define shadow-color (make-color #x70 #x30 #x30))

(define shape-bg-color (make-color #xf7 #xf7 #xf7))
(define shape-border-color (make-color #xd8 #xd8 #xd8))

(current-highlight-color (make-color #xff #xd4 #xd1))

(current-text-color text-plain-color)
(current-base-color text-tertiary-color)
(current-keyword-color text-plain-color)
(current-id-color (make-color #x37 #x50 #x73))
(current-literal-color (make-color #x87 #x4f #x37))
(current-comment-color (make-color #x9E #x55 #x55))
(define current-constructor-color (make-parameter (make-color #x59 #x37 #x73)))
(let ([super (current-token-class->color)])
  (current-token-class->color
   (λ (c) (case c
            [(constructor) (current-constructor-color)]
            [else (super c)]))))

(define (c:plain p) (colorize p text-plain-color))

(define (flavor-highlight-color flavor)
  (->color% (match flavor
              [0 (hsv 0.01 0.18 1.0)]
              [1 (hsv 0.35 0.22 1.0)]
              [2 (hsv 0.60 0.29 1.0)])))

(define (flavor-text-color flavor)
  (->color% (match flavor
              [0 (hsv 0.01 0.80 0.85)]
              [1 (hsv 0.35 0.60 0.60)]
              [2 (hsv 0.60 0.80 0.80)])))