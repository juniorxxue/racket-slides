#lang slideshow

(require slideshow/code
         threading)

(provide (all-defined-out))

(define (ppath-last path)
  (match path
    [#f             #f]
    [(list _ ... p) p]
    [(? pict? p)    p]))

; Combines picts by extending the last line, as determined by pict-last.
(define (line-append p0 . ps)
  (foldl (λ (p2 p1) (line-append/2 p1 p2)) p0 ps))
(define (line-append/2 p1 p2)
  (define draw-p1 (make-pict-drawer p1))
  (define draw-p2 (make-pict-drawer p2))
  ; find the rightmost point on the baseline of (pict-last p1)
  (define-values [last-x last-y] (rbl-find p1 (or (pict-last p1) p1)))

  ; figure out where we’ll place p2 relative to p1, since we want to align the
  ; descent line of (pict-last p1) with the ascent line of p2
  (define p2-y-relative (- last-y (pict-ascent p2)))
  ; if p2-y is negative, that means p2’s ascent peeks out above the top of p1,
  ; so compute how far we need to offset p1/p2 relative to the top of the new pict
  (define p1-y (if (negative? p2-y-relative) (- p2-y-relative) 0))
  (define p2-y (if (negative? p2-y-relative) 0 p2-y-relative))

  ; the x coordinate is simpler, since we don’t have to deal with ascent/descent,
  ; but it’s possible (though unlikely) that last-x is negative, in which case we
  ; want to do a similar adjustment
  (define p1-x (if (negative? last-x) (- last-x) 0))
  (define p2-x (if (negative? last-x) 0 last-x))

  ; compute rightmost point and bottommost point in the new pict’s bounding box
  (define w (max (+ p1-x (pict-width p1))
                 (+ p2-x (pict-width p2))))
  (define h (max (+ p1-y (pict-height p1))
                 (+ p2-y (pict-height p2))))
  ; same for uppermost ascent line and lowermost descent line
  (define a (min (+ p1-y (pict-ascent p1))
                 (+ p2-y (pict-ascent p2))))
  (define d (- h (max (+ p1-y (- (pict-height p1) (pict-descent p1)))
                      (+ p2-y (- (pict-height p2) (pict-descent p2))))))

  ; compute child offsets, which are weird because pict uses an inverted
  ; coordinate system, so these are relative to the lowermost point
  (define p1-dy (- h (+ p1-y (pict-height p1))))
  (define p2-dy (- h (+ p2-y (pict-height p2))))

  ; invent a new, totally unique pict to use as pict-last, in case (pict-last p2)
  ; already exists somewhere in the pict
  (define p2-last (or (ppath-last (pict-last p2)) p2))
  (define-values [p2-last-x p2-last-y] (lt-find p2 (or (pict-last p2) p2)))
  (define last-p (blank (pict-width p2-last)
                        (pict-height p2-last)
                        (pict-ascent p2-last)
                        (pict-descent p2-last)))
  
  (~> (dc (λ (dc dx dy)
            (draw-p1 dc (+ dx p1-x) (+ dy p1-y))
            (draw-p2 dc (+ dx p2-x) (+ dy p2-y)))
          w h a d)
      (struct-copy pict _
                   [children (list (make-child p1 p1-x p1-dy 1 1 0 0)
                                   (make-child p2 p2-x p2-dy 1 1 0 0)
                                   (make-child last-p
                                               (+ p2-x p2-last-x)
                                               (+ p2-dy p2-last-y)
                                               1 1 0 0))]
                   [last last-p])))

(define current-code-tt (make-parameter tt))

(define (haskell-code-tt str)
  (parameterize ([current-keyword-list '("let" "if" "then" "else" "catch" "prompt" "delimit" "->")])
    (codeblock-pict
     #:keep-lang-line? #f
     (string-append "#lang haskell-lexer/distinguish-constructors\n" str))))

(define (code . elems)
  (define (decode elem)
    (match elem
      ["\n" 'newline]
      [(? string?)
       ((current-code-tt) elem)]
      [_ elem]))

  (define H (tt "H"))
  (define blank-line (blank 0 (pict-height H) (pict-ascent H) (pict-descent H)))
  (define lines
    (for/fold ([lines (list blank-line)])
              ([elem (in-list elems)])
      (match (decode elem)
        ['newline (cons blank-line lines)]
        [p        (cons (line-append (first lines) p) (rest lines))])))
  (apply vl-append (current-code-line-sep) (reverse lines)))

(define (haskell . elems)
  (parameterize ([current-code-tt haskell-code-tt])
    (apply code elems)))
