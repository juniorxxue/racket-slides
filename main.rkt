#lang at-exp slideshow

(require (for-syntax racket/match
                     syntax/parse/experimental/template)
         pict/conditional
         pict/shadow
         ppict/align
         ppict/tag
         racket/draw
         racket/runtime-path
         racket/sandbox
         ;; rsvg
         slideshow/code
         slideshow/text
         ;; latex
         "latex-pict/main.rkt"
         
        
         syntax/parse/define
         threading

         (prefix-in racket: racket/base)
         (prefix-in slideshow: slideshow/base)
         (only-in slideshow [current-font-size current-text-size])

         "lib/color.rkt"
         "lib/pict.rkt"
         "lib/slideshow.rkt"
         "lib/unicode.rkt"
         "lib/util.rkt")


(define CONFERENCE-NAME "ICFP'24")
(current-preamble "\\RequirePackage[libertine]{newtxmath}\n\\usepackage{mathpartir}")

;; ---------------------------------------------------------------------------------------------------

(define section (make-parameter #f))

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

;; ---------------------------------------------------------------------------------------------------

(define-syntax-parser #%top
  [(_ . id:id)
   #:do [(define tag-name
           (match (symbol->string (syntax-e #'id))
             [(regexp #px"^t:(.+)$" (list _ tag-name)) tag-name]
             [_ #f]))]
   #:when tag-name
   (quasisyntax/loc #'id
     (tag* '#,(string->symbol tag-name)))]
  [(_ . id) (quasisyntax/loc this-syntax (racket:#%top . id))])

(define c:highlights (make-parameter '()))
(define c:flavors (make-parameter (hash)))

(define (tag-highlight-color tag)
  (flavor-highlight-color (hash-ref (c:flavors) tag 0)))
(define (tag-text-color tag)
  (flavor-text-color (hash-ref (c:flavors) tag 0)))

(struct tagged (tag values) #:transparent)
(define (tag t . vs)
  (match vs
    [(list (? pict? p))
     (~> (tag-pict p t)
         (when~> (memq t (c:highlights))
           (highlight #:color (tag-highlight-color t))))]
    [_
     (tagged t vs)]))
(define ((tag* t) . vs)
  (apply tag t vs))

;; ---------------------------------------------------------------------------------------------------

(set-margin! 20)
(set-title-h! 80)

(current-slide-assembler (make-scaling-slide-assembler #:background-color background-color))

(current-titlet
 (λ (s) (parameterize ([current-main-font "Concourse C3"])
          (colorize (t (string-downcase s)) text-plain-color))))

;; ---------------------------------------------------------------------------------------------------

(define slides-prompt-tag (make-continuation-prompt-tag 'slides))
(define current-slide-render (make-parameter (thunk (error 'current-slide-render "no slide renderer"))))

(struct slide-info (title body skippable?) #:transparent
  #:guard (struct-guard/c (or/c string? pict? #f) pict? any/c))

(define (call-with-slide-renderer render-thunk body-thunk #:title [title #f])
  (define skipped 0)
  (parameterize ([current-slide-render render-thunk])
    (let loop ([continue-thunk body-thunk])
      (call-with-continuation-prompt
       continue-thunk slides-prompt-tag
       (λ (info continue)
         (cond
           [(and condense? (slide-info-skippable? info))
            (set! skipped (add1 skipped))
            (loop continue)]
           [else
            ; add a bunch of 'nexts to the slide to tell slideshow that some slides were dropped,
            ; which will cause it to display a range for the slide number
            (apply slide (append (make-list skipped 'next) (list (slide-info-body info)))
                   #:title (slide-info-title info)
                   #:name (section)
                   #:layout 'top)
            (set! skipped 0)
            (loop continue)])))))
  (skip-slides skipped))

(define (add-slide! info)
  (call-with-composable-continuation
   (λ (continue)
     (abort-current-continuation slides-prompt-tag
                                 info
                                 (thunk (continue (void)))))
   slides-prompt-tag))

(define (render-slide! #:skippable? skippable?)
  (define-values [title body] ((current-slide-render)))
  (add-slide! (slide-info title body (and skippable? #t))))
(define (next)  (render-slide! #:skippable? #t))
(define (next!) (render-slide! #:skippable? #f))

(define-syntax-parser slides
  [(_ ({~describe "binding pair" [x:id e:expr]} ...)
      {~alt {~seq #:with param:expr param-val:expr}
            {~optional {~seq #:title title-e:expr}}
            {~once {~var render-e (expr/c #'pict? #:name "render expression")}}
            {~optional {~seq #:timeline timeline-body:expr ...+}}
            {~optional {~and #:condense-last {~bind [condense-last? #t]}}}}
      ...
      {~optional {~seq #:where ~! where-body:expr ...}})

   (define stx this-syntax)
   (define-template-metafunction maybe-tl:last!
     (syntax-parser
       [(_ body ...) (if (attribute condense-last?)
                         (syntax/loc stx (let () body ...))
                         (syntax/loc stx (tl:last! body ...)))]))

   (quasisyntax/loc this-syntax
     (let ([x (make-parameter e)] ...)
       (parameterize ([c:highlights (c:highlights)]
                      [param param-val] ...)
         (call-with-slide-renderer
          #,(syntax/loc #'render-e (thunk {~? {~@ where-body ...}} (values {~? title-e #f} render-e)))
          #,(syntax/loc this-syntax (thunk {~? (maybe-tl:last! {~@ timeline-body ...}) (next!)} (void)))))))])

(define (blank-slide)
  (slide #:name (section) #:condense? #t))

;; ---------------------------------------------------------------------------------------------------

(define (tl:last!/proc continue)
  (define prev #f)
  (begin0
    (let loop ([continue continue])
      (call-with-continuation-prompt
       continue slides-prompt-tag
       (λ (info continue)
         (when prev (add-slide! prev))
         (set! prev info)
         (loop continue))))
    (if prev
        (add-slide! (struct-copy slide-info prev [skippable? #f]))
        (error 'tl:last! "timeline did not yield"))))

(define-simple-macro (tl:last! body ...+)
  (tl:last!/proc (thunk body ...)))

(define (tl:sequence param seq)
  (for ([v seq])
    (param v)
    (next)))

(define (tl:flags #:set [val #t] . params)
  (for ([param (in-list params)])
    (param val)
    (next)))

(define (tl:show . params)
  (apply tl:flags #:set show params))

(define (c:highlight+ which)
  (if (list? which)
      (c:highlights (append which (c:highlights)))
      (c:highlights (cons which (c:highlights)))))

(define (tl:highlight+ . whichs)
  (for ([which (in-list whichs)])
    (c:highlight+ which)
    (next)))

;; ---------------------------------------------------------------------------------------------------

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
      [(tagged t elems)
       (tag t (apply code elems))]
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

(define (matht str)
  (text str "Blackboard Modern Math" (current-text-size)))

(define (mathv str)
  (matht (apply string (for/list ([c (in-string str)])
                         (math-char c #:italic? #t)))))

(define (mathit str)
  (text str (cons 'italic "Blackboard Modern Roman") (current-text-size)))

(define (basic-step #:ip ip #:leaf? [leaf? #t] . lines)
  (~> (for/list ([(line i) (in-indexed lines)])
        (define redex? (and leaf? (= i ip)))
        (hc-append
         (pict-when redex?
           (~> (arrow-line #:arrow-size 20 #:line-length 0)
               (colorize text-plain-color)))
         (blank 5 0)
         (if (< i ip)
             (colorize (apply code line) text-tertiary-color)
             (tag (if redex? 'redex 'continuation) (apply haskell line)))))
      (apply vl-append (current-code-line-sep) _)))

(define (ol #:sep [sep (em 4/5)]
            #:spacing [spacing (current-para-spacing)]
            #:stage [stage #f]
            . elems)
  (define num-picts (parameterize ([current-main-font "Concourse T3"])
                      (for/list ([i (in-range (length elems))])
                        (c:plain (t (~a (add1 i)))))))
  (define max-num-width (apply max (map pict-width num-picts)))
  (~>> (for/list ([elem (in-list elems)]
                  [num (in-list num-picts)])
         (htl-append sep (indent #:by (- max-num-width (pict-width num)) num) elem))
       (apply paras #:spacing spacing #:stage stage)))

(define current-shape-border-width (make-parameter 2))

(define (spike size #:border-width [border-width (current-shape-border-width)])
  (define s/2 (/ size 2))
  (define path (new dc-path%))
  (send path move-to 0 0)
  (send path line-to s/2 s/2)
  (send path line-to size 0)
  (dc (λ (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen (make-pen #:width border-width
                                   #:color shape-border-color))
        (send dc set-brush (make-brush #:color shape-bg-color))
        (send dc draw-path path x y)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))
      size
      s/2))

(define (box w h
             #:highlight [highlight-tag #f]
             #:border-width [border-width (current-shape-border-width)])
  (define highlight? (member highlight-tag (c:highlights)))
  (filled-rectangle w h
                    #:color (if highlight? (tag-highlight-color highlight-tag) shape-bg-color)
                    #:border-width border-width
                    #:border-color (if highlight? (tag-text-color highlight-tag) shape-border-color)))

(define current-box-padding (make-parameter 15))
(define (wrap-box p
                  #:padding [padding (current-box-padding)]
                  #:highlight [highlight-tag #f])
  (cc-superimpose (box (+ (pict-width p) (* padding 2))
                       (+ (pict-height p) (* padding 2))
                       #:highlight highlight-tag)
                  p))

(define (balloon w h
                 #:corner-radius [r 25]
                 #:spike-size [spike-s 25]
                 #:spike-position [spike-posn 0]
                 #:spike-side [spike-side 'top]
                 #:color [color shape-bg-color]
                 #:border-color [border-color shape-border-color]
                 #:border-width [bw (current-shape-border-width)]
                 #:highlight [highlight-tag #f])
  (define 2spike-s (* 2 spike-s))
  (define spike-side-type (match spike-side
                            [(or 'top 'bottom) 'horizontal]
                            [(or 'left 'right) 'vertical]))
  (define spike-side-len (match spike-side-type
                           ['horizontal w]
                           ['vertical   h]))
  (define spike-offset (* spike-posn (- spike-side-len 2spike-s)))
  (define posn-after-spike (+ spike-offset 2spike-s))
  (define len-after-spike (- spike-side-len (+ spike-offset 2spike-s)))

  (define tl-r (cond
                 [(eq? spike-side 'left)   (min r spike-offset)]
                 [(eq? spike-side 'top)    (min r spike-offset)]
                 [else                     r]))
  (define tr-r (cond
                 [(eq? spike-side 'right)  (min r spike-offset)]
                 [(eq? spike-side 'top)    (min r len-after-spike)]
                 [else                     r]))
  (define bl-r (cond
                 [(eq? spike-side 'left)   (min r len-after-spike)]
                 [(eq? spike-side 'bottom) (min r spike-offset)]
                 [else                     r]))
  (define br-r (cond
                 [(eq? spike-side 'right)  (min r len-after-spike)]
                 [(eq? spike-side 'bottom) (min r len-after-spike)]
                 [else                     r]))
  
  (define path (new dc-path%))
  ; top left corner
  (send path arc 0 0 tl-r tl-r (turns 1/2) (turns 1/4) #f)
  ; top side
  (when (eq? spike-side 'top)
    (send path line-to spike-offset 0)
    (send path line-to (+ spike-offset spike-s) (- spike-s))
    (send path line-to (+ spike-offset 2spike-s) 0))
  (send path line-to tr-r 0)
  ; top right corner
  (send path arc (- w tr-r) 0 tr-r tr-r (turns 1/4) 0 #f)
  ; right side
  (when (eq? spike-side 'right)
    (send path line-to w spike-offset)
    (send path line-to (+ w spike-s) (+ spike-offset spike-s))
    (send path line-to w (+ spike-offset 2spike-s)))
  (send path line-to w br-r)
  ; bottom right corner
  (send path arc (- w br-r) (- h br-r) br-r br-r 0 (turns -1/4) #f)
  ; bottom side
  (when (eq? spike-side 'bottom)
    (send path line-to posn-after-spike h)
    (send path line-to (- posn-after-spike spike-s) (+ h spike-s))
    (send path line-to (- posn-after-spike 2spike-s) h))
  (send path line-to bl-r h)
  ; bottom left corner
  (send path arc 0 (- h bl-r) bl-r bl-r (turns 3/4) (turns 1/2) #f)
  ; left side
  (when (eq? spike-side 'left)
    (send path line-to 0 posn-after-spike)
    (send path line-to spike-s (- posn-after-spike spike-s))
    (send path line-to 0 (- posn-after-spike 2spike-s)))
  (send path close)

  (define highlight? (member highlight-tag (c:highlights)))
  (define color* (if highlight? (tag-highlight-color highlight-tag) color))
  (define border-color* (if highlight? (tag-text-color highlight-tag) border-color))

  (~> (dc (λ (dc dx dy) (send dc draw-path path dx (+ dy spike-s))) w (+ h spike-s))
      (inset 0 (- spike-s) 0 0)
      (adjust-pen #:color border-color* #:width bw)
      (maybe-colorize color*)))

(define (encircle p
                  #:padding [padding 15]
                  #:highlight? [highlight? #f]
                  #:color [color (if highlight? (current-highlight-color) shape-bg-color)]
                  #:border-color [border-color (if highlight? (current-highlight-border-color) shape-border-color)]
                  #:border-width [border-width 2])
  (~> (disk (+ (max (pict-width p) (pict-height p))
               (* (+ padding (/ border-width 2)) 2))
            #:color color
            #:border-width border-width
            #:border-color border-color)
      (cc-superimpose p)))

(define (p:file p
                #:color [color shape-bg-color]
                #:border-color [border-color shape-border-color])
  (cc-superimpose (~> (file-icon 40 50)
                      (adjust-pen #:color border-color
                                  #:width 1.25
                                  #:cap 'projecting
                                  #:join 'miter)
                      (colorize color))
                  (~> (scale-to-fit p 25 35)
                      (colorize text-secondary-color))))

(define terminal-text-color (->color% (hsv 0 0 0.93)))
(define terminal-bg-color (->color% (hsv 0 0 0.2)))
(define terminal-highlight-color (->color% (hsv 0 0.3 0.4)))
(define (wrap-terminal-frame p #:padding [padding 20])
  (cc-superimpose
   (filled-rounded-rectangle (+ (pict-width p) (* padding 2))
                             (+ (pict-height p) (* padding 2))
                             15
                             #:draw-border? #f
                             #:color terminal-bg-color)
   (colorize p terminal-text-color)))

(define (strikethrough p
                       #:line-width [line-width 2]
                       #:color [color #f])
  (pin-over p 0 (- (* (pict-ascent p) 3/4) (/ line-width 2))
            (filled-rectangle (pict-width p) line-width #:draw-border? #f #:color color)))

(define (cross-out p
                   #:line-width [line-width 2]
                   #:color [color #f])
  (define w (pict-width p))
  (define h (pict-height p))
  (~> p
      (pin-over p cc-find #:hole cc-find
                (adjust-pen #:width line-width #:color color (line w h)))
      (pin-over p cc-find #:hole cc-find
                (adjust-pen #:width line-width #:color color (line w (- h))))))

(define (steps #:append p-append
               #:stage stage
               . ps)
  (~> (for/list ([(p i) (in-indexed (in-list ps))])
        (pict-when (> stage i) p))
      (apply p-append _)))

(define (pin-arrow-tail base-p src-path find-src dest-path find-dest
                        #:start-angle [start-angle #f]
                        #:end-angle [end-angle #f]
                        #:start-pull [start-pull 1/4]
                        #:end-pull [end-pull 1/4]
                        #:color [color text-secondary-color])
  (pin-line base-p
            (find-child base-p src-path) find-src
            (find-child base-p dest-path) find-dest
            #:line-width 2.5
            #:color color
            #:start-angle start-angle
            #:end-angle end-angle
            #:start-pull start-pull
            #:end-pull end-pull))

(define (pin-arrow base-p src-path find-src dest-path find-dest
                   #:start-angle [start-angle #f]
                   #:end-angle [end-angle #f]
                   #:start-pull [start-pull 1/4]
                   #:end-pull [end-pull 1/4]
                   #:color [color text-secondary-color])
  (pin-arrow-line 15 base-p
                  (find-child base-p src-path) find-src
                  (find-child base-p dest-path) find-dest
                  #:line-width 2.5
                  #:color color
                  #:start-angle start-angle
                  #:end-angle end-angle
                  #:start-pull start-pull
                  #:end-pull end-pull))

(define (reduction-arrow)
  (~> (arrow-line #:arrow-size 15
                  #:line-length 25
                  #:line-width 2.5)
      (colorize text-secondary-color)))

(define (reduction-steps
         #:stage [stage +inf.0]
         #:arrow [arrow-p (~> (reduction-arrow)
                              (rotate (turns 3/4)))]
         #:margin [margin 5]
         . ps)
  (match ps
    ['() (blank)]
    [(cons p ps)
     (~> (for/list ([p (in-list ps)])
           (vc-append (inset arrow-p 0 margin) p))
         (apply steps p _
                #:append vc-append
                #:stage stage))]))

(define (e_1)
  (define base-p (mathv "e"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "1") 0.6) -10))
      (refocus base-p)
      (inset 0 0 15 0)))
(define (e_2)
  (define base-p (mathv "e"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "2") 0.6) -10))
      (refocus base-p)
      (inset 0 0 15 0)))

(define (E #:base [base-p (mathv "E")]
           #:tag [t #f]
           . elems)
  (line-append
   (hbl-append
    (~> base-p (when~> t (tag t _)))
    (matht "["))
   (apply elem #:color #f elems)
   (matht "]")))

(define (E_1 #:tag [tag #f] . elems)
  (define base-p (mathv "E"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "1") 0.6) -10))
      (refocus base-p)
      (inset 0 0 10 0)
      (apply E elems #:base _ #:tag tag)))

(define (E_2 #:tag [tag #f] . elems)
  (define base-p (mathv "E"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "2") 0.6) -10))
      (refocus base-p)
      (inset 0 0 10 0)
      (apply E elems #:base _ #:tag tag)))

(define (delay-highlight-proc thunk tag)
  (if (member tag (c:highlights))
      (parameterize ([c:highlights (remove tag (c:highlights))])
        (highlight (thunk)
                   #:path tag
                   #:color (tag-highlight-color tag)))
      (thunk)))

(define-syntax-parse-rule (delay-highlight pict-e:expr tag-e:expr)
  (delay-highlight-proc (λ () pict-e) tag-e))

(define (hole #:color [color text-plain-color])
  (~> (disk 0.8 #:draw-border? #f #:color color)
      (lift-above-baseline -0.1)
      (scale (current-text-size))
      (tag 'hole _)))

;; ---------------------------------------------------------------------------------------------------

(section "Title")

(slides ()
  (~> (vc-append title
                 (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                 #;(~> (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                     (inset 0 -5 0 5))
                 (with-size 30
                   (hflex (+ (pict-width title) 20)
                          (t "Xu Xue, HKUPLG") (spring 1) (t CONFERENCE-NAME))))
      (colorize text-secondary-color))
  #:where
  (define delcont (with-font "Concourse C3"
                    (htl-append @t{Contextual} (blank 30 0) @t{Typing})))
 (define title (~> (with-size 150
                      (with-font "Concourse C2"
                        (t "Contextual Typing")))
                    (colorize text-plain-color))))


(begin
  (section "Background")

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 6)
    #:with current-para-spacing '(lines 0.5)
    #:with current-para-fill? #f
    #:with current-para-width 1200
    #:title "Type inference and what we believe"
    (paras
     #:stage (s:bullet)
     @item{Having reasonable and meaningful annotations is good.}
     @item{Local information is good.}
     @item{Having guidelines for langauge designers and programmers is good.}
     @item{Scalabilities are necessary.}
     @item{Implementation can be easily derived.}
     ))

  (slides ([s:bullet 0])
   #:timeline (tl:sequence s:bullet 3)
   #:with current-para-spacing '(lines 0.5)
   #:with current-para-fill? #f
   #:with current-para-width 1200
   #:title "Bidirectional Typing"
   (paras
    #:stage (s:bullet)
    @item{Merge type inference and type checking by two modes;}
    @item{Types are propogated to neighbouring expressions;}   
    ;@mathpar[#:scale 3]{\inferrule*[right=\texttt{Int}]{\Gamma \vdash e_1 \Rightarrow A \to B \\ \Gamma \vdash e_2 \Leftarrow A}{\Gamma \vdash e_1~e_2 \Rightarrow B} @"\n\n" \inferrule*[right=\texttt{Int}]{\Gamma \vdash e_1 \Rightarrow A \to B}{\Gamma \vdash e_1~e_2 \Rightarrow B} }
    )
))
