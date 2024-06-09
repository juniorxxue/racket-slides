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
         "lib/balloon.rkt"
         "lib/color.rkt"
         "lib/pict.rkt"
         "lib/slideshow.rkt"
         "lib/unicode.rkt"
         "lib/util.rkt")


(define CONFERENCE-NAME "ICFP'24")
(current-preamble "\\RequirePackage[libertine]{newtxmath}\n\\usepackage{mathpartir}\n\\usepackage{xcolor}")

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

;; ---------------------------------------------------------------------------------------------------

(section "Title")

(define title (~> (with-size 150
                    (with-font "Concourse C2"
                      (t "Contextual Typing")))
                  (colorize text-plain-color)))

(define delcont (with-font "Concourse C3"
                  (htl-append @t{Contextual} (blank 30 0) @t{Typing})))

(slide
 (~> (vc-append title
                 (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                 (with-size 30
                   (hflex (+ (pict-width title) 20)
                          (t "Xu Xue, HKUPLG") (spring 1) (t CONFERENCE-NAME))))
      (colorize text-secondary-color)))

(begin
  (section "Background")

  (slide
    #:title "Type inference and what we believe"
    @item{Having reasonable and meaningful annotations is good.}
    @item{Local information is good.}
    @item{Having guidelines for langauge designers and programmers is good.}
    @item{Scalabilities are necessary.}
    @item{Implementation can be easily derived.}
    )

  (slide
   #:title "Bidirectional Typing"
   @item{Merge type inference and type checking by two modes;}
   'next
   (indent #:by (em 2) @item{Inference mode: @tex-math[#:scale 4]{\Gamma \vdash e \Rightarrow A}})
   'next
   (indent #:by (em 2) @item{Checking mode: @tex-math[#:scale 4]{\Gamma \vdash e \Leftarrow A}})
   'next
   @item{Types are propogated to neighbouring expressions;}
   )

  (slide
   #:title "Bidirectional Typing: Problems Statement"
   @item{Trade-off between expressive power and backtracking;}
   (indent #:by (em 2) @item{more expressive, less syntax-directness;})
   (indent #:by (em 2) @item{all-or-nothing inference strategy;})
   @item{Unclear annotatability and rule duplication;}
   @item{Inexpressive subsumption.}
  )

  (define p:call @haskell{exclaim})
  (define p:expr @haskell{@p:call True})

  (define (p:exclaim)
    @haskell{
 exclaim :: Show a => a -> String
 exclaim x = show x ++ "!"})

  (slide
   #:title "Balloon"
   (~> @haskell{@p:call True}
       (pin-balloon p:call cb-find (scale (p:exclaim) 0.75)
                    #:spike-position 0.45
                    #:show? #t)
       (inset 0 0 0 150)))
  
 

  (define p:qtas @tex-math[#:scale 4]{\Gamma \vdash_n e : A})
  (define p:algo @tex-math[#:scale 4]{\Gamma \vdash \Sigma \Rightarrow e \Rightarrow A})
  
  (slide
   #:title "Our Proposal: Contextual Typing"
   'next
   @item{Quantitative Type Assignment Systems (QTASs)}
   'next
   (indent #:by (em 2) (item (~> p:qtas
                                 (pin-balloon p:qtas (adjust-find cb-find -23 0) (scale (t "Counter: quantifies how much information we know from the context") 0.5) #:spike-position 0.45 #:spike-size 10)                   
                                 (inset 0 0 0 80))))
   'next
   @item{Algorithmic Type System}
   'next
   (indent #:by (em 2) (item (~> p:algo
                                 (pin-balloon p:algo (adjust-find cb-find -50 0) (scale (t "Context: precisely captures the information of surrounding context") 0.5) #:spike-position 0.45 #:spike-size 10)                   
                                 (inset 0 0 0 80))))
    )
   )  
