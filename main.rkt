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
         "lib/theme.rkt"
         "lib/ol.rkt"
         "lib/util.rkt")


(define CONFERENCE-NAME "ICFP'24")
(current-preamble "\\RequirePackage[libertine]{newtxmath}\n\\usepackage{mathpartir}\n\\usepackage{xcolor}")

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

(define section (make-parameter #f))

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
                          (t "Xu Xue, The University of Hong Kong") (spring 1) (t CONFERENCE-NAME))))
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

  (slide
   #:title "Recap"
   @item{Contextual typing is a lightweight approach to type inference}
   (indent #:by (em 2) @item{that exploits partially known contextual information;})
   @item{It enables several improvements over bidirectional typing}
   (indent #:by (em 8) (ol #:make-num circled
                           @elem{fewer annotations are required, without resorting to backtracking}
                           @elem{annotatability becomes clearer}
                           @elem{more powerful subsumption rule}))
))