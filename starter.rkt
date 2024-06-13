#lang at-exp slideshow/widescreen

(require pict/color)
(require "theme.rkt")
(require "code.rkt")
(require pict/face)

(require "latex-pict/main.rkt")
(current-preamble "\\RequirePackage[libertine]{newtxmath}\n\\usepackage{mathpartir}\n\\usepackage{xcolor}")

(define happyface
  (scale (face 'happy) 0.2))

(define (darkblue p)
  (colorize p (dark "blue")))

(define (authors p1 p2)
  (para #:align 'center
             (darkblue (it p1))
             (it "and")              
             (darkblue (it p2))))

(define (institution txt)
  (it txt))

(define (emph txt)
  (darkblue (t txt)))

(define ($ txt)
  (tex-math #:scale 4 txt))
  
#;(define (tex-math #:scale num txt)
  (t txt))

#;(define (mathpar txt)
  (t txt))

(slide
 (scale (titlet "Contextual Typing") 1.5) 
 (authors "Xu Xue" "Bruno C. d. S. Oliveira") 
 (institution "The University of Hong Kong"))

(slide
 #:title "Type Inference and what we believe ..."
 'next
 (item "Let" @emph{type annotations} "be reasonable and meaningful;")
 'next
 'alts
 (list
  (list
   @subitem{unambitious in complete type inference;}   
   @subitem{the places to put the annotations should be easy to predict;})
  (list @item{Type information propogation is@emph{local};}
        'next
        'alts
        (list         
         (list @subitem{better error report;}
               @subitem{better performance;}
               @subitem{etc.})
         (list
          @item{@emph{Guidelines} are easy to follow;}
          'next
          'alts
          (list           
           (list @subitem{for language designers;}
                 @subitem{and programmers;})
           (list
            @item{@emph{Scalability} is necessary;}
            'next
            @item{@emph{Implementation} can be easily derived.})))))))

(slide
 #:title "Bidirectional Typing"
 @item{Merge type inference and type checking by two modes;}
 'next
 @subitem{Inference mode: @${\Gamma \vdash e \Rightarrow A}}
 'next
 @subitem{Checking mode: @${\Gamma \vdash e \Leftarrow A}}
 'next
 @item{Mode-correct bidirectional type systems can be directly implemented;} 
 'next
 @haskell{
 infer :: Env -> Term -> Type
 check :: Env -> Term -> Type -> Bool
          }
  'next
 @item{Types are propogated to neighbouring expressions;}
 ;'next
 ;@mathpar[#:scale 3]{\inferrule*[right=Ann]{\Gamma \vdash e \Leftarrow A}{\Gamma \vdash (e : A) \Rightarrow A}}
 )

(slide
 #:title "Bidirectional Typing: Problems"
 @item{Trade-off between expressive power and backtracking;}
 @subitem{more expressive, less syntax-directness;}
 @subitem{all-or-nothing inference strategy;}
 @item{Unclear annotatability and rule duplication;}
 @item{Inexpressive subsumption.}
 )

(slide
 #:title "Our Proposal: Contextual Typing"
 @item{Quantitative Type Assignment Systems (QTASs);}
 @subitem{as a specification for programmers;}
 @subitem{tells you where the annotations are needed;}
 @subitem{parametrised with a counter: @${\Gamma \vdash_n e : A}}
 (blank)
 @item{Syntax-directed Algorithmic Type Systems;}
 @subitem{is decidable;}
 @subitem{parametrised with a context: @${\Gamma \vdash \Sigma \Rightarrow e \Rightarrow A}}
 )

(slide
 @titlet{Soundness}
 (blank)
 'next
 @${If~ \Gamma \vdash \square \Rightarrow e \Rightarrow A, then~ \Gamma \vdash_0 e : A.}
 'next
 @${If~ \Gamma \vdash A \Rightarrow e \Rightarrow A, then~ \Gamma \vdash_\infty e : A.}
 'next
 (blank)
 @titlet{Completeness}
 (blank)
 'next
 @${If~ \Gamma \vdash_0 e : A, then~ \Gamma \vdash \square \Rightarrow e \Rightarrow A.}
 'next
 @${If~ \Gamma \vdash_\infty e : A, then~ \Gamma \vdash A \Rightarrow e \Rightarrow A.}
 )

(slide
 #:title "Recap"
 @item{Contextual typing is a lightweight approach to type inference}
 @subitem{that exploits partially known contextual information;}
 @item{It enables several improvements over bidirectional typing}
 )

(slide
 #:title "Code Block"
 (haskell "infer :: Int -> Int -> Int\n
infer n1 n2 = n1 + n2")
 )