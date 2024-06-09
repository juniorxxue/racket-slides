#lang scribble/manual
@require[@for-label[latex-pict
                    racket/base]]
@title{latex-pict}
@author{soegaard}

@defmodule[latex-pict]

@verbatim{
;;;
;;; TeX Formulas
;;;

;; This module contains a few functions that can turn a piece of TeX (string)
;; into a pict that renders the formula.

;; The module relies on  pdflatex to render the formula into a pdf file
;; and Poppler which is a C library, that renders pdf formula into Cairo contexts.
;; Cairo being the underlying graphics libray or racket/draw (and picts).
;; Note that introduces a dependecy on racket-popper.

;; Results are cached to speed things up, when rendering the same document
;; multiple times (caching is based on the entire tex document, so changes
;; in options etc. will trigger a rerendering).

;; TeX and LaTeX has three different rendering modes:

;;     text          for the body text
;;     math          for math formulas inline with text        $ or \( \)
;;     displaymath   for larger math formulass, not inline    $$ or \[ \]
;;     


;; Extracting a formula in "math" mode gives a tight bounding box around
;; the formula. The mode "displaymath" on the other hand has large
;; left and right margins. Therefore tex-display-math renders its
;; input as  \( \displaystyle <your-formula> \).
;; In case you really need the real display environment, you can
;; use text-real-display-math, which uses \[ <your-formula> \].

;; The main exports of this library is therefore:

(provide tex-math                ; \(                <formula> \)
         tex-display-math        ; \( \displaystyle  <formula> \)
         tex-real-display-math)  ; \[                <formula> \]

;; You will notice, that there is export that renders plain text,
;; but that is currently not supported by the "preview" package.

;; When a snippet of TeX without any context is rendered, it
;; is first put into a  TeX document, then pdflatex is invoked,
;; size information is extracted from the log file, and then
;; the pdf file is used to produce the pict.

;; It would be great if the user of this library didn't need to know
;; any details, about the TeX document, but TeX can be difficult to
;; work with. Especiallu if one doesn't know which document class
;; and packages that are used.

;; Here is the latex document that pdflatex will render to pdf,
;; when the mode tex-math is used:

;;     \documentclass{standalone}
;;     \usepackage[active,tightpage,lyx,pdftex,<modeoption>]{preview}
;;     \usepackage{amsmath}
;;         <your-preamble>
;;     \begin{document}
;;        \( <your-latex-snippet> \)
;;     \end{document}

;; where <modeoption> is one of "textmath" and "displaymath",
;;       <your-preamble> defaults as "", but you can put \usepackage{a_nice_pkg} here
;;       <your-latex-snippet>  is surrounded

;; First of all the document classes "standalone" and the
;; package "preview" is used. The combination renders the
;; document into a pdf with a minimal margin around the context.

;; The options for preview are:
;;    active    - actually use preview (otherwise preview is ignored)
;;    tightpage - no margins, option is needed to use the pdftex option
;;    pdftex    - assume PDFTeX is the output driver
;;    lyx       - makes pdftex write size information in the log file
;;                (we use it to determine size and placement of baseline)

;; The package "preview" by David Kastrup and the AUCTeX Team is available here:
;;     https://ctan.org/pkg/preview?lang=en

;; The package "amsmath" by the American Mathematical Society contains
;; most math related commands, that you will neeed. The documentation is here:
;;     https://ctan.org/pkg/amsmath?lang=en

;; The document class "standalone" is documented here:
;;     https://ctan.org/pkg/standalone?lang=en


;; The Racket functions tex-math, tex-display-math, and, read-display-math passes
;; the appropriate <modeoption> automatically.

;; The keyword argument #:document-class-options can be used to pass along
;; options (in the form of a list of symbols/strings) to the "standalone"
;; documentclass. Among the interesting options are 10pt, 11pt and 12pt.

;; The option #:preamble allows you to pass along a preamble, where you
;; can declare any packages you need. If you need the same preamble multiple
;; times, you can use the parameter  current-premable.
;; The default preamble is
;;    \usepackage{amsmath}
;; If you set the preamble and still need amsmath, remember to include it
;; in your own preamble.

;; The option #:preview allows you to pass along options in the form
;; of a list of symbols or strings to the "preview" TeX package.
;; If you need the same options multiple times, you can use the
;; parameter  current-preview-options

(provide current-document-class-options current-preamble current-preview-options)

;; Note that it is convenient to se the at-notation from Scribble to call,
;; the text functions:

;;   #lang at-exp racket
;;   (require latex-pict)
;;   @"@"tex-math{ a^2 + b^2 = c^2 }

;; Or perhaps introduce $ and $$ :

;;   #lang at-exp racket
;;   (require latex-pict)
;;   (define $  tex-math)
;;   (define $$ tex-display-math)
;;   @"@"${ c = \sqrt{ a^2 + b^2 = c^2} }

;; You can of course also use the functions without the at-notation:
;;   ($ "c = \\sqrt{ a^2 + b^2 = c^2}")
;; Just remember that \ needs to be quoted as \\ in Racket strings.


;;;
;;; Configuration
;;;

; Preamble, used if the option #:preamble isn't passed.
(define current-preamble  (make-parameter ""))

; Document class options 
(define current-document-class-options (make-parameter '()))
  
; Preview options
(define current-preview-options (make-parameter '()))


; Scale factor applied to the pict before it is rendered.
; The parameter value is used, if the option #:scale isn't used.
(define current-scale-factor (make-parameter 2))

}
