#lang slideshow

(require
  slideshow/code
  racket/draw)

;; font
(current-main-font "Concourse T3")
(current-code-font "Fira Code")
(current-font-size 40)
(get-current-code-font-size
 (thunk (round (* (current-font-size) 9/10))))

;; title and margin
(current-titlet
 (λ (s)
  (colorize (text s (current-main-font) 60)
            (current-title-color))))