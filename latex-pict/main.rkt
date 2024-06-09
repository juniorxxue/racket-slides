#lang racket/base

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install latex-pict
;; To uninstall:
;;   $ raco pkg remove latex-pict
;; To view documentation:
;;   $ raco docs latex-pict
;;

(require "tex.rkt")
(provide (all-from-out "tex.rkt"))
