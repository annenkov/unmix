#lang racket

(define **unmix-path** "")

;; File extension for target scheme/racket programs.
(define **program-file-ext** ".rkt")

;; Racket-specific module header for target programs.
;; Set to #f in case of non-racket runtime for generated programs.
(define **lang-directive** "#lang racket")

(provide (all-defined-out))
